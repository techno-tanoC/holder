module Lib where

import qualified Data.Map as M
import Data.Traversable
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

newtype Holder k a = Holder { holder :: TVar (M.Map k a) }

empty :: IO (Holder k a)
empty = Holder <$> newTVarIO M.empty

find :: Ord k => k -> Holder k a -> IO (Maybe a)
find k (Holder h) = M.lookup k <$> readTVarIO h

insert :: Ord k => k -> a -> Holder k a -> IO ()
insert k a (Holder h) = atomically . modifyTVar h $ M.insertWith (flip const) k a

update :: Ord k => k -> (a -> a) -> Holder k a -> IO ()
update k f (Holder h) = atomically . modifyTVar h $ M.adjust f k

elems :: Holder k a -> IO [a]
elems (Holder h) = atomically $ M.elems <$> readTVar h

assocs :: Holder k a -> IO [(k, a)]
assocs (Holder h) = atomically $ M.assocs <$> readTVar h
