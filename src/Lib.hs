module Lib (
  Holder
, newHolder
, get
, insert
, delete
, modify
) where

import qualified Data.Map.Strict as M
import Data.Traversable
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

newtype Holder k a = Holder { holder :: TVar (M.Map k a) }

newHolder :: STM (Holder k v)
newHolder = fmap Holder . newTVar $ M.empty

get :: Ord k => k -> Holder k a -> STM (Maybe a)
get k (Holder t) = M.lookup k <$> readTVar t

insert :: Ord k => k -> a -> Holder k a -> STM ()
insert k a (Holder t) = modifyTVar' t $ M.insert k a

delete :: Ord k => k -> Holder k a -> STM ()
delete k (Holder t) = modifyTVar' t (M.delete k)

modify :: Ord k => (a -> a) -> k -> Holder k a -> STM ()
modify f k (Holder t) = modifyTVar' t (M.adjust f k)

dump :: Holder k a -> STM (M.Map k a)
dump = readTVar . holder
