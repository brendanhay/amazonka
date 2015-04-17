{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.Model.Paginator
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Model.Paginator where

import           Control.Applicative
import           Control.Lens

import           Data.Text           (Text)

data Key
    = NoKey
    | Key    Text
    | Index  Text Key
    | Apply  Text Key
    | Choice Key  Key
      deriving (Eq, Show)

keyNames :: Traversal' Key Text
keyNames f = \case
    NoKey      -> pure NoKey
    Key    n   -> Key    <$> f n
    Index  n k -> Index  <$> f n <*> keyNames f k
    Apply  n k -> Apply  <$> f n <*> keyNames f k
    Choice a b -> Choice <$> keyNames f a <*> keyNames f b

data Token a = Token
    { _tokInputAnn  :: a
    , _tokOutputAnn :: a
    , _tokInput     :: Key
    , _tokOutput    :: Key
    } deriving (Eq, Show)

makeLenses ''Token

tokenKeys :: Traversal' (Token a) Key
tokenKeys f (Token ar br a b) = Token ar br <$> f a <*> f b

data Pager a
    = More Key [Token a]
    | Next Key (Token a)
      deriving (Eq, Show)

pagerKeys :: Traversal' (Pager a) Key
pagerKeys f = \case
    More k ts -> More <$> f k <*> traverse (tokenKeys f) ts
    Next k t  -> Next <$> f k <*> tokenKeys f t
