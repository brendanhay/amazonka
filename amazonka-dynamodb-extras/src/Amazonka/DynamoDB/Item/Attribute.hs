{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      : Amazonka.DynamoDB.Item.Attribute
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Amazonka.DynamoDB.Item.Attribute
    ( KnownDynamoName
    , DynamoName
    , getName
    ) where

import Amazonka.DynamoDB.Item.Value

import Data.Proxy (Proxy (..))
import Data.Text  (Text)

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Text as Text

type KnownDynamoName a = KnownSymbol (DynamoName a)

type family DynamoName a :: Symbol

getName :: forall a. KnownDynamoName a => Proxy a -> Text
getName _ = Text.pack $ symbolVal (Proxy :: Proxy (DynamoName a))
