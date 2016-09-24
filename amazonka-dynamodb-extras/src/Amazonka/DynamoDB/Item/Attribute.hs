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
    ( DynamoAttributeName
    , getAttributeName
    ) where

import Amazonka.DynamoDB.Item.Value

import Data.Proxy (Proxy (..))
import Data.Text  (Text)

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Text as Text

type family DynamoAttributeName a :: Symbol

getAttributeName :: forall a. KnownSymbol (DynamoAttributeName a)
                 => Proxy a
                 -> Text
getAttributeName _ =
    Text.pack $ symbolVal (Proxy :: Proxy (DynamoAttributeName a))
