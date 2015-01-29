{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : Network.AWS.Data.Internal.Numeric
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Numeric where

import Control.Lens.TH
import Control.Monad
import Data.Aeson.Types
import Data.Monoid
import Data.Scientific
import Network.AWS.Data.Internal.ByteString
import Network.AWS.Data.Internal.Query
import Network.AWS.Data.Internal.Text
import Network.AWS.Data.Internal.XML
import Numeric.Natural

newtype Nat = Nat { unNat :: Natural }
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Enum
        , Num
        , Real
        , Integral
        , ToByteString
        , FromText
        , ToText
        , FromXML
        , ToXML
        , ToQuery
        )

instance FromJSON Nat where
    parseJSON = parseJSON >=> go
      where
        go n = case floatingOrInteger n of
            Left  (_ :: Double) -> fail (floatErr n)
            Right i
                | n < 0         -> fail (negateErr n)
                | otherwise     -> return . Nat $ fromInteger i

        floatErr  = mappend "Cannot convert float to Natural: " . show
        negateErr = mappend "Cannot convert negative number to Natural: " . show

instance ToJSON Nat where
    toJSON = Number . flip scientific 0 . toInteger . unNat

makePrisms ''Nat
