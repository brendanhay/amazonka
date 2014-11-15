{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : Network.AWS.Data.Internal.Numeric
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Numeric where

import Control.Applicative
import Control.Lens.TH
import Control.Monad
import Data.Aeson.Types
import Network.AWS.Data.Internal.ByteString
import Network.AWS.Data.Internal.Query
import Network.AWS.Data.Internal.Text
import Network.AWS.Data.Internal.XML
import Numeric.Natural

newtype Nat = Nat { unNat :: Natural }
    deriving
        ( Eq
        , Ord
        , Show
        , Enum
        , Num
        , Real
        , Integral
        , Whole
        , ToByteString
        , FromText
        , ToText
        , FromXML
        , ToXML
        , ToQuery
        )

instance FromJSON Nat where
    parseJSON = parseJSON >=> \n ->
        if n < 0
            then fail $ "Cannot convert negative number to Natural: " ++ show n
            else pure $ Nat (fromInteger n)

instance ToJSON Nat where
    toJSON = String . toText . unNat

makePrisms ''Nat
