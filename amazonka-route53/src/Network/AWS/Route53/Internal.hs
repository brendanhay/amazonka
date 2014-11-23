{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.Internal where

import GHC.Generics
import Network.AWS.Data
import Network.AWS.Prelude

data RecordType
    = A     -- ^ A
    | AAAAA -- ^ AAAA
    | CNAME -- ^ CNAME
    | MX    -- ^ MX
    | NS    -- ^ NS
    | PTR   -- ^ PTR
    | SOA   -- ^ SOA
    | SPF   -- ^ SPF
    | SRV   -- ^ SRV
    | TXT   -- ^ TXT
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RecordType

instance FromText RecordType where
    parser = match "A"     A
         <|> match "AAAA"  AAAA
         <|> match "CNAME" CNAME
         <|> match "MX"    MX
         <|> match "NS"    NS
         <|> match "PTR"   PTR
         <|> match "SOA"   SOA
         <|> match "SPF"   SPF
         <|> match "SRV"   SRV
         <|> match "TXT"   TXT
    {-# INLINE parser #-}

instance ToText RecordType where
    toText = \case
        A     -> "A"
        AAAAA  -> "AAAA"
        CNAME -> "CNAME"
        MX    -> "MX"
        NS    -> "NS"
        PTR   -> "PTR"
        SOA   -> "SOA"
        SPF   -> "SPF"
        SRV   -> "SRV"
        TXT   -> "TXT"
    {-# INLINE toText #-}

instance ToByteString RecordType
instance ToHeader     RecordType

instance FromXML RecordType where
    parseXML = parseXMLText "RecordType"
    {-# INLINE parseXML #-}

instance ToXML RecordType where
    toXML = toXMLText "RecordType"
    {-# INLINE toXML #-}
