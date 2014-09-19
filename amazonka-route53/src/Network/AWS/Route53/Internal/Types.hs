{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.Internal.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.Internal.Types where

import GHC.Generics
import Network.AWS.Prelude

newtype ResourceId = ResourceId Text
    deriving (Eq, Show, Generic, IsString)

instance FromText     ResourceId where parser = ResourceId <$> takeText
instance ToText       ResourceId where toText (ResourceId t) = t
instance ToByteString ResourceId
instance FromXML      ResourceId
instance ToXML        ResourceId
instance ToQuery      ResourceId where toQuery = toQuery . toBS

data RecordType = A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT
    deriving (Eq, Read, Show, Ord, Generic)

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

instance ToText RecordType where
    toText t = case t of
         A     -> "A"
         AAAA  -> "AAAA"
         CNAME -> "CNAME"
         MX    -> "MX"
         NS    -> "NS"
         PTR   -> "PTR"
         SOA   -> "SOA"
         SPF   -> "SPF"
         SRV   -> "SRV"
         TXT   -> "TXT"

instance ToByteString RecordType
instance FromXML      RecordType
instance ToXML        RecordType

instance ToQuery RecordType where
    toQuery = toQuery . toBS

