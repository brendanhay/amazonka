{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53
    (
    -- * Hosted Zones
      CreateHostedZone         (..)
    , GetHostedZone            (..)
    , ListHostedZones          (..)
    , DeleteHostedZone         (..)

    -- * Resource Record Sets
    , RecordAction             (..)
    , RecordType               (..)
    , ResourceRecordSet        (..)
    , ChangeResourceRecordSets (..)
    , ListResourceRecordSets   (..)
    , GetChange                (..)

    -- * Health Checks
    , CreateHealthCheck        (..)
    , GetHealthCheck           (..)
    , ListHealthChecks         (..)
    , DeleteHealthCheck        (..)

    -- * Request Signing
    , r53Sign
    , r53Post
    ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Hastache
import           Data.Aeson.TH
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Monoid
import           Data.String
import qualified Data.Text                  as Text
import           Network.AWS.Internal
import           Network.Http.Client
import qualified System.IO.Streams          as Streams
import           Text.Hastache

--
-- Signing
--

r53Sign :: Method -> ByteString -> [(ByteString, ByteString)] -> AWS SignedRequest
r53Sign meth path qry = version3 $
    (emptyRequest meth r53Version r53Endpoint path Nothing)
        { rqQuery = qry
        }

r53Post :: Template a =>  ByteString -> a -> AWS SignedRequest
r53Post path tmpl = render >>= version3
    . emptyRequest POST r53Version r53Endpoint path
    . Just
  where
    render = liftIO $ do
        bstr <- hastacheStr defaultConfig (readTemplate tmpl) (jsonContext tmpl)
        LBS.putStrLn bstr
        Streams.fromLazyByteString bstr

r53Version :: ApiVersion
r53Version = "2012-12-12"

r53Endpoint :: ByteString
r53Endpoint = "route53.amazonaws.com"

--
-- Hosted Zones
--

-- | Creates a new hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html>
data CreateHostedZone = CreateHostedZone
    { chzCallerRef  :: !CallerRef
    , chzDomainName :: !ByteString
    , chzComment    :: !(Maybe String)
    } deriving (Show)

$(deriveTmpl (underscore . dropPrefix "chz") ''CreateHostedZone)

instance GlobalRequest CreateHostedZone where
    signGlobal = r53Post "hostedzone"

-- | Gets information about a specified hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHostedZone.html>
newtype GetHostedZone = GetHostedZone ByteString
    deriving (Show, IsString, IsByteString)

instance GlobalRequest GetHostedZone where
    signGlobal chk = r53Sign GET ("hostedzone/" <> toBS chk) []

-- | Gets a list of the hosted zones that are associated with the
-- current AWS account.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZones.html>
data ListHostedZones = ListHostedZones
    { lhzMarker   :: !(Maybe ByteString)
    , lhzMaxItems :: !(Maybe Integer)
    } deriving (Show)

$(deriveQS' (lowercase . dropPrefix "lhz") ''ListHostedZones)

instance GlobalRequest ListHostedZones where
    signGlobal = r53Sign GET "hostedzone" . queryString

-- | Deletes a hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteHostedZone.html>
newtype DeleteHostedZone = DeleteHostedZone ByteString
    deriving (Show, IsString, IsByteString)

instance GlobalRequest DeleteHostedZone where
    signGlobal chk = r53Sign DELETE ("hostedzone/" <> toBS chk) []

--
-- Resource Records
--

data RecordAction = CreateAction | DeleteAction

instance Show RecordAction where
    show CreateAction = "CREATE"
    show DeleteAction = "DELETE"

instance ToJSON RecordAction where
    toJSON = String . Text.pack . show

data RecordType = A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT
    deriving (Show)

instance QueryParam RecordType where
    queryParam k v = [(k, BS.pack $ show v)]

instance ToJSON RecordType where
    toJSON = String . Text.pack . show

data ResourceRecordSet = ResourceRecordSet
    { rrsAction        :: !RecordAction
    , rrsName          :: !ByteString
    , rrsType          :: !RecordType
    , rrsTTL           :: !Integer
    , rrsHealthCheckId :: !(Maybe ByteString)
    , rrsValues        :: ![ByteString]
    } deriving (Show)

$(deriveToJSON (underscore . dropPrefix "rrs") ''ResourceRecordSet)

-- | Adds, deletes, and changes resource record sets in a Route 53 hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html>
data ChangeResourceRecordSets = ChangeResourceRecordSets
    { crrsComment :: !(Maybe ByteString)
    , crrsChanges :: ![ResourceRecordSet]
    } deriving (Show)

$(deriveTmpl (underscore . dropPrefix "crrs") ''ChangeResourceRecordSets)

instance GlobalRequest ChangeResourceRecordSets where
    signGlobal = r53Post ""

-- | Lists details about all of the resource record sets in a hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListResourceRecordSets.html>
data ListResourceRecordSets = ListResourceRecordSets
    { lrrsZoneId     :: !ByteString
    , lrrsName       :: !(Maybe ByteString)
    , lrrsType       :: !(Maybe RecordType)
    , lrrsIdentifier :: !(Maybe ByteString)
    , lrrsMaxItems   :: !(Maybe Integer)
    } deriving (Show)

$(deriveQS' (lowercase . dropPrefix "lrrs") ''ListResourceRecordSets)

instance GlobalRequest ListResourceRecordSets where
    signGlobal rs@ListResourceRecordSets{..} =
        r53Sign GET ("hostedzone/" <> lrrsZoneId <> "/rrset") $ queryString rs

-- | Returns the current status of a change batch request that you
-- submitted by using ChangeResourceRecordSets.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html>
newtype GetChange = GetChange ByteString
    deriving (Show, IsString, IsByteString)

instance GlobalRequest GetChange where
    signGlobal chk = r53Sign GET ("change/" <> toBS chk) []

--
-- HealthChecks
--

-- | Creates a new health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHealthCheck.html>
data CreateHealthCheck = CreateHealthCheck
    { chcCallerRef :: !CallerRef
    , chcIpAddress :: !ByteString
    , chcPort      :: !Int
    , chcProtocol  :: !Protocol
    , chcResource  :: !ByteString
    , chcFQDN      :: !ByteString
    } deriving (Show)

$(deriveTmpl (underscore . dropPrefix "chc") ''CreateHealthCheck)

instance GlobalRequest CreateHealthCheck where
    signGlobal = r53Post "healthcheck"

-- | Gets information about a specified health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheck.html>
newtype GetHealthCheck = GetHealthCheck ByteString
    deriving (Show, IsString, IsByteString)

instance GlobalRequest GetHealthCheck where
    signGlobal chk = r53Sign GET ("healthcheck/" <> toBS chk) []

-- | Gets a list of the health checks that are associated
-- with the current AWS account.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHealthChecks.html>
data ListHealthChecks = ListHealthChecks
    { lhcMarker   :: !(Maybe ByteString)
    , lhcMaxItems :: !(Maybe Integer)
    } deriving (Show)

$(deriveQS' (lowercase . dropPrefix "lhc") ''ListHealthChecks)

instance GlobalRequest ListHealthChecks where
    signGlobal = r53Sign GET "healthcheck" . queryString

-- | Deletes a health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteHealthCheck.html>
newtype DeleteHealthCheck = DeleteHealthCheck ByteString
    deriving (Show, IsString, IsByteString)

instance GlobalRequest DeleteHealthCheck where
    signGlobal chk = r53Sign DELETE (mappend "healthcheck/" $ toBS chk) []
