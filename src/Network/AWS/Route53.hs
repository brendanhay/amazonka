{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# LANGUAGE FlexibleInstances       #-}

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
    ) where

import           Control.Applicative
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
import           Network.Http.Client        hiding (get)
import           System.IO.Streams          (InputStream)
import qualified System.IO.Streams          as Streams
import           Text.Hastache

--
-- R53 Requests
--

data R53

instance AWSSigner R53 where
    sign = version3

instance AWSRegion R53 where
    regionalise _ = id

get, delete :: FromJSON a => ByteString -> [(ByteString, ByteString)] -> AWS (RawRequest R53 a)
get    = req GET
delete = req DELETE

post :: (Template a, FromJSON b) => ByteString -> a -> AWS (RawRequest R53 b)
post path tmpl = emptyRequest POST version endpoint path . Just <$> render tmpl

req :: FromJSON a => Method -> ByteString -> [(ByteString, ByteString)] -> AWS (RawRequest R53 a)
req meth path qry = return $ rq { rqQuery = qry }
  where
    rq = emptyRequest meth version endpoint path Nothing

version :: ApiVersion
version = "2012-12-12"

endpoint :: ByteString
endpoint = "route53.amazonaws.com"

render :: (MonadIO m, Template a) => a -> m (InputStream ByteString)
render tmpl = liftIO $ do
    bstr <- hastacheStr defaultConfig (readTemplate tmpl) (jsonContext tmpl)
    LBS.putStrLn bstr
    Streams.fromLazyByteString bstr

--
-- Hosted Zones
--

-- | Creates a new hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_CreateHostedZone.html>
data CreateHostedZone = CreateHostedZone
    { chzCallerRef  :: !CallerRef
    , chzDomainName :: !ByteString
    , chzComment    :: !(Maybe String)
    } deriving (Show)

$(deriveTmpl ''CreateHostedZone)

instance AWSRequest R53 CreateHostedZone Object where
    request = post "hostedzone"

-- | Gets information about a specified hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_GetHostedZone.html>
newtype GetHostedZone = GetHostedZone ByteString
    deriving (Show, IsString, ToByteString)

instance AWSRequest R53 GetHostedZone Object where
    request chk = get ("hostedzone/" <> toBS chk) []

-- | Gets a list of the hosted zones that are associated with the
-- current AWS account.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_ListHostedZones.html>
data ListHostedZones = ListHostedZones
    { lhzMarker   :: !(Maybe ByteString)
    , lhzMaxItems :: !(Maybe Integer)
    } deriving (Show)

$(deriveQS' (lowerAll . dropLower) ''ListHostedZones)

instance AWSRequest R53 ListHostedZones Object where
    request = get "hostedzone" . queryString

-- | Deletes a hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_DeleteHostedZone.html>
newtype DeleteHostedZone = DeleteHostedZone ByteString
    deriving (Show, IsString, ToByteString)

instance AWSRequest R53 DeleteHostedZone Object where
    request chk = delete ("hostedzone/" <> toBS chk) []

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

$(deriveToJSON (underscore . dropLower) ''ResourceRecordSet)

-- | Adds, deletes, and changes resource record sets in a Route 53 hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_ChangeResourceRecordSets.html>
data ChangeResourceRecordSets = ChangeResourceRecordSets
    { crrsZoneId  :: !ByteString
    , crrsComment :: !(Maybe ByteString)
    , crrsChanges :: ![ResourceRecordSet]
    } deriving (Show)

$(deriveTmpl ''ChangeResourceRecordSets)

instance AWSRequest R53 ChangeResourceRecordSets Object where
    request rs@ChangeResourceRecordSets{..} =
        post ("hostedzone/" <> crrsZoneId <> "/rrset") rs

-- | Lists details about all of the resource record sets in a hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_ListResourceRecordSets.html>
data ListResourceRecordSets = ListResourceRecordSets
    { lrrsZoneId     :: !ByteString
    , lrrsName       :: !(Maybe ByteString)
    , lrrsType       :: !(Maybe RecordType)
    , lrrsIdentifier :: !(Maybe ByteString)
    , lrrsMaxItems   :: !(Maybe Integer)
    } deriving (Show)

$(deriveQS' (lowerAll . dropLower) ''ListResourceRecordSets)

instance AWSRequest R53 ListResourceRecordSets Object where
    request rs@ListResourceRecordSets{..} =
        get ("hostedzone/" <> lrrsZoneId <> "/rrset") $ queryString rs

-- | Returns the current status of a change batch request that you
-- submitted by using ChangeResourceRecordSets.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_GetChange.html>
newtype GetChange = GetChange ByteString
    deriving (Show, IsString, ToByteString)

instance AWSRequest R53 GetChange Object where
    request chk = get ("change/" <> toBS chk) []

--
-- HealthChecks
--

-- | Creates a new health check.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_CreateHealthCheck.html>
data CreateHealthCheck = CreateHealthCheck
    { chcCallerRef :: !CallerRef
    , chcIpAddress :: !ByteString
    , chcPort      :: !Int
    , chcProtocol  :: !Protocol
    , chcResource  :: !ByteString
    , chcFQDN      :: !ByteString
    } deriving (Show)

$(deriveTmpl ''CreateHealthCheck)

instance AWSRequest R53 CreateHealthCheck Object where
    request = post "healthcheck"

-- | Gets information about a specified health check.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_GetHealthCheck.html>
newtype GetHealthCheck = GetHealthCheck ByteString
    deriving (Show, IsString, ToByteString)

instance AWSRequest R53 GetHealthCheck Object where
    request chk = get ("healthcheck/" <> toBS chk) []

-- | Gets a list of the health checks that are associated
-- with the current AWS account.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_ListHealthChecks.html>
data ListHealthChecks = ListHealthChecks
    { lhcMarker   :: !(Maybe ByteString)
    , lhcMaxItems :: !(Maybe Integer)
    } deriving (Show)

$(deriveQS' (lowerAll . dropLower) ''ListHealthChecks)

instance AWSRequest R53 ListHealthChecks Object where
    request = get "healthcheck" . queryString

-- | Deletes a health check.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_DeleteHealthCheck.html>
newtype DeleteHealthCheck = DeleteHealthCheck ByteString
    deriving (Show, IsString, ToByteString)

instance AWSRequest R53 DeleteHealthCheck Object where
    request chk = delete (mappend "healthcheck/" $ toBS chk) []
