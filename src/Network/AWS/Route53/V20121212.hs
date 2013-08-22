{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- |
-- Module      : Network.AWS.Route53.V20121212
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V20121212
    (
    -- * Version
      route53Version

    -- * Hosted Zones
    , CreateHostedZone         (..)
    , GetHostedZone            (..)
    , ListHostedZones          (..)
    , DeleteHostedZone         (..)

    -- * Record Sets
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

    -- * Response Types
    , module Types
    ) where

import           Control.Applicative                 ((<$>))
import           Control.Monad.IO.Class
import           Data.ByteString                     (ByteString)
import           Data.Monoid
import           Data.String
import           Network.AWS.Internal
import           Network.AWS.Route53.V20121212.Types as Types
import           Network.Http.Client                 (Method(..))
import qualified System.IO.Streams                   as Streams
import           Text.XML.Expat.Pickle.Generic

data R53

instance AWSService R53 where
    service _ = Service "route53" (toBS route53Version) "route53.amazonaws.com"
        SigningVersion3 <$> currentRegion

route53Version :: ByteString
route53Version = "2012-12-12"

req :: IsQuery a => Method -> ByteString -> a -> AWS (RawRequest R53 b)
req meth path qry = return $ (emptyRequest meth FormEncoded path Nothing)
    { rqQuery = toQuery qry
    }

body :: IsXML a => Method -> ByteString -> a -> AWS (RawRequest R53 b)
body meth path val =
    emptyRequest meth Xml (route53Version <> "/" <> path) . Just <$> contents
  where
    contents = liftIO . Streams.fromByteString $ toXML val

--
-- Hosted Zones
--

-- | Creates a new hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_CreateHostedZone.html>
data CreateHostedZone = CreateHostedZone
    { chzCallerReference :: !CallerReference
    , chzDomainName      :: !ByteString
    , chzComment         :: !(Maybe ByteString)
    } deriving (Eq, Show, Generic)

instance IsXML CreateHostedZone

instance AWSRequest R53 CreateHostedZone CreateHostedZoneResponse where
    request = body POST "hostedzone"

-- | Gets information about a specified hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_GetHostedZone.html>
newtype GetHostedZone = GetHostedZone ByteString
    deriving (Show, IsString, IsByteString)

instance AWSRequest R53 GetHostedZone GetHostedZoneResponse where
    request chk = req GET ("hostedzone/" <> toBS chk) ()

-- | Gets a list of the hosted zones that are associated with the
-- current AWS account.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_ListHostedZones.html>
data ListHostedZones = ListHostedZones
    { lhzMarker   :: !(Maybe ByteString)
    , lhzMaxItems :: !(Maybe Integer)
    } deriving (Eq, Show, Generic)

instance IsQuery ListHostedZones where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListHostedZones ListHostedZonesResponse where
    request = req GET "hostedzone"

-- | Deletes a hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_DeleteHostedZone.html>
newtype DeleteHostedZone = DeleteHostedZone ByteString
    deriving (Show, IsString, IsByteString)

instance AWSRequest R53 DeleteHostedZone DeleteHostedZoneResponse where
    request chk = req DELETE ("hostedzone/" <> toBS chk) ()

--
-- Record Sets
--

-- | Adds, deletes, and changes resource record sets in a Route 53 hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_ChangeResourceRecordSets.html>
data ChangeResourceRecordSets = ChangeResourceRecordSets
    { crrsZoneId  :: !ByteString
    , crrsComment :: !(Maybe ByteString)
    , crrsChanges :: ![ResourceRecordSet]
    } deriving (Eq, Show, Generic)

instance IsXML ChangeResourceRecordSets

instance AWSRequest R53 ChangeResourceRecordSets ChangeResourceRecordSetsResponse where
    request rs@ChangeResourceRecordSets{..} =
        body POST ("hostedzone/" <> crrsZoneId <> "/rrset") rs

-- | Lists details about all of the resource record sets in a hosted zone.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_ListResourceRecordSets.html>
data ListResourceRecordSets = ListResourceRecordSets
    { lrrsZoneId     :: !ByteString
    , lrrsName       :: !(Maybe ByteString)
    , lrrsType       :: !(Maybe RecordType)
    , lrrsIdentifier :: !(Maybe ByteString)
    , lrrsMaxItems   :: !(Maybe Integer)
    } deriving (Eq, Show, Generic)

instance IsQuery ListResourceRecordSets where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListResourceRecordSets ListResourceRecordSetsResponse where
    request rs@ListResourceRecordSets{..} =
        req GET ("hostedzone/" <> lrrsZoneId <> "/rrset") rs

-- | Returns the current status of a change batch request that you
-- submitted by using ChangeResourceRecordSets.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_GetChange.html>
newtype GetChange = GetChange ByteString
    deriving (Show, IsString, IsByteString)

instance AWSRequest R53 GetChange GetChangeResponse where
    request chk = req GET ("change/" <> toBS chk) ()

--
-- Health Checks
--

-- | Creates a new health check.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_CreateHealthCheck.html>
data CreateHealthCheck = CreateHealthCheck
    { chcCallerReference   :: !CallerReference
    , chcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Eq, Show, Generic)

instance IsXML CreateHealthCheck

instance AWSRequest R53 CreateHealthCheck CreateHealthCheckResponse where
    request = body POST "healthcheck"

-- | Gets information about a specified health check.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_GetHealthCheck.html>
newtype GetHealthCheck = GetHealthCheck ByteString
    deriving (Show, IsString, IsByteString)

instance AWSRequest R53 GetHealthCheck GetHealthCheckResponse where
    request chk = req GET ("healthcheck/" <> toBS chk) ()

-- | Gets a list of the health checks that are associated
-- with the current AWS account.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_ListHealthChecks.html>
data ListHealthChecks = ListHealthChecks
    { lhcMarker   :: !(Maybe ByteString)
    , lhcMaxItems :: !(Maybe Integer)
    } deriving (Eq, Show, Generic)

instance IsQuery ListHealthChecks where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListHealthChecks ListHealthChecksResponse where
    request = req GET "healthcheck"

-- | Deletes a health check.
--
-- <http://docs.aws.amazon.com/R53/latest/APIReference/API_DeleteHealthCheck.html>
newtype DeleteHealthCheck = DeleteHealthCheck ByteString
    deriving (Show, IsString, IsByteString)

instance AWSRequest R53 DeleteHealthCheck DeleteHealthCheckResponse where
    request chk = req DELETE ("healthcheck/" <> toBS chk) ()
