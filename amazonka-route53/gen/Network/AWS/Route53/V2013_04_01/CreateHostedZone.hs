{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.CreateHostedZone
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action creates a new hosted zone. To create a new hosted zone, send a
-- POST request to the 2013-04-01/hostedzone resource. The request body must
-- include an XML document with a CreateHostedZoneRequest element. The
-- response returns the CreateHostedZoneResponse element that contains
-- metadata about the hosted zone. Route 53 automatically creates a default
-- SOA record and four NS records for the zone. The NS records in the hosted
-- zone are the name servers you give your registrar to delegate your domain
-- to. For more information about SOA and NS records, see NS and SOA Records
-- that Route 53 Creates for a Hosted Zone in the Amazon Route 53 Developer
-- Guide. When you create a zone, its initial status is PENDING. This means
-- that it is not yet available on all DNS servers. The status of the zone
-- changes to INSYNC when the NS and SOA records are available on all Route 53
-- DNS servers.
module Network.AWS.Route53.V2013_04_01.CreateHostedZone
    (
    -- * Request
      CreateHostedZone
    -- ** Request constructor
    , mkCreateHostedZone
    -- ** Request lenses
    , chzName
    , chzCallerReference
    , chzHostedZoneConfig

    -- * Response
    , CreateHostedZoneResponse
    -- ** Response constructor
    , mkCreateHostedZoneResponse
    -- ** Response lenses
    , chzrHostedZone
    , chzrChangeInfo
    , chzrDelegationSet
    , chzrLocation
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type that contains information about the request to create a
-- hosted zone.
data CreateHostedZone = CreateHostedZone
    { _chzName :: Text
    , _chzCallerReference :: Text
    , _chzHostedZoneConfig :: Maybe HostedZoneConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateHostedZone' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @CallerReference ::@ @Text@
--
-- * @HostedZoneConfig ::@ @Maybe HostedZoneConfig@
--
mkCreateHostedZone :: Text -- ^ 'chzName'
                   -> Text -- ^ 'chzCallerReference'
                   -> CreateHostedZone
mkCreateHostedZone p1 p2 = CreateHostedZone
    { _chzName = p1
    , _chzCallerReference = p2
    , _chzHostedZoneConfig = Nothing
    }

-- | The name of the domain. This must be a fully-specified domain, for example,
-- www.example.com. The trailing dot is optional; Route 53 assumes that the
-- domain name is fully qualified. This means that Route 53 treats
-- www.example.com (without a trailing dot) and www.example.com. (with a
-- trailing dot) as identical. This is the name you have registered with your
-- DNS registrar. You should ask your registrar to change the authoritative
-- name servers for your domain to the set of NameServers elements returned in
-- DelegationSet.
chzName :: Lens' CreateHostedZone Text
chzName = lens _chzName (\s a -> s { _chzName = a })

-- | A unique string that identifies the request and that allows failed
-- CreateHostedZone requests to be retried without the risk of executing the
-- operation twice. You must use a unique CallerReference string every time
-- you create a hosted zone. CallerReference can be any unique string; you
-- might choose to use a string that identifies your project, such as
-- DNSMigration_01. Valid characters are any Unicode code points that are
-- legal in an XML 1.0 document. The UTF-8 encoding of the value must be less
-- than 128 bytes.
chzCallerReference :: Lens' CreateHostedZone Text
chzCallerReference =
    lens _chzCallerReference (\s a -> s { _chzCallerReference = a })

-- | A complex type that contains an optional comment about your hosted zone.
chzHostedZoneConfig :: Lens' CreateHostedZone (Maybe HostedZoneConfig)
chzHostedZoneConfig =
    lens _chzHostedZoneConfig (\s a -> s { _chzHostedZoneConfig = a })

instance ToPath CreateHostedZone

instance ToQuery CreateHostedZone

instance ToHeaders CreateHostedZone

instance ToXML CreateHostedZone where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateHostedZoneRequest"

-- | A complex type containing the response information for the new hosted zone.
data CreateHostedZoneResponse = CreateHostedZoneResponse
    { _chzrHostedZone :: HostedZone
    , _chzrChangeInfo :: ChangeInfo
    , _chzrDelegationSet :: DelegationSet
    , _chzrLocation :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateHostedZoneResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HostedZone ::@ @HostedZone@
--
-- * @ChangeInfo ::@ @ChangeInfo@
--
-- * @DelegationSet ::@ @DelegationSet@
--
-- * @Location ::@ @Text@
--
mkCreateHostedZoneResponse :: HostedZone -- ^ 'chzrHostedZone'
                           -> ChangeInfo -- ^ 'chzrChangeInfo'
                           -> DelegationSet -- ^ 'chzrDelegationSet'
                           -> Text -- ^ 'chzrLocation'
                           -> CreateHostedZoneResponse
mkCreateHostedZoneResponse p1 p2 p3 p4 = CreateHostedZoneResponse
    { _chzrHostedZone = p1
    , _chzrChangeInfo = p2
    , _chzrDelegationSet = p3
    , _chzrLocation = p4
    }

-- | A complex type that contains identifying information about the hosted zone.
chzrHostedZone :: Lens' CreateHostedZoneResponse HostedZone
chzrHostedZone = lens _chzrHostedZone (\s a -> s { _chzrHostedZone = a })

-- | A complex type that contains information about the request to create a
-- hosted zone. This includes an ID that you use when you call the GetChange
-- action to get the current status of the change request.
chzrChangeInfo :: Lens' CreateHostedZoneResponse ChangeInfo
chzrChangeInfo = lens _chzrChangeInfo (\s a -> s { _chzrChangeInfo = a })

-- | A complex type that contains name server information.
chzrDelegationSet :: Lens' CreateHostedZoneResponse DelegationSet
chzrDelegationSet =
    lens _chzrDelegationSet (\s a -> s { _chzrDelegationSet = a })

-- | The unique URL representing the new hosted zone.
chzrLocation :: Lens' CreateHostedZoneResponse Text
chzrLocation = lens _chzrLocation (\s a -> s { _chzrLocation = a })

instance AWSRequest CreateHostedZone where
    type Sv CreateHostedZone = Route53
    type Rs CreateHostedZone = CreateHostedZoneResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CreateHostedZoneResponse
            <*> xml %| "HostedZone"
            <*> xml %| "ChangeInfo"
            <*> xml %| "DelegationSet"
            <*> hs ~: "Location"
