{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Route53.CreateHostedZone
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
-- DNS servers. When trying to create a hosted zone using a reusable
-- delegation set, you could specify an optional DelegationSetId, and Route53
-- would assign those 4 NS records for the zone, instead of alloting a new
-- one.
module Network.AWS.Route53.CreateHostedZone
    (
    -- * Request
      CreateHostedZone
    -- ** Request constructor
    , createHostedZone
    -- ** Request lenses
    , chzCallerReference
    , chzDelegationSetId
    , chzHostedZoneConfig
    , chzName
    , chzVPC

    -- * Response
    , CreateHostedZoneResponse
    -- ** Response constructor
    , createHostedZoneResponse
    -- ** Response lenses
    , chzrChangeInfo
    , chzrDelegationSet
    , chzrHostedZone
    , chzrLocation
    , chzrVPC
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53.Types

data CreateHostedZone = CreateHostedZone
    { _chzCallerReference  :: Text
    , _chzDelegationSetId  :: Maybe Text
    , _chzHostedZoneConfig :: Maybe HostedZoneConfig
    , _chzName             :: Text
    , _chzVPC              :: Maybe VPC
    } deriving (Eq, Show, Generic)

-- | 'CreateHostedZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chzCallerReference' @::@ 'Text'
--
-- * 'chzDelegationSetId' @::@ 'Maybe' 'Text'
--
-- * 'chzHostedZoneConfig' @::@ 'Maybe' 'HostedZoneConfig'
--
-- * 'chzName' @::@ 'Text'
--
-- * 'chzVPC' @::@ 'Maybe' 'VPC'
--
createHostedZone :: Text -- ^ 'chzName'
                 -> Text -- ^ 'chzCallerReference'
                 -> CreateHostedZone
createHostedZone p1 p2 = CreateHostedZone
    { _chzName             = p1
    , _chzCallerReference  = p2
    , _chzVPC              = Nothing
    , _chzHostedZoneConfig = Nothing
    , _chzDelegationSetId  = Nothing
    }

-- | A unique string that identifies the request and that allows failed
-- CreateHostedZone requests to be retried without the risk of executing the
-- operation twice. You must use a unique CallerReference string every time
-- you create a hosted zone. CallerReference can be any unique string; you
-- might choose to use a string that identifies your project, such as
-- DNSMigration_01. Valid characters are any Unicode code points that are
-- legal in an XML 1.0 document. The UTF-8 encoding of the value must be
-- less than 128 bytes.
chzCallerReference :: Lens' CreateHostedZone Text
chzCallerReference =
    lens _chzCallerReference (\s a -> s { _chzCallerReference = a })

-- | The delegation set id of the reusable delgation set whose NS records you
-- want to assign to the new hosted zone.
chzDelegationSetId :: Lens' CreateHostedZone (Maybe Text)
chzDelegationSetId =
    lens _chzDelegationSetId (\s a -> s { _chzDelegationSetId = a })

-- | A complex type that contains an optional comment about your hosted zone.
chzHostedZoneConfig :: Lens' CreateHostedZone (Maybe HostedZoneConfig)
chzHostedZoneConfig =
    lens _chzHostedZoneConfig (\s a -> s { _chzHostedZoneConfig = a })

-- | The name of the domain. This must be a fully-specified domain, for
-- example, www.example.com. The trailing dot is optional; Route 53 assumes
-- that the domain name is fully qualified. This means that Route 53 treats
-- www.example.com (without a trailing dot) and www.example.com. (with a
-- trailing dot) as identical. This is the name you have registered with
-- your DNS registrar. You should ask your registrar to change the
-- authoritative name servers for your domain to the set of NameServers
-- elements returned in DelegationSet.
chzName :: Lens' CreateHostedZone Text
chzName = lens _chzName (\s a -> s { _chzName = a })

-- | The VPC that you want your hosted zone to be associated with. By
-- providing this parameter, your newly created hosted cannot be resolved
-- anywhere other than the given VPC.
chzVPC :: Lens' CreateHostedZone (Maybe VPC)
chzVPC = lens _chzVPC (\s a -> s { _chzVPC = a })

instance ToPath CreateHostedZone where
    toPath = const "/2013-04-01/hostedzone"

instance ToQuery CreateHostedZone where
    toQuery = const mempty

instance ToHeaders CreateHostedZone

instance ToBody CreateHostedZone where
    toBody = toBody . encodeXML . _chzName

data CreateHostedZoneResponse = CreateHostedZoneResponse
    { _chzrChangeInfo    :: ChangeInfo
    , _chzrDelegationSet :: DelegationSet
    , _chzrHostedZone    :: HostedZone
    , _chzrLocation      :: Text
    , _chzrVPC           :: Maybe VPC
    } deriving (Eq, Show, Generic)

-- | 'CreateHostedZoneResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chzrChangeInfo' @::@ 'ChangeInfo'
--
-- * 'chzrDelegationSet' @::@ 'DelegationSet'
--
-- * 'chzrHostedZone' @::@ 'HostedZone'
--
-- * 'chzrLocation' @::@ 'Text'
--
-- * 'chzrVPC' @::@ 'Maybe' 'VPC'
--
createHostedZoneResponse :: HostedZone -- ^ 'chzrHostedZone'
                         -> ChangeInfo -- ^ 'chzrChangeInfo'
                         -> DelegationSet -- ^ 'chzrDelegationSet'
                         -> Text -- ^ 'chzrLocation'
                         -> CreateHostedZoneResponse
createHostedZoneResponse p1 p2 p3 p4 = CreateHostedZoneResponse
    { _chzrHostedZone    = p1
    , _chzrChangeInfo    = p2
    , _chzrDelegationSet = p3
    , _chzrLocation      = p4
    , _chzrVPC           = Nothing
    }

-- | A complex type that contains information about the request to create a
-- hosted zone. This includes an ID that you use when you call the GetChange
-- action to get the current status of the change request.
chzrChangeInfo :: Lens' CreateHostedZoneResponse ChangeInfo
chzrChangeInfo = lens _chzrChangeInfo (\s a -> s { _chzrChangeInfo = a })

-- | A complex type that contains name server information.
chzrDelegationSet :: Lens' CreateHostedZoneResponse DelegationSet
chzrDelegationSet =
    lens _chzrDelegationSet (\s a -> s { _chzrDelegationSet = a })

-- | A complex type that contains identifying information about the hosted
-- zone.
chzrHostedZone :: Lens' CreateHostedZoneResponse HostedZone
chzrHostedZone = lens _chzrHostedZone (\s a -> s { _chzrHostedZone = a })

-- | The unique URL representing the new hosted zone.
chzrLocation :: Lens' CreateHostedZoneResponse Text
chzrLocation = lens _chzrLocation (\s a -> s { _chzrLocation = a })

chzrVPC :: Lens' CreateHostedZoneResponse (Maybe VPC)
chzrVPC = lens _chzrVPC (\s a -> s { _chzrVPC = a })

instance AWSRequest CreateHostedZone where
    type Sv CreateHostedZone = Route53
    type Rs CreateHostedZone = CreateHostedZoneResponse

    request  = post
    response = xmlResponse $ \h x -> CreateHostedZoneResponse
        <$> x %| "ChangeInfo"
        <*> x %| "DelegationSet"
        <*> x %| "HostedZone"
        <*> h ~:| "Location"
        <*> x %| "VPC"
