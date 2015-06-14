{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.CreateHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This action creates a new hosted zone.
--
-- To create a new hosted zone, send a @POST@ request to the
-- @2013-04-01\/hostedzone@ resource. The request body must include an XML
-- document with a @CreateHostedZoneRequest@ element. The response returns
-- the @CreateHostedZoneResponse@ element that contains metadata about the
-- hosted zone.
--
-- Route 53 automatically creates a default SOA record and four NS records
-- for the zone. The NS records in the hosted zone are the name servers you
-- give your registrar to delegate your domain to. For more information
-- about SOA and NS records, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/SOA-NSrecords.html NS and SOA Records that Route 53 Creates for a Hosted Zone>
-- in the /Amazon Route 53 Developer Guide/.
--
-- When you create a zone, its initial status is @PENDING@. This means that
-- it is not yet available on all DNS servers. The status of the zone
-- changes to @INSYNC@ when the NS and SOA records are available on all
-- Route 53 DNS servers.
--
-- When trying to create a hosted zone using a reusable delegation set, you
-- could specify an optional DelegationSetId, and Route53 would assign
-- those 4 NS records for the zone, instead of alloting a new one.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html>
module Network.AWS.Route53.CreateHostedZone
    (
    -- * Request
      CreateHostedZone
    -- ** Request constructor
    , createHostedZone
    -- ** Request lenses
    , chzDelegationSetId
    , chzHostedZoneConfig
    , chzVPC
    , chzName
    , chzCallerReference

    -- * Response
    , CreateHostedZoneResponse
    -- ** Response constructor
    , createHostedZoneResponse
    -- ** Response lenses
    , chzrVPC
    , chzrHostedZone
    , chzrChangeInfo
    , chzrDelegationSet
    , chzrLocation
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53.Types

-- | /See:/ 'createHostedZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chzDelegationSetId'
--
-- * 'chzHostedZoneConfig'
--
-- * 'chzVPC'
--
-- * 'chzName'
--
-- * 'chzCallerReference'
data CreateHostedZone = CreateHostedZone'{_chzDelegationSetId :: Maybe Text, _chzHostedZoneConfig :: Maybe HostedZoneConfig, _chzVPC :: Maybe VPC, _chzName :: Text, _chzCallerReference :: Text} deriving (Eq, Read, Show)

-- | 'CreateHostedZone' smart constructor.
createHostedZone :: Text -> Text -> CreateHostedZone
createHostedZone pName pCallerReference = CreateHostedZone'{_chzDelegationSetId = Nothing, _chzHostedZoneConfig = Nothing, _chzVPC = Nothing, _chzName = pName, _chzCallerReference = pCallerReference};

-- | The delegation set id of the reusable delgation set whose NS records you
-- want to assign to the new hosted zone.
chzDelegationSetId :: Lens' CreateHostedZone (Maybe Text)
chzDelegationSetId = lens _chzDelegationSetId (\ s a -> s{_chzDelegationSetId = a});

-- | A complex type that contains an optional comment about your hosted zone.
chzHostedZoneConfig :: Lens' CreateHostedZone (Maybe HostedZoneConfig)
chzHostedZoneConfig = lens _chzHostedZoneConfig (\ s a -> s{_chzHostedZoneConfig = a});

-- | The VPC that you want your hosted zone to be associated with. By
-- providing this parameter, your newly created hosted cannot be resolved
-- anywhere other than the given VPC.
chzVPC :: Lens' CreateHostedZone (Maybe VPC)
chzVPC = lens _chzVPC (\ s a -> s{_chzVPC = a});

-- | The name of the domain. This must be a fully-specified domain, for
-- example, www.example.com. The trailing dot is optional; Route 53 assumes
-- that the domain name is fully qualified. This means that Route 53 treats
-- www.example.com (without a trailing dot) and www.example.com. (with a
-- trailing dot) as identical.
--
-- This is the name you have registered with your DNS registrar. You should
-- ask your registrar to change the authoritative name servers for your
-- domain to the set of @NameServers@ elements returned in @DelegationSet@.
chzName :: Lens' CreateHostedZone Text
chzName = lens _chzName (\ s a -> s{_chzName = a});

-- | A unique string that identifies the request and that allows failed
-- @CreateHostedZone@ requests to be retried without the risk of executing
-- the operation twice. You must use a unique @CallerReference@ string
-- every time you create a hosted zone. @CallerReference@ can be any unique
-- string; you might choose to use a string that identifies your project,
-- such as @DNSMigration_01@.
--
-- Valid characters are any Unicode code points that are legal in an XML
-- 1.0 document. The UTF-8 encoding of the value must be less than 128
-- bytes.
chzCallerReference :: Lens' CreateHostedZone Text
chzCallerReference = lens _chzCallerReference (\ s a -> s{_chzCallerReference = a});

instance AWSRequest CreateHostedZone where
        type Sv CreateHostedZone = Route53
        type Rs CreateHostedZone = CreateHostedZoneResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateHostedZoneResponse' <$>
                   x .@? "VPC" <*> x .@ "HostedZone" <*>
                     x .@ "ChangeInfo"
                     <*> x .@ "DelegationSet"
                     <*> h .# "Location")

instance ToElement CreateHostedZone where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateHostedZoneRequest"

instance ToHeaders CreateHostedZone where
        toHeaders = const mempty

instance ToPath CreateHostedZone where
        toPath = const "/2013-04-01/hostedzone"

instance ToQuery CreateHostedZone where
        toQuery = const mempty

instance ToXML CreateHostedZone where
        toXML CreateHostedZone'{..}
          = mconcat
              ["DelegationSetId" @= _chzDelegationSetId,
               "HostedZoneConfig" @= _chzHostedZoneConfig,
               "VPC" @= _chzVPC, "Name" @= _chzName,
               "CallerReference" @= _chzCallerReference]

-- | /See:/ 'createHostedZoneResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chzrVPC'
--
-- * 'chzrHostedZone'
--
-- * 'chzrChangeInfo'
--
-- * 'chzrDelegationSet'
--
-- * 'chzrLocation'
data CreateHostedZoneResponse = CreateHostedZoneResponse'{_chzrVPC :: Maybe VPC, _chzrHostedZone :: HostedZone, _chzrChangeInfo :: ChangeInfo, _chzrDelegationSet :: DelegationSet, _chzrLocation :: Text} deriving (Eq, Read, Show)

-- | 'CreateHostedZoneResponse' smart constructor.
createHostedZoneResponse :: HostedZone -> ChangeInfo -> DelegationSet -> Text -> CreateHostedZoneResponse
createHostedZoneResponse pHostedZone pChangeInfo pDelegationSet pLocation = CreateHostedZoneResponse'{_chzrVPC = Nothing, _chzrHostedZone = pHostedZone, _chzrChangeInfo = pChangeInfo, _chzrDelegationSet = pDelegationSet, _chzrLocation = pLocation};

-- | FIXME: Undocumented member.
chzrVPC :: Lens' CreateHostedZoneResponse (Maybe VPC)
chzrVPC = lens _chzrVPC (\ s a -> s{_chzrVPC = a});

-- | A complex type that contains identifying information about the hosted
-- zone.
chzrHostedZone :: Lens' CreateHostedZoneResponse HostedZone
chzrHostedZone = lens _chzrHostedZone (\ s a -> s{_chzrHostedZone = a});

-- | A complex type that contains information about the request to create a
-- hosted zone. This includes an ID that you use when you call the
-- GetChange action to get the current status of the change request.
chzrChangeInfo :: Lens' CreateHostedZoneResponse ChangeInfo
chzrChangeInfo = lens _chzrChangeInfo (\ s a -> s{_chzrChangeInfo = a});

-- | A complex type that contains name server information.
chzrDelegationSet :: Lens' CreateHostedZoneResponse DelegationSet
chzrDelegationSet = lens _chzrDelegationSet (\ s a -> s{_chzrDelegationSet = a});

-- | The unique URL representing the new hosted zone.
chzrLocation :: Lens' CreateHostedZoneResponse Text
chzrLocation = lens _chzrLocation (\ s a -> s{_chzrLocation = a});
