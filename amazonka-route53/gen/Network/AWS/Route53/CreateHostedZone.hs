{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action creates a new hosted zone.
--
-- To create a new hosted zone, send a 'POST' request to the
-- '2013-04-01\/hostedzone' resource. The request body must include an XML
-- document with a 'CreateHostedZoneRequest' element. The response returns
-- the 'CreateHostedZoneResponse' element that contains metadata about the
-- hosted zone.
--
-- Route 53 automatically creates a default SOA record and four NS records
-- for the zone. The NS records in the hosted zone are the name servers you
-- give your registrar to delegate your domain to. For more information
-- about SOA and NS records, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/SOA-NSrecords.html NS and SOA Records that Route 53 Creates for a Hosted Zone>
-- in the /Amazon Route 53 Developer Guide/.
--
-- When you create a zone, its initial status is 'PENDING'. This means that
-- it is not yet available on all DNS servers. The status of the zone
-- changes to 'INSYNC' when the NS and SOA records are available on all
-- Route 53 DNS servers.
--
-- When trying to create a hosted zone using a reusable delegation set, you
-- could specify an optional DelegationSetId, and Route53 would assign
-- those 4 NS records for the zone, instead of alloting a new one.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html AWS API Reference> for CreateHostedZone.
module Network.AWS.Route53.CreateHostedZone
    (
    -- * Creating a Request
      createHostedZone
    , CreateHostedZone
    -- * Request Lenses
    , chzDelegationSetId
    , chzVPC
    , chzHostedZoneConfig
    , chzName
    , chzCallerReference

    -- * Destructuring the Response
    , createHostedZoneResponse
    , CreateHostedZoneResponse
    -- * Response Lenses
    , chzrsVPC
    , chzrsStatus
    , chzrsHostedZone
    , chzrsChangeInfo
    , chzrsDelegationSet
    , chzrsLocation
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to create a
-- hosted zone.
--
-- /See:/ 'createHostedZone' smart constructor.
data CreateHostedZone = CreateHostedZone'
    { _chzDelegationSetId  :: !(Maybe Text)
    , _chzVPC              :: !(Maybe VPC)
    , _chzHostedZoneConfig :: !(Maybe HostedZoneConfig)
    , _chzName             :: !Text
    , _chzCallerReference  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chzDelegationSetId'
--
-- * 'chzVPC'
--
-- * 'chzHostedZoneConfig'
--
-- * 'chzName'
--
-- * 'chzCallerReference'
createHostedZone
    :: Text -- ^ 'chzName'
    -> Text -- ^ 'chzCallerReference'
    -> CreateHostedZone
createHostedZone pName_ pCallerReference_ =
    CreateHostedZone'
    { _chzDelegationSetId = Nothing
    , _chzVPC = Nothing
    , _chzHostedZoneConfig = Nothing
    , _chzName = pName_
    , _chzCallerReference = pCallerReference_
    }

-- | The delegation set id of the reusable delgation set whose NS records you
-- want to assign to the new hosted zone.
chzDelegationSetId :: Lens' CreateHostedZone (Maybe Text)
chzDelegationSetId = lens _chzDelegationSetId (\ s a -> s{_chzDelegationSetId = a});

-- | The VPC that you want your hosted zone to be associated with. By
-- providing this parameter, your newly created hosted cannot be resolved
-- anywhere other than the given VPC.
chzVPC :: Lens' CreateHostedZone (Maybe VPC)
chzVPC = lens _chzVPC (\ s a -> s{_chzVPC = a});

-- | A complex type that contains an optional comment about your hosted zone.
chzHostedZoneConfig :: Lens' CreateHostedZone (Maybe HostedZoneConfig)
chzHostedZoneConfig = lens _chzHostedZoneConfig (\ s a -> s{_chzHostedZoneConfig = a});

-- | The name of the domain. This must be a fully-specified domain, for
-- example, www.example.com. The trailing dot is optional; Route 53 assumes
-- that the domain name is fully qualified. This means that Route 53 treats
-- www.example.com (without a trailing dot) and www.example.com. (with a
-- trailing dot) as identical.
--
-- This is the name you have registered with your DNS registrar. You should
-- ask your registrar to change the authoritative name servers for your
-- domain to the set of 'NameServers' elements returned in 'DelegationSet'.
chzName :: Lens' CreateHostedZone Text
chzName = lens _chzName (\ s a -> s{_chzName = a});

-- | A unique string that identifies the request and that allows failed
-- 'CreateHostedZone' requests to be retried without the risk of executing
-- the operation twice. You must use a unique 'CallerReference' string
-- every time you create a hosted zone. 'CallerReference' can be any unique
-- string; you might choose to use a string that identifies your project,
-- such as 'DNSMigration_01'.
--
-- Valid characters are any Unicode code points that are legal in an XML
-- 1.0 document. The UTF-8 encoding of the value must be less than 128
-- bytes.
chzCallerReference :: Lens' CreateHostedZone Text
chzCallerReference = lens _chzCallerReference (\ s a -> s{_chzCallerReference = a});

instance AWSRequest CreateHostedZone where
        type Rs CreateHostedZone = CreateHostedZoneResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 CreateHostedZoneResponse' <$>
                   (x .@? "VPC") <*> (pure (fromEnum s)) <*>
                     (x .@ "HostedZone")
                     <*> (x .@ "ChangeInfo")
                     <*> (x .@ "DelegationSet")
                     <*> (h .# "Location"))

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
               "VPC" @= _chzVPC,
               "HostedZoneConfig" @= _chzHostedZoneConfig,
               "Name" @= _chzName,
               "CallerReference" @= _chzCallerReference]

-- | A complex type containing the response information for the new hosted
-- zone.
--
-- /See:/ 'createHostedZoneResponse' smart constructor.
data CreateHostedZoneResponse = CreateHostedZoneResponse'
    { _chzrsVPC           :: !(Maybe VPC)
    , _chzrsStatus        :: !Int
    , _chzrsHostedZone    :: !HostedZone
    , _chzrsChangeInfo    :: !ChangeInfo
    , _chzrsDelegationSet :: !DelegationSet
    , _chzrsLocation      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chzrsVPC'
--
-- * 'chzrsStatus'
--
-- * 'chzrsHostedZone'
--
-- * 'chzrsChangeInfo'
--
-- * 'chzrsDelegationSet'
--
-- * 'chzrsLocation'
createHostedZoneResponse
    :: Int -- ^ 'chzrsStatus'
    -> HostedZone -- ^ 'chzrsHostedZone'
    -> ChangeInfo -- ^ 'chzrsChangeInfo'
    -> DelegationSet -- ^ 'chzrsDelegationSet'
    -> Text -- ^ 'chzrsLocation'
    -> CreateHostedZoneResponse
createHostedZoneResponse pStatus_ pHostedZone_ pChangeInfo_ pDelegationSet_ pLocation_ =
    CreateHostedZoneResponse'
    { _chzrsVPC = Nothing
    , _chzrsStatus = pStatus_
    , _chzrsHostedZone = pHostedZone_
    , _chzrsChangeInfo = pChangeInfo_
    , _chzrsDelegationSet = pDelegationSet_
    , _chzrsLocation = pLocation_
    }

-- | Undocumented member.
chzrsVPC :: Lens' CreateHostedZoneResponse (Maybe VPC)
chzrsVPC = lens _chzrsVPC (\ s a -> s{_chzrsVPC = a});

-- | The response status code.
chzrsStatus :: Lens' CreateHostedZoneResponse Int
chzrsStatus = lens _chzrsStatus (\ s a -> s{_chzrsStatus = a});

-- | A complex type that contains identifying information about the hosted
-- zone.
chzrsHostedZone :: Lens' CreateHostedZoneResponse HostedZone
chzrsHostedZone = lens _chzrsHostedZone (\ s a -> s{_chzrsHostedZone = a});

-- | A complex type that contains information about the request to create a
-- hosted zone. This includes an ID that you use when you call the
-- GetChange action to get the current status of the change request.
chzrsChangeInfo :: Lens' CreateHostedZoneResponse ChangeInfo
chzrsChangeInfo = lens _chzrsChangeInfo (\ s a -> s{_chzrsChangeInfo = a});

-- | A complex type that contains name server information.
chzrsDelegationSet :: Lens' CreateHostedZoneResponse DelegationSet
chzrsDelegationSet = lens _chzrsDelegationSet (\ s a -> s{_chzrsDelegationSet = a});

-- | The unique URL representing the new hosted zone.
chzrsLocation :: Lens' CreateHostedZoneResponse Text
chzrsLocation = lens _chzrsLocation (\ s a -> s{_chzrsLocation = a});
