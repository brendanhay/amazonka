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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new public hosted zone, used to specify how the Domain Name System (DNS) routes traffic on the Internet for a domain, such as example.com, and its subdomains.
--
--
-- /Important:/ Public hosted zones can't be converted to a private hosted zone or vice versa. Instead, create a new hosted zone with the same name and create new resource record sets.
--
-- Send a @POST@ request to the @/2013-04-01/hostedzone@ resource. The request body must include a document with a @CreateHostedZoneRequest@ element. The response returns the @CreateHostedZoneResponse@ element containing metadata about the hosted zone.
--
-- Fore more information about charges for hosted zones, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
-- Note the following:
--
--     * You can't create a hosted zone for a top-level domain (TLD).
--
--     * Amazon Route 53 automatically creates a default SOA record and four NS records for the zone. For more information about SOA and NS records, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/SOA-NSrecords.html NS and SOA Records that Amazon Route 53 Creates for a Hosted Zone> in the /Amazon Route 53 Developer Guide/ .
--
--     * If your domain is registered with a registrar other than Amazon Route 53, you must update the name servers with your registrar to make Amazon Route 53 your DNS service. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/creating-migrating.html Configuring Amazon Route 53 as your DNS Service> in the /Amazon Route 53 Developer's Guide/ .
--
--
--
-- After creating a zone, its initial status is @PENDING@ . This means that it is not yet available on all DNS servers. The status of the zone changes to @INSYNC@ when the NS and SOA records are available on all Amazon Route 53 DNS servers.
--
-- When trying to create a hosted zone using a reusable delegation set, specify an optional DelegationSetId, and Amazon Route 53 would assign those 4 NS records for the zone, instead of allotting a new one.
--
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
    , chzrsResponseStatus
    , chzrsHostedZone
    , chzrsChangeInfo
    , chzrsDelegationSet
    , chzrsLocation
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type containing the hosted zone request information.
--
--
--
-- /See:/ 'createHostedZone' smart constructor.
data CreateHostedZone = CreateHostedZone'
    { _chzDelegationSetId  :: !(Maybe ResourceId)
    , _chzVPC              :: !(Maybe VPC)
    , _chzHostedZoneConfig :: !(Maybe HostedZoneConfig)
    , _chzName             :: !Text
    , _chzCallerReference  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chzDelegationSetId' - If you want to associate a reusable delegation set with this hosted zone, the ID that Amazon Route 53 assigned to the reusable delegation set when you created it. For more information about reusable delegation sets, see 'CreateReusableDelegationSet' .     * Type    * String     * Default    * None     * Parent    * @CreatedHostedZoneRequest@
--
-- * 'chzVPC' - The VPC that you want your hosted zone to be associated with. By providing this parameter, your newly created hosted can't be resolved anywhere other than the given VPC.
--
-- * 'chzHostedZoneConfig' - (Optional) A complex type that contains an optional comment about your hosted zone. If you don't want to specify a comment, omit both the @HostedZoneConfig@ and @Comment@ elements.
--
-- * 'chzName' - The name of the domain. For resource record types that include a domain name, specify a fully qualified domain name, for example, /www.example.com/ . The trailing dot is optional; Amazon Route 53 assumes that the domain name is fully qualified. This means that Amazon Route 53 treats /www.example.com/ (without a trailing dot) and /www.example.com./ (with a trailing dot) as identical. If you're creating a public hosted zone, this is the name you have registered with your DNS registrar. If your domain name is registered with a registrar other than Amazon Route 53, change the name servers for your domain to the set of @NameServers@ that @CreateHostedZone@ returns in the DelegationSet element.
--
-- * 'chzCallerReference' - A unique string that identifies the request and that allows failed @CreateHostedZone@ requests to be retried without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you create a hosted zone. @CallerReference@ can be any unique string, for example, a date/time stamp.
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

-- | If you want to associate a reusable delegation set with this hosted zone, the ID that Amazon Route 53 assigned to the reusable delegation set when you created it. For more information about reusable delegation sets, see 'CreateReusableDelegationSet' .     * Type    * String     * Default    * None     * Parent    * @CreatedHostedZoneRequest@
chzDelegationSetId :: Lens' CreateHostedZone (Maybe ResourceId)
chzDelegationSetId = lens _chzDelegationSetId (\ s a -> s{_chzDelegationSetId = a});

-- | The VPC that you want your hosted zone to be associated with. By providing this parameter, your newly created hosted can't be resolved anywhere other than the given VPC.
chzVPC :: Lens' CreateHostedZone (Maybe VPC)
chzVPC = lens _chzVPC (\ s a -> s{_chzVPC = a});

-- | (Optional) A complex type that contains an optional comment about your hosted zone. If you don't want to specify a comment, omit both the @HostedZoneConfig@ and @Comment@ elements.
chzHostedZoneConfig :: Lens' CreateHostedZone (Maybe HostedZoneConfig)
chzHostedZoneConfig = lens _chzHostedZoneConfig (\ s a -> s{_chzHostedZoneConfig = a});

-- | The name of the domain. For resource record types that include a domain name, specify a fully qualified domain name, for example, /www.example.com/ . The trailing dot is optional; Amazon Route 53 assumes that the domain name is fully qualified. This means that Amazon Route 53 treats /www.example.com/ (without a trailing dot) and /www.example.com./ (with a trailing dot) as identical. If you're creating a public hosted zone, this is the name you have registered with your DNS registrar. If your domain name is registered with a registrar other than Amazon Route 53, change the name servers for your domain to the set of @NameServers@ that @CreateHostedZone@ returns in the DelegationSet element.
chzName :: Lens' CreateHostedZone Text
chzName = lens _chzName (\ s a -> s{_chzName = a});

-- | A unique string that identifies the request and that allows failed @CreateHostedZone@ requests to be retried without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you create a hosted zone. @CallerReference@ can be any unique string, for example, a date/time stamp.
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

instance Hashable CreateHostedZone

instance NFData CreateHostedZone

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

-- | A complex type containing the response information for the hosted zone.
--
--
--
-- /See:/ 'createHostedZoneResponse' smart constructor.
data CreateHostedZoneResponse = CreateHostedZoneResponse'
    { _chzrsVPC            :: !(Maybe VPC)
    , _chzrsResponseStatus :: !Int
    , _chzrsHostedZone     :: !HostedZone
    , _chzrsChangeInfo     :: !ChangeInfo
    , _chzrsDelegationSet  :: !DelegationSet
    , _chzrsLocation       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chzrsVPC' - A complex type that contains information about an Amazon VPC that you associated with this hosted zone.
--
-- * 'chzrsResponseStatus' - -- | The response status code.
--
-- * 'chzrsHostedZone' - A complex type that contains general information about the hosted zone.
--
-- * 'chzrsChangeInfo' - A complex type that describes the changes made to your hosted zone.
--
-- * 'chzrsDelegationSet' - A complex type that describes the name servers for this hosted zone.
--
-- * 'chzrsLocation' - The unique URL representing the new hosted zone.
createHostedZoneResponse
    :: Int -- ^ 'chzrsResponseStatus'
    -> HostedZone -- ^ 'chzrsHostedZone'
    -> ChangeInfo -- ^ 'chzrsChangeInfo'
    -> DelegationSet -- ^ 'chzrsDelegationSet'
    -> Text -- ^ 'chzrsLocation'
    -> CreateHostedZoneResponse
createHostedZoneResponse pResponseStatus_ pHostedZone_ pChangeInfo_ pDelegationSet_ pLocation_ =
    CreateHostedZoneResponse'
    { _chzrsVPC = Nothing
    , _chzrsResponseStatus = pResponseStatus_
    , _chzrsHostedZone = pHostedZone_
    , _chzrsChangeInfo = pChangeInfo_
    , _chzrsDelegationSet = pDelegationSet_
    , _chzrsLocation = pLocation_
    }

-- | A complex type that contains information about an Amazon VPC that you associated with this hosted zone.
chzrsVPC :: Lens' CreateHostedZoneResponse (Maybe VPC)
chzrsVPC = lens _chzrsVPC (\ s a -> s{_chzrsVPC = a});

-- | -- | The response status code.
chzrsResponseStatus :: Lens' CreateHostedZoneResponse Int
chzrsResponseStatus = lens _chzrsResponseStatus (\ s a -> s{_chzrsResponseStatus = a});

-- | A complex type that contains general information about the hosted zone.
chzrsHostedZone :: Lens' CreateHostedZoneResponse HostedZone
chzrsHostedZone = lens _chzrsHostedZone (\ s a -> s{_chzrsHostedZone = a});

-- | A complex type that describes the changes made to your hosted zone.
chzrsChangeInfo :: Lens' CreateHostedZoneResponse ChangeInfo
chzrsChangeInfo = lens _chzrsChangeInfo (\ s a -> s{_chzrsChangeInfo = a});

-- | A complex type that describes the name servers for this hosted zone.
chzrsDelegationSet :: Lens' CreateHostedZoneResponse DelegationSet
chzrsDelegationSet = lens _chzrsDelegationSet (\ s a -> s{_chzrsDelegationSet = a});

-- | The unique URL representing the new hosted zone.
chzrsLocation :: Lens' CreateHostedZoneResponse Text
chzrsLocation = lens _chzrsLocation (\ s a -> s{_chzrsLocation = a});

instance NFData CreateHostedZoneResponse
