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
-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupEgress
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [EC2-VPC only] Adds one or more egress rules to a security group for use with a VPC. Specifically, this action permits instances to send traffic to one or more destination IPv4 or IPv6 CIDR address ranges, or to one or more destination security groups for the same VPC. This action doesn't apply to security groups for use in EC2-Classic. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ . For more information about security group limits, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html Amazon VPC Limits> .
--
--
-- Each rule consists of the protocol (for example, TCP), plus either a CIDR range or a source group. For the TCP and UDP protocols, you must also specify the destination port or port range. For the ICMP protocol, you must also specify the ICMP type and code. You can use -1 for the type or code to mean all types or all codes.
--
-- Rule changes are propagated to affected instances as quickly as possible. However, a small delay might occur.
--
module Network.AWS.EC2.AuthorizeSecurityGroupEgress
    (
    -- * Creating a Request
      authorizeSecurityGroupEgress
    , AuthorizeSecurityGroupEgress
    -- * Request Lenses
    , asgeFromPort
    , asgeIPPermissions
    , asgeIPProtocol
    , asgeToPort
    , asgeCidrIP
    , asgeSourceSecurityGroupOwnerId
    , asgeSourceSecurityGroupName
    , asgeDryRun
    , asgeGroupId

    -- * Destructuring the Response
    , authorizeSecurityGroupEgressResponse
    , AuthorizeSecurityGroupEgressResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AuthorizeSecurityGroupEgress.
--
--
--
-- /See:/ 'authorizeSecurityGroupEgress' smart constructor.
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
  { _asgeFromPort                   :: {-# NOUNPACK #-}!(Maybe Int)
  , _asgeIPPermissions              :: {-# NOUNPACK #-}!(Maybe [IPPermission])
  , _asgeIPProtocol                 :: {-# NOUNPACK #-}!(Maybe Text)
  , _asgeToPort                     :: {-# NOUNPACK #-}!(Maybe Int)
  , _asgeCidrIP                     :: {-# NOUNPACK #-}!(Maybe Text)
  , _asgeSourceSecurityGroupOwnerId :: {-# NOUNPACK #-}!(Maybe Text)
  , _asgeSourceSecurityGroupName    :: {-# NOUNPACK #-}!(Maybe Text)
  , _asgeDryRun                     :: {-# NOUNPACK #-}!(Maybe Bool)
  , _asgeGroupId                    :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeSecurityGroupEgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgeFromPort' - The start of port range for the TCP and UDP protocols, or an ICMP type number. We recommend that you specify the port range in a set of IP permissions instead.
--
-- * 'asgeIPPermissions' - A set of IP permissions. You can't specify a destination security group and a CIDR IP address range.
--
-- * 'asgeIPProtocol' - The IP protocol name or number. We recommend that you specify the protocol in a set of IP permissions instead.
--
-- * 'asgeToPort' - The end of port range for the TCP and UDP protocols, or an ICMP type number. We recommend that you specify the port range in a set of IP permissions instead.
--
-- * 'asgeCidrIP' - The CIDR IPv4 address range. We recommend that you specify the CIDR range in a set of IP permissions instead.
--
-- * 'asgeSourceSecurityGroupOwnerId' - The AWS account number for a destination security group. To authorize outbound access to a destination security group, we recommend that you use a set of IP permissions instead.
--
-- * 'asgeSourceSecurityGroupName' - The name of a destination security group. To authorize outbound access to a destination security group, we recommend that you use a set of IP permissions instead.
--
-- * 'asgeDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'asgeGroupId' - The ID of the security group.
authorizeSecurityGroupEgress
    :: Text -- ^ 'asgeGroupId'
    -> AuthorizeSecurityGroupEgress
authorizeSecurityGroupEgress pGroupId_ =
  AuthorizeSecurityGroupEgress'
  { _asgeFromPort = Nothing
  , _asgeIPPermissions = Nothing
  , _asgeIPProtocol = Nothing
  , _asgeToPort = Nothing
  , _asgeCidrIP = Nothing
  , _asgeSourceSecurityGroupOwnerId = Nothing
  , _asgeSourceSecurityGroupName = Nothing
  , _asgeDryRun = Nothing
  , _asgeGroupId = pGroupId_
  }


-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. We recommend that you specify the port range in a set of IP permissions instead.
asgeFromPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgeFromPort = lens _asgeFromPort (\ s a -> s{_asgeFromPort = a});

-- | A set of IP permissions. You can't specify a destination security group and a CIDR IP address range.
asgeIPPermissions :: Lens' AuthorizeSecurityGroupEgress [IPPermission]
asgeIPPermissions = lens _asgeIPPermissions (\ s a -> s{_asgeIPPermissions = a}) . _Default . _Coerce;

-- | The IP protocol name or number. We recommend that you specify the protocol in a set of IP permissions instead.
asgeIPProtocol :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeIPProtocol = lens _asgeIPProtocol (\ s a -> s{_asgeIPProtocol = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP type number. We recommend that you specify the port range in a set of IP permissions instead.
asgeToPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgeToPort = lens _asgeToPort (\ s a -> s{_asgeToPort = a});

-- | The CIDR IPv4 address range. We recommend that you specify the CIDR range in a set of IP permissions instead.
asgeCidrIP :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeCidrIP = lens _asgeCidrIP (\ s a -> s{_asgeCidrIP = a});

-- | The AWS account number for a destination security group. To authorize outbound access to a destination security group, we recommend that you use a set of IP permissions instead.
asgeSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeSourceSecurityGroupOwnerId = lens _asgeSourceSecurityGroupOwnerId (\ s a -> s{_asgeSourceSecurityGroupOwnerId = a});

-- | The name of a destination security group. To authorize outbound access to a destination security group, we recommend that you use a set of IP permissions instead.
asgeSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeSourceSecurityGroupName = lens _asgeSourceSecurityGroupName (\ s a -> s{_asgeSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
asgeDryRun :: Lens' AuthorizeSecurityGroupEgress (Maybe Bool)
asgeDryRun = lens _asgeDryRun (\ s a -> s{_asgeDryRun = a});

-- | The ID of the security group.
asgeGroupId :: Lens' AuthorizeSecurityGroupEgress Text
asgeGroupId = lens _asgeGroupId (\ s a -> s{_asgeGroupId = a});

instance AWSRequest AuthorizeSecurityGroupEgress
         where
        type Rs AuthorizeSecurityGroupEgress =
             AuthorizeSecurityGroupEgressResponse
        request = postQuery ec2
        response
          = receiveNull AuthorizeSecurityGroupEgressResponse'

instance Hashable AuthorizeSecurityGroupEgress where

instance NFData AuthorizeSecurityGroupEgress where

instance ToHeaders AuthorizeSecurityGroupEgress where
        toHeaders = const mempty

instance ToPath AuthorizeSecurityGroupEgress where
        toPath = const "/"

instance ToQuery AuthorizeSecurityGroupEgress where
        toQuery AuthorizeSecurityGroupEgress'{..}
          = mconcat
              ["Action" =:
                 ("AuthorizeSecurityGroupEgress" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "FromPort" =: _asgeFromPort,
               toQuery
                 (toQueryList "IpPermissions" <$> _asgeIPPermissions),
               "IpProtocol" =: _asgeIPProtocol,
               "ToPort" =: _asgeToPort, "CidrIp" =: _asgeCidrIP,
               "SourceSecurityGroupOwnerId" =:
                 _asgeSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _asgeSourceSecurityGroupName,
               "DryRun" =: _asgeDryRun, "GroupId" =: _asgeGroupId]

-- | /See:/ 'authorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse =
  AuthorizeSecurityGroupEgressResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeSecurityGroupEgressResponse' with the minimum fields required to make a request.
--
authorizeSecurityGroupEgressResponse
    :: AuthorizeSecurityGroupEgressResponse
authorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'


instance NFData AuthorizeSecurityGroupEgressResponse
         where
