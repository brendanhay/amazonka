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
-- Module      : Network.AWS.EC2.RevokeSecurityGroupEgress
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [EC2-VPC only] Removes one or more egress rules from a security group for EC2-VPC. This action doesn't apply to security groups for use in EC2-Classic. The values that you specify in the revoke request (for example, ports) must match the existing rule's values for the rule to be revoked.
--
--
-- Each rule consists of the protocol and the IPv4 or IPv6 CIDR range or source security group. For the TCP and UDP protocols, you must also specify the destination port or range of ports. For the ICMP protocol, you must also specify the ICMP type and code.
--
-- Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.
--
module Network.AWS.EC2.RevokeSecurityGroupEgress
    (
    -- * Creating a Request
      revokeSecurityGroupEgress
    , RevokeSecurityGroupEgress
    -- * Request Lenses
    , rsgeFromPort
    , rsgeIPPermissions
    , rsgeIPProtocol
    , rsgeToPort
    , rsgeCidrIP
    , rsgeSourceSecurityGroupOwnerId
    , rsgeSourceSecurityGroupName
    , rsgeDryRun
    , rsgeGroupId

    -- * Destructuring the Response
    , revokeSecurityGroupEgressResponse
    , RevokeSecurityGroupEgressResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for RevokeSecurityGroupEgress.
--
--
--
-- /See:/ 'revokeSecurityGroupEgress' smart constructor.
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress'
  { _rsgeFromPort                   :: {-# NOUNPACK #-}!(Maybe Int)
  , _rsgeIPPermissions              :: {-# NOUNPACK #-}!(Maybe [IPPermission])
  , _rsgeIPProtocol                 :: {-# NOUNPACK #-}!(Maybe Text)
  , _rsgeToPort                     :: {-# NOUNPACK #-}!(Maybe Int)
  , _rsgeCidrIP                     :: {-# NOUNPACK #-}!(Maybe Text)
  , _rsgeSourceSecurityGroupOwnerId :: {-# NOUNPACK #-}!(Maybe Text)
  , _rsgeSourceSecurityGroupName    :: {-# NOUNPACK #-}!(Maybe Text)
  , _rsgeDryRun                     :: {-# NOUNPACK #-}!(Maybe Bool)
  , _rsgeGroupId                    :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeSecurityGroupEgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsgeFromPort' - The start of port range for the TCP and UDP protocols, or an ICMP type number. We recommend that you specify the port range in a set of IP permissions instead.
--
-- * 'rsgeIPPermissions' - A set of IP permissions. You can't specify a destination security group and a CIDR IP address range.
--
-- * 'rsgeIPProtocol' - The IP protocol name or number. We recommend that you specify the protocol in a set of IP permissions instead.
--
-- * 'rsgeToPort' - The end of port range for the TCP and UDP protocols, or an ICMP type number. We recommend that you specify the port range in a set of IP permissions instead.
--
-- * 'rsgeCidrIP' - The CIDR IP address range. We recommend that you specify the CIDR range in a set of IP permissions instead.
--
-- * 'rsgeSourceSecurityGroupOwnerId' - The AWS account number for a destination security group. To revoke outbound access to a destination security group, we recommend that you use a set of IP permissions instead.
--
-- * 'rsgeSourceSecurityGroupName' - The name of a destination security group. To revoke outbound access to a destination security group, we recommend that you use a set of IP permissions instead.
--
-- * 'rsgeDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rsgeGroupId' - The ID of the security group.
revokeSecurityGroupEgress
    :: Text -- ^ 'rsgeGroupId'
    -> RevokeSecurityGroupEgress
revokeSecurityGroupEgress pGroupId_ =
  RevokeSecurityGroupEgress'
  { _rsgeFromPort = Nothing
  , _rsgeIPPermissions = Nothing
  , _rsgeIPProtocol = Nothing
  , _rsgeToPort = Nothing
  , _rsgeCidrIP = Nothing
  , _rsgeSourceSecurityGroupOwnerId = Nothing
  , _rsgeSourceSecurityGroupName = Nothing
  , _rsgeDryRun = Nothing
  , _rsgeGroupId = pGroupId_
  }


-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. We recommend that you specify the port range in a set of IP permissions instead.
rsgeFromPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgeFromPort = lens _rsgeFromPort (\ s a -> s{_rsgeFromPort = a});

-- | A set of IP permissions. You can't specify a destination security group and a CIDR IP address range.
rsgeIPPermissions :: Lens' RevokeSecurityGroupEgress [IPPermission]
rsgeIPPermissions = lens _rsgeIPPermissions (\ s a -> s{_rsgeIPPermissions = a}) . _Default . _Coerce;

-- | The IP protocol name or number. We recommend that you specify the protocol in a set of IP permissions instead.
rsgeIPProtocol :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeIPProtocol = lens _rsgeIPProtocol (\ s a -> s{_rsgeIPProtocol = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP type number. We recommend that you specify the port range in a set of IP permissions instead.
rsgeToPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgeToPort = lens _rsgeToPort (\ s a -> s{_rsgeToPort = a});

-- | The CIDR IP address range. We recommend that you specify the CIDR range in a set of IP permissions instead.
rsgeCidrIP :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeCidrIP = lens _rsgeCidrIP (\ s a -> s{_rsgeCidrIP = a});

-- | The AWS account number for a destination security group. To revoke outbound access to a destination security group, we recommend that you use a set of IP permissions instead.
rsgeSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupOwnerId = lens _rsgeSourceSecurityGroupOwnerId (\ s a -> s{_rsgeSourceSecurityGroupOwnerId = a});

-- | The name of a destination security group. To revoke outbound access to a destination security group, we recommend that you use a set of IP permissions instead.
rsgeSourceSecurityGroupName :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupName = lens _rsgeSourceSecurityGroupName (\ s a -> s{_rsgeSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rsgeDryRun :: Lens' RevokeSecurityGroupEgress (Maybe Bool)
rsgeDryRun = lens _rsgeDryRun (\ s a -> s{_rsgeDryRun = a});

-- | The ID of the security group.
rsgeGroupId :: Lens' RevokeSecurityGroupEgress Text
rsgeGroupId = lens _rsgeGroupId (\ s a -> s{_rsgeGroupId = a});

instance AWSRequest RevokeSecurityGroupEgress where
        type Rs RevokeSecurityGroupEgress =
             RevokeSecurityGroupEgressResponse
        request = postQuery ec2
        response
          = receiveNull RevokeSecurityGroupEgressResponse'

instance Hashable RevokeSecurityGroupEgress where

instance NFData RevokeSecurityGroupEgress where

instance ToHeaders RevokeSecurityGroupEgress where
        toHeaders = const mempty

instance ToPath RevokeSecurityGroupEgress where
        toPath = const "/"

instance ToQuery RevokeSecurityGroupEgress where
        toQuery RevokeSecurityGroupEgress'{..}
          = mconcat
              ["Action" =:
                 ("RevokeSecurityGroupEgress" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "FromPort" =: _rsgeFromPort,
               toQuery
                 (toQueryList "IpPermissions" <$> _rsgeIPPermissions),
               "IpProtocol" =: _rsgeIPProtocol,
               "ToPort" =: _rsgeToPort, "CidrIp" =: _rsgeCidrIP,
               "SourceSecurityGroupOwnerId" =:
                 _rsgeSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _rsgeSourceSecurityGroupName,
               "DryRun" =: _rsgeDryRun, "GroupId" =: _rsgeGroupId]

-- | /See:/ 'revokeSecurityGroupEgressResponse' smart constructor.
data RevokeSecurityGroupEgressResponse =
  RevokeSecurityGroupEgressResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeSecurityGroupEgressResponse' with the minimum fields required to make a request.
--
revokeSecurityGroupEgressResponse
    :: RevokeSecurityGroupEgressResponse
revokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse'


instance NFData RevokeSecurityGroupEgressResponse
         where
