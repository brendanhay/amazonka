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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more egress rules from a security group for EC2-VPC. The
-- values that you specify in the revoke request (for example, ports) must
-- match the existing rule\'s values for the rule to be revoked.
--
-- Each rule consists of the protocol and the CIDR range or source security
-- group. For the TCP and UDP protocols, you must also specify the
-- destination port or range of ports. For the ICMP protocol, you must also
-- specify the ICMP type and code.
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupEgress.html AWS API Reference> for RevokeSecurityGroupEgress.
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
    , rsgeCIdRIP
    , rsgeSourceSecurityGroupOwnerId
    , rsgeSourceSecurityGroupName
    , rsgeDryRun
    , rsgeGroupId

    -- * Destructuring the Response
    , revokeSecurityGroupEgressResponse
    , RevokeSecurityGroupEgressResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'revokeSecurityGroupEgress' smart constructor.
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress'
    { _rsgeFromPort                   :: !(Maybe Int)
    , _rsgeIPPermissions              :: !(Maybe [IPPermission])
    , _rsgeIPProtocol                 :: !(Maybe Text)
    , _rsgeToPort                     :: !(Maybe Int)
    , _rsgeCIdRIP                     :: !(Maybe Text)
    , _rsgeSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _rsgeSourceSecurityGroupName    :: !(Maybe Text)
    , _rsgeDryRun                     :: !(Maybe Bool)
    , _rsgeGroupId                    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RevokeSecurityGroupEgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsgeFromPort'
--
-- * 'rsgeIPPermissions'
--
-- * 'rsgeIPProtocol'
--
-- * 'rsgeToPort'
--
-- * 'rsgeCIdRIP'
--
-- * 'rsgeSourceSecurityGroupOwnerId'
--
-- * 'rsgeSourceSecurityGroupName'
--
-- * 'rsgeDryRun'
--
-- * 'rsgeGroupId'
revokeSecurityGroupEgress
    :: Text -- ^ 'rsgeGroupId'
    -> RevokeSecurityGroupEgress
revokeSecurityGroupEgress pGroupId_ =
    RevokeSecurityGroupEgress'
    { _rsgeFromPort = Nothing
    , _rsgeIPPermissions = Nothing
    , _rsgeIPProtocol = Nothing
    , _rsgeToPort = Nothing
    , _rsgeCIdRIP = Nothing
    , _rsgeSourceSecurityGroupOwnerId = Nothing
    , _rsgeSourceSecurityGroupName = Nothing
    , _rsgeDryRun = Nothing
    , _rsgeGroupId = pGroupId_
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use '-1' to specify all ICMP types.
rsgeFromPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgeFromPort = lens _rsgeFromPort (\ s a -> s{_rsgeFromPort = a});

-- | A set of IP permissions. You can\'t specify a destination security group
-- and a CIDR IP address range.
rsgeIPPermissions :: Lens' RevokeSecurityGroupEgress [IPPermission]
rsgeIPPermissions = lens _rsgeIPPermissions (\ s a -> s{_rsgeIPPermissions = a}) . _Default . _Coerce;

-- | The IP protocol name ('tcp', 'udp', 'icmp') or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- Use '-1' to specify all.
rsgeIPProtocol :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeIPProtocol = lens _rsgeIPProtocol (\ s a -> s{_rsgeIPProtocol = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use '-1' to specify all ICMP codes for
-- the ICMP type.
rsgeToPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgeToPort = lens _rsgeToPort (\ s a -> s{_rsgeToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
rsgeCIdRIP :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeCIdRIP = lens _rsgeCIdRIP (\ s a -> s{_rsgeCIdRIP = a});

-- | The AWS account number for a destination security group. To revoke
-- outbound access to a destination security group, we recommend that you
-- use a set of IP permissions instead.
rsgeSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupOwnerId = lens _rsgeSourceSecurityGroupOwnerId (\ s a -> s{_rsgeSourceSecurityGroupOwnerId = a});

-- | The name of a destination security group. To revoke outbound access to a
-- destination security group, we recommend that you use a set of IP
-- permissions instead.
rsgeSourceSecurityGroupName :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupName = lens _rsgeSourceSecurityGroupName (\ s a -> s{_rsgeSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
rsgeDryRun :: Lens' RevokeSecurityGroupEgress (Maybe Bool)
rsgeDryRun = lens _rsgeDryRun (\ s a -> s{_rsgeDryRun = a});

-- | The ID of the security group.
rsgeGroupId :: Lens' RevokeSecurityGroupEgress Text
rsgeGroupId = lens _rsgeGroupId (\ s a -> s{_rsgeGroupId = a});

instance AWSRequest RevokeSecurityGroupEgress where
        type Rs RevokeSecurityGroupEgress =
             RevokeSecurityGroupEgressResponse
        request = postQuery eC2
        response
          = receiveNull RevokeSecurityGroupEgressResponse'

instance ToHeaders RevokeSecurityGroupEgress where
        toHeaders = const mempty

instance ToPath RevokeSecurityGroupEgress where
        toPath = const "/"

instance ToQuery RevokeSecurityGroupEgress where
        toQuery RevokeSecurityGroupEgress'{..}
          = mconcat
              ["Action" =:
                 ("RevokeSecurityGroupEgress" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "FromPort" =: _rsgeFromPort,
               toQuery
                 (toQueryList "IpPermissions" <$> _rsgeIPPermissions),
               "IpProtocol" =: _rsgeIPProtocol,
               "ToPort" =: _rsgeToPort, "CidrIp" =: _rsgeCIdRIP,
               "SourceSecurityGroupOwnerId" =:
                 _rsgeSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _rsgeSourceSecurityGroupName,
               "DryRun" =: _rsgeDryRun, "GroupId" =: _rsgeGroupId]

-- | /See:/ 'revokeSecurityGroupEgressResponse' smart constructor.
data RevokeSecurityGroupEgressResponse =
    RevokeSecurityGroupEgressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RevokeSecurityGroupEgressResponse' with the minimum fields required to make a request.
--
revokeSecurityGroupEgressResponse
    :: RevokeSecurityGroupEgressResponse
revokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse'
