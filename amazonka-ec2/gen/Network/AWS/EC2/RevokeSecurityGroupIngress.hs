{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.RevokeSecurityGroupIngress
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

-- | Removes one or more ingress rules from a security group. The values that
-- you specify in the revoke request (for example, ports) must match the
-- existing rule\'s values for the rule to be removed.
--
-- Each rule consists of the protocol and the CIDR range or source security
-- group. For the TCP and UDP protocols, you must also specify the
-- destination port or range of ports. For the ICMP protocol, you must also
-- specify the ICMP type and code.
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupIngress.html>
module Network.AWS.EC2.RevokeSecurityGroupIngress
    (
    -- * Request
      RevokeSecurityGroupIngress
    -- ** Request constructor
    , revokeSecurityGroupIngress
    -- ** Request lenses
    , rsgiFromPort
    , rsgiIPPermissions
    , rsgiIPProtocol
    , rsgiGroupId
    , rsgiToPort
    , rsgiCIDRIP
    , rsgiGroupName
    , rsgiSourceSecurityGroupOwnerId
    , rsgiSourceSecurityGroupName
    , rsgiDryRun

    -- * Response
    , RevokeSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeSecurityGroupIngressResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'revokeSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsgiFromPort'
--
-- * 'rsgiIPPermissions'
--
-- * 'rsgiIPProtocol'
--
-- * 'rsgiGroupId'
--
-- * 'rsgiToPort'
--
-- * 'rsgiCIDRIP'
--
-- * 'rsgiGroupName'
--
-- * 'rsgiSourceSecurityGroupOwnerId'
--
-- * 'rsgiSourceSecurityGroupName'
--
-- * 'rsgiDryRun'
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress'{_rsgiFromPort :: Maybe Int, _rsgiIPPermissions :: Maybe [IPPermission], _rsgiIPProtocol :: Maybe Text, _rsgiGroupId :: Maybe Text, _rsgiToPort :: Maybe Int, _rsgiCIDRIP :: Maybe Text, _rsgiGroupName :: Maybe Text, _rsgiSourceSecurityGroupOwnerId :: Maybe Text, _rsgiSourceSecurityGroupName :: Maybe Text, _rsgiDryRun :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'RevokeSecurityGroupIngress' smart constructor.
revokeSecurityGroupIngress :: RevokeSecurityGroupIngress
revokeSecurityGroupIngress = RevokeSecurityGroupIngress'{_rsgiFromPort = Nothing, _rsgiIPPermissions = Nothing, _rsgiIPProtocol = Nothing, _rsgiGroupId = Nothing, _rsgiToPort = Nothing, _rsgiCIDRIP = Nothing, _rsgiGroupName = Nothing, _rsgiSourceSecurityGroupOwnerId = Nothing, _rsgiSourceSecurityGroupName = Nothing, _rsgiDryRun = Nothing};

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
rsgiFromPort :: Lens' RevokeSecurityGroupIngress (Maybe Int)
rsgiFromPort = lens _rsgiFromPort (\ s a -> s{_rsgiFromPort = a});

-- | A set of IP permissions. You can\'t specify a source security group and
-- a CIDR IP address range.
rsgiIPPermissions :: Lens' RevokeSecurityGroupIngress [IPPermission]
rsgiIPPermissions = lens _rsgiIPPermissions (\ s a -> s{_rsgiIPPermissions = a}) . _Default;

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- Use @-1@ to specify all.
rsgiIPProtocol :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiIPProtocol = lens _rsgiIPProtocol (\ s a -> s{_rsgiIPProtocol = a});

-- | The ID of the security group.
rsgiGroupId :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiGroupId = lens _rsgiGroupId (\ s a -> s{_rsgiGroupId = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
rsgiToPort :: Lens' RevokeSecurityGroupIngress (Maybe Int)
rsgiToPort = lens _rsgiToPort (\ s a -> s{_rsgiToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
rsgiCIDRIP :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiCIDRIP = lens _rsgiCIDRIP (\ s a -> s{_rsgiCIDRIP = a});

-- | [EC2-Classic, default VPC] The name of the security group.
rsgiGroupName :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiGroupName = lens _rsgiGroupName (\ s a -> s{_rsgiGroupName = a});

-- | The ID of the source security group. You can\'t specify a source
-- security group and a CIDR IP address range.
rsgiSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiSourceSecurityGroupOwnerId = lens _rsgiSourceSecurityGroupOwnerId (\ s a -> s{_rsgiSourceSecurityGroupOwnerId = a});

-- | [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify a source security group and a CIDR IP address range.
rsgiSourceSecurityGroupName :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiSourceSecurityGroupName = lens _rsgiSourceSecurityGroupName (\ s a -> s{_rsgiSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsgiDryRun :: Lens' RevokeSecurityGroupIngress (Maybe Bool)
rsgiDryRun = lens _rsgiDryRun (\ s a -> s{_rsgiDryRun = a});

instance AWSRequest RevokeSecurityGroupIngress where
        type Sv RevokeSecurityGroupIngress = EC2
        type Rs RevokeSecurityGroupIngress =
             RevokeSecurityGroupIngressResponse
        request = post
        response
          = receiveNull RevokeSecurityGroupIngressResponse'

instance ToHeaders RevokeSecurityGroupIngress where
        toHeaders = const mempty

instance ToPath RevokeSecurityGroupIngress where
        toPath = const "/"

instance ToQuery RevokeSecurityGroupIngress where
        toQuery RevokeSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("RevokeSecurityGroupIngress" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "FromPort" =: _rsgiFromPort,
               toQuery (toQueryList "item" <$> _rsgiIPPermissions),
               "IpProtocol" =: _rsgiIPProtocol,
               "GroupId" =: _rsgiGroupId, "ToPort" =: _rsgiToPort,
               "CidrIp" =: _rsgiCIDRIP,
               "GroupName" =: _rsgiGroupName,
               "SourceSecurityGroupOwnerId" =:
                 _rsgiSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _rsgiSourceSecurityGroupName,
               "DryRun" =: _rsgiDryRun]

-- | /See:/ 'revokeSecurityGroupIngressResponse' smart constructor.
data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse' deriving (Eq, Read, Show)

-- | 'RevokeSecurityGroupIngressResponse' smart constructor.
revokeSecurityGroupIngressResponse :: RevokeSecurityGroupIngressResponse
revokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse';
