{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupEgress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds one or more egress rules to a security group for use with a VPC.
-- Specifically, this action permits instances to send traffic to one or
-- more destination CIDR IP address ranges, or to one or more destination
-- security groups for the same VPC.
--
-- You can have up to 50 rules per security group (covering both ingress
-- and egress rules).
--
-- A security group is for use with instances either in the EC2-Classic
-- platform or in a specific VPC. This action doesn\'t apply to security
-- groups for use in EC2-Classic. For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- Each rule consists of the protocol (for example, TCP), plus either a
-- CIDR range or a source group. For the TCP and UDP protocols, you must
-- also specify the destination port or port range. For the ICMP protocol,
-- you must also specify the ICMP type and code. You can use -1 for the
-- type or code to mean all types or all codes.
--
-- Rule changes are propagated to affected instances as quickly as
-- possible. However, a small delay might occur.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupEgress.html>
module Network.AWS.EC2.AuthorizeSecurityGroupEgress
    (
    -- * Request
      AuthorizeSecurityGroupEgress
    -- ** Request constructor
    , authorizeSecurityGroupEgress
    -- ** Request lenses
    , asgeFromPort
    , asgeIPPermissions
    , asgeIPProtocol
    , asgeToPort
    , asgeCIDRIP
    , asgeSourceSecurityGroupOwnerId
    , asgeSourceSecurityGroupName
    , asgeDryRun
    , asgeGroupId

    -- * Response
    , AuthorizeSecurityGroupEgressResponse
    -- ** Response constructor
    , authorizeSecurityGroupEgressResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'authorizeSecurityGroupEgress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgeFromPort'
--
-- * 'asgeIPPermissions'
--
-- * 'asgeIPProtocol'
--
-- * 'asgeToPort'
--
-- * 'asgeCIDRIP'
--
-- * 'asgeSourceSecurityGroupOwnerId'
--
-- * 'asgeSourceSecurityGroupName'
--
-- * 'asgeDryRun'
--
-- * 'asgeGroupId'
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
    { _asgeFromPort                   :: !(Maybe Int)
    , _asgeIPPermissions              :: !(Maybe [IPPermission])
    , _asgeIPProtocol                 :: !(Maybe Text)
    , _asgeToPort                     :: !(Maybe Int)
    , _asgeCIDRIP                     :: !(Maybe Text)
    , _asgeSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _asgeSourceSecurityGroupName    :: !(Maybe Text)
    , _asgeDryRun                     :: !(Maybe Bool)
    , _asgeGroupId                    :: !Text
    } deriving (Eq,Read,Show)

-- | 'AuthorizeSecurityGroupEgress' smart constructor.
authorizeSecurityGroupEgress :: Text -> AuthorizeSecurityGroupEgress
authorizeSecurityGroupEgress pGroupId =
    AuthorizeSecurityGroupEgress'
    { _asgeFromPort = Nothing
    , _asgeIPPermissions = Nothing
    , _asgeIPProtocol = Nothing
    , _asgeToPort = Nothing
    , _asgeCIDRIP = Nothing
    , _asgeSourceSecurityGroupOwnerId = Nothing
    , _asgeSourceSecurityGroupName = Nothing
    , _asgeDryRun = Nothing
    , _asgeGroupId = pGroupId
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
asgeFromPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgeFromPort = lens _asgeFromPort (\ s a -> s{_asgeFromPort = a});

-- | A set of IP permissions. You can\'t specify a destination security group
-- and a CIDR IP address range.
asgeIPPermissions :: Lens' AuthorizeSecurityGroupEgress [IPPermission]
asgeIPPermissions = lens _asgeIPPermissions (\ s a -> s{_asgeIPPermissions = a}) . _Default;

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- Use @-1@ to specify all.
asgeIPProtocol :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeIPProtocol = lens _asgeIPProtocol (\ s a -> s{_asgeIPProtocol = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
asgeToPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgeToPort = lens _asgeToPort (\ s a -> s{_asgeToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
asgeCIDRIP :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeCIDRIP = lens _asgeCIDRIP (\ s a -> s{_asgeCIDRIP = a});

-- | The ID of the destination security group. You can\'t specify a
-- destination security group and a CIDR IP address range.
asgeSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeSourceSecurityGroupOwnerId = lens _asgeSourceSecurityGroupOwnerId (\ s a -> s{_asgeSourceSecurityGroupOwnerId = a});

-- | [EC2-Classic, default VPC] The name of the destination security group.
-- You can\'t specify a destination security group and a CIDR IP address
-- range.
asgeSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeSourceSecurityGroupName = lens _asgeSourceSecurityGroupName (\ s a -> s{_asgeSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
asgeDryRun :: Lens' AuthorizeSecurityGroupEgress (Maybe Bool)
asgeDryRun = lens _asgeDryRun (\ s a -> s{_asgeDryRun = a});

-- | The ID of the security group.
asgeGroupId :: Lens' AuthorizeSecurityGroupEgress Text
asgeGroupId = lens _asgeGroupId (\ s a -> s{_asgeGroupId = a});

instance AWSRequest AuthorizeSecurityGroupEgress
         where
        type Sv AuthorizeSecurityGroupEgress = EC2
        type Rs AuthorizeSecurityGroupEgress =
             AuthorizeSecurityGroupEgressResponse
        request = post
        response
          = receiveNull AuthorizeSecurityGroupEgressResponse'

instance ToHeaders AuthorizeSecurityGroupEgress where
        toHeaders = const mempty

instance ToPath AuthorizeSecurityGroupEgress where
        toPath = const "/"

instance ToQuery AuthorizeSecurityGroupEgress where
        toQuery AuthorizeSecurityGroupEgress'{..}
          = mconcat
              ["Action" =:
                 ("AuthorizeSecurityGroupEgress" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "FromPort" =: _asgeFromPort,
               toQuery (toQueryList "item" <$> _asgeIPPermissions),
               "IpProtocol" =: _asgeIPProtocol,
               "ToPort" =: _asgeToPort, "CidrIp" =: _asgeCIDRIP,
               "SourceSecurityGroupOwnerId" =:
                 _asgeSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _asgeSourceSecurityGroupName,
               "DryRun" =: _asgeDryRun, "GroupId" =: _asgeGroupId]

-- | /See:/ 'authorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse =
    AuthorizeSecurityGroupEgressResponse'
    deriving (Eq,Read,Show)

-- | 'AuthorizeSecurityGroupEgressResponse' smart constructor.
authorizeSecurityGroupEgressResponse :: AuthorizeSecurityGroupEgressResponse
authorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'
