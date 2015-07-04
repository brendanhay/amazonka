{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupIngress
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

-- | Adds one or more ingress rules to a security group.
--
-- EC2-Classic: You can have up to 100 rules per group.
--
-- EC2-VPC: You can have up to 50 rules per group (covering both ingress
-- and egress rules).
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
--
-- [EC2-Classic] This action gives one or more CIDR IP address ranges
-- permission to access a security group in your account, or gives one or
-- more security groups (called the /source groups/) permission to access a
-- security group for your account. A source group can be for your own AWS
-- account, or another.
--
-- [EC2-VPC] This action gives one or more CIDR IP address ranges
-- permission to access a security group in your VPC, or gives one or more
-- other security groups (called the /source groups/) permission to access
-- a security group for your VPC. The security groups must all be for the
-- same VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupIngress.html>
module Network.AWS.EC2.AuthorizeSecurityGroupIngress
    (
    -- * Request
      AuthorizeSecurityGroupIngress
    -- ** Request constructor
    , authorizeSecurityGroupIngress
    -- ** Request lenses
    , asgiFromPort
    , asgiIPPermissions
    , asgiIPProtocol
    , asgiGroupId
    , asgiToPort
    , asgiCIDRIP
    , asgiGroupName
    , asgiSourceSecurityGroupOwnerId
    , asgiSourceSecurityGroupName
    , asgiDryRun

    -- * Response
    , AuthorizeSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeSecurityGroupIngressResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'authorizeSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgiFromPort'
--
-- * 'asgiIPPermissions'
--
-- * 'asgiIPProtocol'
--
-- * 'asgiGroupId'
--
-- * 'asgiToPort'
--
-- * 'asgiCIDRIP'
--
-- * 'asgiGroupName'
--
-- * 'asgiSourceSecurityGroupOwnerId'
--
-- * 'asgiSourceSecurityGroupName'
--
-- * 'asgiDryRun'
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress'
    { _asgiFromPort                   :: !(Maybe Int)
    , _asgiIPPermissions              :: !(Maybe [IPPermission])
    , _asgiIPProtocol                 :: !(Maybe Text)
    , _asgiGroupId                    :: !(Maybe Text)
    , _asgiToPort                     :: !(Maybe Int)
    , _asgiCIDRIP                     :: !(Maybe Text)
    , _asgiGroupName                  :: !(Maybe Text)
    , _asgiSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _asgiSourceSecurityGroupName    :: !(Maybe Text)
    , _asgiDryRun                     :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeSecurityGroupIngress' smart constructor.
authorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress
authorizeSecurityGroupIngress =
    AuthorizeSecurityGroupIngress'
    { _asgiFromPort = Nothing
    , _asgiIPPermissions = Nothing
    , _asgiIPProtocol = Nothing
    , _asgiGroupId = Nothing
    , _asgiToPort = Nothing
    , _asgiCIDRIP = Nothing
    , _asgiGroupName = Nothing
    , _asgiSourceSecurityGroupOwnerId = Nothing
    , _asgiSourceSecurityGroupName = Nothing
    , _asgiDryRun = Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
asgiFromPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Int)
asgiFromPort = lens _asgiFromPort (\ s a -> s{_asgiFromPort = a});

-- | A set of IP permissions. Can be used to specify multiple rules in a
-- single command.
asgiIPPermissions :: Lens' AuthorizeSecurityGroupIngress [IPPermission]
asgiIPPermissions = lens _asgiIPPermissions (\ s a -> s{_asgiIPPermissions = a}) . _Default;

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- (VPC only) Use @-1@ to specify all.
asgiIPProtocol :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiIPProtocol = lens _asgiIPProtocol (\ s a -> s{_asgiIPProtocol = a});

-- | The ID of the security group. Required for a nondefault VPC.
asgiGroupId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiGroupId = lens _asgiGroupId (\ s a -> s{_asgiGroupId = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
asgiToPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Int)
asgiToPort = lens _asgiToPort (\ s a -> s{_asgiToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
asgiCIDRIP :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiCIDRIP = lens _asgiCIDRIP (\ s a -> s{_asgiCIDRIP = a});

-- | [EC2-Classic, default VPC] The name of the security group.
asgiGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiGroupName = lens _asgiGroupName (\ s a -> s{_asgiGroupName = a});

-- | The ID of the source security group. You can\'t specify this parameter
-- in combination with the following parameters: the CIDR IP address range,
-- the start of the port range, and the end of the port range.
asgiSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiSourceSecurityGroupOwnerId = lens _asgiSourceSecurityGroupOwnerId (\ s a -> s{_asgiSourceSecurityGroupOwnerId = a});

-- | [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, and
-- the end of the port range.
asgiSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiSourceSecurityGroupName = lens _asgiSourceSecurityGroupName (\ s a -> s{_asgiSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
asgiDryRun :: Lens' AuthorizeSecurityGroupIngress (Maybe Bool)
asgiDryRun = lens _asgiDryRun (\ s a -> s{_asgiDryRun = a});

instance AWSRequest AuthorizeSecurityGroupIngress
         where
        type Sv AuthorizeSecurityGroupIngress = EC2
        type Rs AuthorizeSecurityGroupIngress =
             AuthorizeSecurityGroupIngressResponse
        request = post
        response
          = receiveNull AuthorizeSecurityGroupIngressResponse'

instance ToHeaders AuthorizeSecurityGroupIngress
         where
        toHeaders = const mempty

instance ToPath AuthorizeSecurityGroupIngress where
        toPath = const "/"

instance ToQuery AuthorizeSecurityGroupIngress where
        toQuery AuthorizeSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("AuthorizeSecurityGroupIngress" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "FromPort" =: _asgiFromPort,
               toQuery (toQueryList "item" <$> _asgiIPPermissions),
               "IpProtocol" =: _asgiIPProtocol,
               "GroupId" =: _asgiGroupId, "ToPort" =: _asgiToPort,
               "CidrIp" =: _asgiCIDRIP,
               "GroupName" =: _asgiGroupName,
               "SourceSecurityGroupOwnerId" =:
                 _asgiSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _asgiSourceSecurityGroupName,
               "DryRun" =: _asgiDryRun]

-- | /See:/ 'authorizeSecurityGroupIngressResponse' smart constructor.
data AuthorizeSecurityGroupIngressResponse =
    AuthorizeSecurityGroupIngressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeSecurityGroupIngressResponse' smart constructor.
authorizeSecurityGroupIngressResponse :: AuthorizeSecurityGroupIngressResponse
authorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse'
