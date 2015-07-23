{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more ingress rules to a security group.
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
    , asgirqFromPort
    , asgirqIPPermissions
    , asgirqIPProtocol
    , asgirqGroupId
    , asgirqToPort
    , asgirqCIdRIP
    , asgirqGroupName
    , asgirqSourceSecurityGroupOwnerId
    , asgirqSourceSecurityGroupName
    , asgirqDryRun

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
-- * 'asgirqFromPort'
--
-- * 'asgirqIPPermissions'
--
-- * 'asgirqIPProtocol'
--
-- * 'asgirqGroupId'
--
-- * 'asgirqToPort'
--
-- * 'asgirqCIdRIP'
--
-- * 'asgirqGroupName'
--
-- * 'asgirqSourceSecurityGroupOwnerId'
--
-- * 'asgirqSourceSecurityGroupName'
--
-- * 'asgirqDryRun'
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress'
    { _asgirqFromPort                   :: !(Maybe Int)
    , _asgirqIPPermissions              :: !(Maybe [IPPermission])
    , _asgirqIPProtocol                 :: !(Maybe Text)
    , _asgirqGroupId                    :: !(Maybe Text)
    , _asgirqToPort                     :: !(Maybe Int)
    , _asgirqCIdRIP                     :: !(Maybe Text)
    , _asgirqGroupName                  :: !(Maybe Text)
    , _asgirqSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _asgirqSourceSecurityGroupName    :: !(Maybe Text)
    , _asgirqDryRun                     :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeSecurityGroupIngress' smart constructor.
authorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress
authorizeSecurityGroupIngress =
    AuthorizeSecurityGroupIngress'
    { _asgirqFromPort = Nothing
    , _asgirqIPPermissions = Nothing
    , _asgirqIPProtocol = Nothing
    , _asgirqGroupId = Nothing
    , _asgirqToPort = Nothing
    , _asgirqCIdRIP = Nothing
    , _asgirqGroupName = Nothing
    , _asgirqSourceSecurityGroupOwnerId = Nothing
    , _asgirqSourceSecurityGroupName = Nothing
    , _asgirqDryRun = Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
asgirqFromPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Int)
asgirqFromPort = lens _asgirqFromPort (\ s a -> s{_asgirqFromPort = a});

-- | A set of IP permissions. Can be used to specify multiple rules in a
-- single command.
asgirqIPPermissions :: Lens' AuthorizeSecurityGroupIngress [IPPermission]
asgirqIPPermissions = lens _asgirqIPPermissions (\ s a -> s{_asgirqIPPermissions = a}) . _Default;

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- (VPC only) Use @-1@ to specify all.
asgirqIPProtocol :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgirqIPProtocol = lens _asgirqIPProtocol (\ s a -> s{_asgirqIPProtocol = a});

-- | The ID of the security group. Required for a nondefault VPC.
asgirqGroupId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgirqGroupId = lens _asgirqGroupId (\ s a -> s{_asgirqGroupId = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
asgirqToPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Int)
asgirqToPort = lens _asgirqToPort (\ s a -> s{_asgirqToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
asgirqCIdRIP :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgirqCIdRIP = lens _asgirqCIdRIP (\ s a -> s{_asgirqCIdRIP = a});

-- | [EC2-Classic, default VPC] The name of the security group.
asgirqGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgirqGroupName = lens _asgirqGroupName (\ s a -> s{_asgirqGroupName = a});

-- | The ID of the source security group. You can\'t specify this parameter
-- in combination with the following parameters: the CIDR IP address range,
-- the start of the port range, and the end of the port range.
asgirqSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgirqSourceSecurityGroupOwnerId = lens _asgirqSourceSecurityGroupOwnerId (\ s a -> s{_asgirqSourceSecurityGroupOwnerId = a});

-- | [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, and
-- the end of the port range.
asgirqSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgirqSourceSecurityGroupName = lens _asgirqSourceSecurityGroupName (\ s a -> s{_asgirqSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
asgirqDryRun :: Lens' AuthorizeSecurityGroupIngress (Maybe Bool)
asgirqDryRun = lens _asgirqDryRun (\ s a -> s{_asgirqDryRun = a});

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
               "FromPort" =: _asgirqFromPort,
               toQuery
                 (toQueryList "item" <$> _asgirqIPPermissions),
               "IpProtocol" =: _asgirqIPProtocol,
               "GroupId" =: _asgirqGroupId,
               "ToPort" =: _asgirqToPort, "CidrIp" =: _asgirqCIdRIP,
               "GroupName" =: _asgirqGroupName,
               "SourceSecurityGroupOwnerId" =:
                 _asgirqSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _asgirqSourceSecurityGroupName,
               "DryRun" =: _asgirqDryRun]

-- | /See:/ 'authorizeSecurityGroupIngressResponse' smart constructor.
data AuthorizeSecurityGroupIngressResponse =
    AuthorizeSecurityGroupIngressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeSecurityGroupIngressResponse' smart constructor.
authorizeSecurityGroupIngressResponse :: AuthorizeSecurityGroupIngressResponse
authorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse'
