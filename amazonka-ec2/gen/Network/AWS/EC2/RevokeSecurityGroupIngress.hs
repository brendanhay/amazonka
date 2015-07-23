{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RevokeSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more ingress rules from a security group. The values that
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
    , rsgirqFromPort
    , rsgirqIPPermissions
    , rsgirqIPProtocol
    , rsgirqGroupId
    , rsgirqToPort
    , rsgirqCIdRIP
    , rsgirqGroupName
    , rsgirqSourceSecurityGroupOwnerId
    , rsgirqSourceSecurityGroupName
    , rsgirqDryRun

    -- * Response
    , RevokeSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeSecurityGroupIngressResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'revokeSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsgirqFromPort'
--
-- * 'rsgirqIPPermissions'
--
-- * 'rsgirqIPProtocol'
--
-- * 'rsgirqGroupId'
--
-- * 'rsgirqToPort'
--
-- * 'rsgirqCIdRIP'
--
-- * 'rsgirqGroupName'
--
-- * 'rsgirqSourceSecurityGroupOwnerId'
--
-- * 'rsgirqSourceSecurityGroupName'
--
-- * 'rsgirqDryRun'
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress'
    { _rsgirqFromPort                   :: !(Maybe Int)
    , _rsgirqIPPermissions              :: !(Maybe [IPPermission])
    , _rsgirqIPProtocol                 :: !(Maybe Text)
    , _rsgirqGroupId                    :: !(Maybe Text)
    , _rsgirqToPort                     :: !(Maybe Int)
    , _rsgirqCIdRIP                     :: !(Maybe Text)
    , _rsgirqGroupName                  :: !(Maybe Text)
    , _rsgirqSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _rsgirqSourceSecurityGroupName    :: !(Maybe Text)
    , _rsgirqDryRun                     :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeSecurityGroupIngress' smart constructor.
revokeSecurityGroupIngress :: RevokeSecurityGroupIngress
revokeSecurityGroupIngress =
    RevokeSecurityGroupIngress'
    { _rsgirqFromPort = Nothing
    , _rsgirqIPPermissions = Nothing
    , _rsgirqIPProtocol = Nothing
    , _rsgirqGroupId = Nothing
    , _rsgirqToPort = Nothing
    , _rsgirqCIdRIP = Nothing
    , _rsgirqGroupName = Nothing
    , _rsgirqSourceSecurityGroupOwnerId = Nothing
    , _rsgirqSourceSecurityGroupName = Nothing
    , _rsgirqDryRun = Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
rsgirqFromPort :: Lens' RevokeSecurityGroupIngress (Maybe Int)
rsgirqFromPort = lens _rsgirqFromPort (\ s a -> s{_rsgirqFromPort = a});

-- | A set of IP permissions. You can\'t specify a source security group and
-- a CIDR IP address range.
rsgirqIPPermissions :: Lens' RevokeSecurityGroupIngress [IPPermission]
rsgirqIPPermissions = lens _rsgirqIPPermissions (\ s a -> s{_rsgirqIPPermissions = a}) . _Default;

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- Use @-1@ to specify all.
rsgirqIPProtocol :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgirqIPProtocol = lens _rsgirqIPProtocol (\ s a -> s{_rsgirqIPProtocol = a});

-- | The ID of the security group.
rsgirqGroupId :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgirqGroupId = lens _rsgirqGroupId (\ s a -> s{_rsgirqGroupId = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
rsgirqToPort :: Lens' RevokeSecurityGroupIngress (Maybe Int)
rsgirqToPort = lens _rsgirqToPort (\ s a -> s{_rsgirqToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
rsgirqCIdRIP :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgirqCIdRIP = lens _rsgirqCIdRIP (\ s a -> s{_rsgirqCIdRIP = a});

-- | [EC2-Classic, default VPC] The name of the security group.
rsgirqGroupName :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgirqGroupName = lens _rsgirqGroupName (\ s a -> s{_rsgirqGroupName = a});

-- | The ID of the source security group. You can\'t specify this parameter
-- in combination with the following parameters: the CIDR IP address range,
-- the start of the port range, and the end of the port range.
rsgirqSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgirqSourceSecurityGroupOwnerId = lens _rsgirqSourceSecurityGroupOwnerId (\ s a -> s{_rsgirqSourceSecurityGroupOwnerId = a});

-- | [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, and
-- the end of the port range.
rsgirqSourceSecurityGroupName :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgirqSourceSecurityGroupName = lens _rsgirqSourceSecurityGroupName (\ s a -> s{_rsgirqSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsgirqDryRun :: Lens' RevokeSecurityGroupIngress (Maybe Bool)
rsgirqDryRun = lens _rsgirqDryRun (\ s a -> s{_rsgirqDryRun = a});

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
               "FromPort" =: _rsgirqFromPort,
               toQuery
                 (toQueryList "item" <$> _rsgirqIPPermissions),
               "IpProtocol" =: _rsgirqIPProtocol,
               "GroupId" =: _rsgirqGroupId,
               "ToPort" =: _rsgirqToPort, "CidrIp" =: _rsgirqCIdRIP,
               "GroupName" =: _rsgirqGroupName,
               "SourceSecurityGroupOwnerId" =:
                 _rsgirqSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _rsgirqSourceSecurityGroupName,
               "DryRun" =: _rsgirqDryRun]

-- | /See:/ 'revokeSecurityGroupIngressResponse' smart constructor.
data RevokeSecurityGroupIngressResponse =
    RevokeSecurityGroupIngressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeSecurityGroupIngressResponse' smart constructor.
revokeSecurityGroupIngressResponse :: RevokeSecurityGroupIngressResponse
revokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse'
