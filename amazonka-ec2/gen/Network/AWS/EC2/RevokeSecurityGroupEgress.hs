{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RevokeSecurityGroupEgress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupEgress.html>
module Network.AWS.EC2.RevokeSecurityGroupEgress
    (
    -- * Request
      RevokeSecurityGroupEgress
    -- ** Request constructor
    , revokeSecurityGroupEgress
    -- ** Request lenses
    , rsgerqFromPort
    , rsgerqIPPermissions
    , rsgerqIPProtocol
    , rsgerqToPort
    , rsgerqCIdRIP
    , rsgerqSourceSecurityGroupOwnerId
    , rsgerqSourceSecurityGroupName
    , rsgerqDryRun
    , rsgerqGroupId

    -- * Response
    , RevokeSecurityGroupEgressResponse
    -- ** Response constructor
    , revokeSecurityGroupEgressResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'revokeSecurityGroupEgress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsgerqFromPort'
--
-- * 'rsgerqIPPermissions'
--
-- * 'rsgerqIPProtocol'
--
-- * 'rsgerqToPort'
--
-- * 'rsgerqCIdRIP'
--
-- * 'rsgerqSourceSecurityGroupOwnerId'
--
-- * 'rsgerqSourceSecurityGroupName'
--
-- * 'rsgerqDryRun'
--
-- * 'rsgerqGroupId'
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress'
    { _rsgerqFromPort                   :: !(Maybe Int)
    , _rsgerqIPPermissions              :: !(Maybe [IPPermission])
    , _rsgerqIPProtocol                 :: !(Maybe Text)
    , _rsgerqToPort                     :: !(Maybe Int)
    , _rsgerqCIdRIP                     :: !(Maybe Text)
    , _rsgerqSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _rsgerqSourceSecurityGroupName    :: !(Maybe Text)
    , _rsgerqDryRun                     :: !(Maybe Bool)
    , _rsgerqGroupId                    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeSecurityGroupEgress' smart constructor.
revokeSecurityGroupEgress :: Text -> RevokeSecurityGroupEgress
revokeSecurityGroupEgress pGroupId_ =
    RevokeSecurityGroupEgress'
    { _rsgerqFromPort = Nothing
    , _rsgerqIPPermissions = Nothing
    , _rsgerqIPProtocol = Nothing
    , _rsgerqToPort = Nothing
    , _rsgerqCIdRIP = Nothing
    , _rsgerqSourceSecurityGroupOwnerId = Nothing
    , _rsgerqSourceSecurityGroupName = Nothing
    , _rsgerqDryRun = Nothing
    , _rsgerqGroupId = pGroupId_
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
rsgerqFromPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgerqFromPort = lens _rsgerqFromPort (\ s a -> s{_rsgerqFromPort = a});

-- | A set of IP permissions. You can\'t specify a destination security group
-- and a CIDR IP address range.
rsgerqIPPermissions :: Lens' RevokeSecurityGroupEgress [IPPermission]
rsgerqIPPermissions = lens _rsgerqIPPermissions (\ s a -> s{_rsgerqIPPermissions = a}) . _Default;

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- Use @-1@ to specify all.
rsgerqIPProtocol :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgerqIPProtocol = lens _rsgerqIPProtocol (\ s a -> s{_rsgerqIPProtocol = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
rsgerqToPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgerqToPort = lens _rsgerqToPort (\ s a -> s{_rsgerqToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
rsgerqCIdRIP :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgerqCIdRIP = lens _rsgerqCIdRIP (\ s a -> s{_rsgerqCIdRIP = a});

-- | The ID of the destination security group. You can\'t specify a
-- destination security group and a CIDR IP address range.
rsgerqSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgerqSourceSecurityGroupOwnerId = lens _rsgerqSourceSecurityGroupOwnerId (\ s a -> s{_rsgerqSourceSecurityGroupOwnerId = a});

-- | [EC2-Classic, default VPC] The name of the destination security group.
-- You can\'t specify a destination security group and a CIDR IP address
-- range.
rsgerqSourceSecurityGroupName :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgerqSourceSecurityGroupName = lens _rsgerqSourceSecurityGroupName (\ s a -> s{_rsgerqSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsgerqDryRun :: Lens' RevokeSecurityGroupEgress (Maybe Bool)
rsgerqDryRun = lens _rsgerqDryRun (\ s a -> s{_rsgerqDryRun = a});

-- | The ID of the security group.
rsgerqGroupId :: Lens' RevokeSecurityGroupEgress Text
rsgerqGroupId = lens _rsgerqGroupId (\ s a -> s{_rsgerqGroupId = a});

instance AWSRequest RevokeSecurityGroupEgress where
        type Sv RevokeSecurityGroupEgress = EC2
        type Rs RevokeSecurityGroupEgress =
             RevokeSecurityGroupEgressResponse
        request = post
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
               "FromPort" =: _rsgerqFromPort,
               toQuery
                 (toQueryList "item" <$> _rsgerqIPPermissions),
               "IpProtocol" =: _rsgerqIPProtocol,
               "ToPort" =: _rsgerqToPort, "CidrIp" =: _rsgerqCIdRIP,
               "SourceSecurityGroupOwnerId" =:
                 _rsgerqSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _rsgerqSourceSecurityGroupName,
               "DryRun" =: _rsgerqDryRun,
               "GroupId" =: _rsgerqGroupId]

-- | /See:/ 'revokeSecurityGroupEgressResponse' smart constructor.
data RevokeSecurityGroupEgressResponse =
    RevokeSecurityGroupEgressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeSecurityGroupEgressResponse' smart constructor.
revokeSecurityGroupEgressResponse :: RevokeSecurityGroupEgressResponse
revokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse'
