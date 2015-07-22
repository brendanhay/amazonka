{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupEgress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more egress rules to a security group for use with a VPC.
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
    , asgerqFromPort
    , asgerqIPPermissions
    , asgerqIPProtocol
    , asgerqToPort
    , asgerqCIdRIP
    , asgerqSourceSecurityGroupOwnerId
    , asgerqSourceSecurityGroupName
    , asgerqDryRun
    , asgerqGroupId

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
-- * 'asgerqFromPort'
--
-- * 'asgerqIPPermissions'
--
-- * 'asgerqIPProtocol'
--
-- * 'asgerqToPort'
--
-- * 'asgerqCIdRIP'
--
-- * 'asgerqSourceSecurityGroupOwnerId'
--
-- * 'asgerqSourceSecurityGroupName'
--
-- * 'asgerqDryRun'
--
-- * 'asgerqGroupId'
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
    { _asgerqFromPort                   :: !(Maybe Int)
    , _asgerqIPPermissions              :: !(Maybe [IPPermission])
    , _asgerqIPProtocol                 :: !(Maybe Text)
    , _asgerqToPort                     :: !(Maybe Int)
    , _asgerqCIdRIP                     :: !(Maybe Text)
    , _asgerqSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _asgerqSourceSecurityGroupName    :: !(Maybe Text)
    , _asgerqDryRun                     :: !(Maybe Bool)
    , _asgerqGroupId                    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeSecurityGroupEgress' smart constructor.
authorizeSecurityGroupEgress :: Text -> AuthorizeSecurityGroupEgress
authorizeSecurityGroupEgress pGroupId =
    AuthorizeSecurityGroupEgress'
    { _asgerqFromPort = Nothing
    , _asgerqIPPermissions = Nothing
    , _asgerqIPProtocol = Nothing
    , _asgerqToPort = Nothing
    , _asgerqCIdRIP = Nothing
    , _asgerqSourceSecurityGroupOwnerId = Nothing
    , _asgerqSourceSecurityGroupName = Nothing
    , _asgerqDryRun = Nothing
    , _asgerqGroupId = pGroupId
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
asgerqFromPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgerqFromPort = lens _asgerqFromPort (\ s a -> s{_asgerqFromPort = a});

-- | A set of IP permissions. You can\'t specify a destination security group
-- and a CIDR IP address range.
asgerqIPPermissions :: Lens' AuthorizeSecurityGroupEgress [IPPermission]
asgerqIPPermissions = lens _asgerqIPPermissions (\ s a -> s{_asgerqIPPermissions = a}) . _Default;

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- Use @-1@ to specify all.
asgerqIPProtocol :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgerqIPProtocol = lens _asgerqIPProtocol (\ s a -> s{_asgerqIPProtocol = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
asgerqToPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgerqToPort = lens _asgerqToPort (\ s a -> s{_asgerqToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
asgerqCIdRIP :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgerqCIdRIP = lens _asgerqCIdRIP (\ s a -> s{_asgerqCIdRIP = a});

-- | The ID of the destination security group. You can\'t specify a
-- destination security group and a CIDR IP address range.
asgerqSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgerqSourceSecurityGroupOwnerId = lens _asgerqSourceSecurityGroupOwnerId (\ s a -> s{_asgerqSourceSecurityGroupOwnerId = a});

-- | [EC2-Classic, default VPC] The name of the destination security group.
-- You can\'t specify a destination security group and a CIDR IP address
-- range.
asgerqSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgerqSourceSecurityGroupName = lens _asgerqSourceSecurityGroupName (\ s a -> s{_asgerqSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
asgerqDryRun :: Lens' AuthorizeSecurityGroupEgress (Maybe Bool)
asgerqDryRun = lens _asgerqDryRun (\ s a -> s{_asgerqDryRun = a});

-- | The ID of the security group.
asgerqGroupId :: Lens' AuthorizeSecurityGroupEgress Text
asgerqGroupId = lens _asgerqGroupId (\ s a -> s{_asgerqGroupId = a});

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
               "FromPort" =: _asgerqFromPort,
               toQuery
                 (toQueryList "item" <$> _asgerqIPPermissions),
               "IpProtocol" =: _asgerqIPProtocol,
               "ToPort" =: _asgerqToPort, "CidrIp" =: _asgerqCIdRIP,
               "SourceSecurityGroupOwnerId" =:
                 _asgerqSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _asgerqSourceSecurityGroupName,
               "DryRun" =: _asgerqDryRun,
               "GroupId" =: _asgerqGroupId]

-- | /See:/ 'authorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse =
    AuthorizeSecurityGroupEgressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeSecurityGroupEgressResponse' smart constructor.
authorizeSecurityGroupEgressResponse :: AuthorizeSecurityGroupEgressResponse
authorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'
