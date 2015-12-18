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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [EC2-VPC only] Adds one or more egress rules to a security group for use
-- with a VPC. Specifically, this action permits instances to send traffic
-- to one or more destination CIDR IP address ranges, or to one or more
-- destination security groups for the same VPC. This action doesn\'t apply
-- to security groups for use in EC2-Classic. For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- You can have up to 50 rules per security group (covering both ingress
-- and egress rules).
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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupEgress.html AWS API Reference> for AuthorizeSecurityGroupEgress.
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
    , asgeCIdRIP
    , asgeSourceSecurityGroupOwnerId
    , asgeSourceSecurityGroupName
    , asgeDryRun
    , asgeGroupId

    -- * Destructuring the Response
    , authorizeSecurityGroupEgressResponse
    , AuthorizeSecurityGroupEgressResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'authorizeSecurityGroupEgress' smart constructor.
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
    { _asgeFromPort                   :: !(Maybe Int)
    , _asgeIPPermissions              :: !(Maybe [IPPermission])
    , _asgeIPProtocol                 :: !(Maybe Text)
    , _asgeToPort                     :: !(Maybe Int)
    , _asgeCIdRIP                     :: !(Maybe Text)
    , _asgeSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _asgeSourceSecurityGroupName    :: !(Maybe Text)
    , _asgeDryRun                     :: !(Maybe Bool)
    , _asgeGroupId                    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuthorizeSecurityGroupEgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgeFromPort'
--
-- * 'asgeIPPermissions'
--
-- * 'asgeIPProtocol'
--
-- * 'asgeToPort'
--
-- * 'asgeCIdRIP'
--
-- * 'asgeSourceSecurityGroupOwnerId'
--
-- * 'asgeSourceSecurityGroupName'
--
-- * 'asgeDryRun'
--
-- * 'asgeGroupId'
authorizeSecurityGroupEgress
    :: Text -- ^ 'asgeGroupId'
    -> AuthorizeSecurityGroupEgress
authorizeSecurityGroupEgress pGroupId_ =
    AuthorizeSecurityGroupEgress'
    { _asgeFromPort = Nothing
    , _asgeIPPermissions = Nothing
    , _asgeIPProtocol = Nothing
    , _asgeToPort = Nothing
    , _asgeCIdRIP = Nothing
    , _asgeSourceSecurityGroupOwnerId = Nothing
    , _asgeSourceSecurityGroupName = Nothing
    , _asgeDryRun = Nothing
    , _asgeGroupId = pGroupId_
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. We recommend that you specify the port range in a set of IP
-- permissions instead.
asgeFromPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgeFromPort = lens _asgeFromPort (\ s a -> s{_asgeFromPort = a});

-- | A set of IP permissions. You can\'t specify a destination security group
-- and a CIDR IP address range.
asgeIPPermissions :: Lens' AuthorizeSecurityGroupEgress [IPPermission]
asgeIPPermissions = lens _asgeIPPermissions (\ s a -> s{_asgeIPPermissions = a}) . _Default . _Coerce;

-- | The IP protocol name or number. We recommend that you specify the
-- protocol in a set of IP permissions instead.
asgeIPProtocol :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeIPProtocol = lens _asgeIPProtocol (\ s a -> s{_asgeIPProtocol = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP type
-- number. We recommend that you specify the port range in a set of IP
-- permissions instead.
asgeToPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgeToPort = lens _asgeToPort (\ s a -> s{_asgeToPort = a});

-- | The CIDR IP address range. We recommend that you specify the CIDR range
-- in a set of IP permissions instead.
asgeCIdRIP :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeCIdRIP = lens _asgeCIdRIP (\ s a -> s{_asgeCIdRIP = a});

-- | The AWS account number for a destination security group. To authorize
-- outbound access to a destination security group, we recommend that you
-- use a set of IP permissions instead.
asgeSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeSourceSecurityGroupOwnerId = lens _asgeSourceSecurityGroupOwnerId (\ s a -> s{_asgeSourceSecurityGroupOwnerId = a});

-- | The name of a destination security group. To authorize outbound access
-- to a destination security group, we recommend that you use a set of IP
-- permissions instead.
asgeSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeSourceSecurityGroupName = lens _asgeSourceSecurityGroupName (\ s a -> s{_asgeSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
asgeDryRun :: Lens' AuthorizeSecurityGroupEgress (Maybe Bool)
asgeDryRun = lens _asgeDryRun (\ s a -> s{_asgeDryRun = a});

-- | The ID of the security group.
asgeGroupId :: Lens' AuthorizeSecurityGroupEgress Text
asgeGroupId = lens _asgeGroupId (\ s a -> s{_asgeGroupId = a});

instance AWSRequest AuthorizeSecurityGroupEgress
         where
        type Rs AuthorizeSecurityGroupEgress =
             AuthorizeSecurityGroupEgressResponse
        request = postQuery eC2
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
               "Version" =: ("2015-10-01" :: ByteString),
               "FromPort" =: _asgeFromPort,
               toQuery
                 (toQueryList "IpPermissions" <$> _asgeIPPermissions),
               "IpProtocol" =: _asgeIPProtocol,
               "ToPort" =: _asgeToPort, "CidrIp" =: _asgeCIdRIP,
               "SourceSecurityGroupOwnerId" =:
                 _asgeSourceSecurityGroupOwnerId,
               "SourceSecurityGroupName" =:
                 _asgeSourceSecurityGroupName,
               "DryRun" =: _asgeDryRun, "GroupId" =: _asgeGroupId]

-- | /See:/ 'authorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse =
    AuthorizeSecurityGroupEgressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuthorizeSecurityGroupEgressResponse' with the minimum fields required to make a request.
--
authorizeSecurityGroupEgressResponse
    :: AuthorizeSecurityGroupEgressResponse
authorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'
