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
-- Module      : Network.AWS.EC2.RevokeSecurityGroupIngress
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more ingress rules from a security group. The values that you specify in the revoke request (for example, ports) must match the existing rule\'s values for the rule to be removed.
--
-- Each rule consists of the protocol and the CIDR range or source security group. For the TCP and UDP protocols, you must also specify the destination port or range of ports. For the ICMP protocol, you must also specify the ICMP type and code.
--
-- Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.
module Network.AWS.EC2.RevokeSecurityGroupIngress
    (
    -- * Creating a Request
      revokeSecurityGroupIngress
    , RevokeSecurityGroupIngress
    -- * Request Lenses
    , rsgiFromPort
    , rsgiIPPermissions
    , rsgiIPProtocol
    , rsgiGroupId
    , rsgiToPort
    , rsgiCIdRIP
    , rsgiSourceSecurityGroupOwnerId
    , rsgiGroupName
    , rsgiSourceSecurityGroupName
    , rsgiDryRun

    -- * Destructuring the Response
    , revokeSecurityGroupIngressResponse
    , RevokeSecurityGroupIngressResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RevokeSecurityGroupIngress.
--
-- /See:/ 'revokeSecurityGroupIngress' smart constructor.
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress'
    { _rsgiFromPort                   :: !(Maybe Int)
    , _rsgiIPPermissions              :: !(Maybe [IPPermission])
    , _rsgiIPProtocol                 :: !(Maybe Text)
    , _rsgiGroupId                    :: !(Maybe Text)
    , _rsgiToPort                     :: !(Maybe Int)
    , _rsgiCIdRIP                     :: !(Maybe Text)
    , _rsgiSourceSecurityGroupOwnerId :: !(Maybe Text)
    , _rsgiGroupName                  :: !(Maybe Text)
    , _rsgiSourceSecurityGroupName    :: !(Maybe Text)
    , _rsgiDryRun                     :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RevokeSecurityGroupIngress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
-- * 'rsgiCIdRIP'
--
-- * 'rsgiSourceSecurityGroupOwnerId'
--
-- * 'rsgiGroupName'
--
-- * 'rsgiSourceSecurityGroupName'
--
-- * 'rsgiDryRun'
revokeSecurityGroupIngress
    :: RevokeSecurityGroupIngress
revokeSecurityGroupIngress =
    RevokeSecurityGroupIngress'
    { _rsgiFromPort = Nothing
    , _rsgiIPPermissions = Nothing
    , _rsgiIPProtocol = Nothing
    , _rsgiGroupId = Nothing
    , _rsgiToPort = Nothing
    , _rsgiCIdRIP = Nothing
    , _rsgiSourceSecurityGroupOwnerId = Nothing
    , _rsgiGroupName = Nothing
    , _rsgiSourceSecurityGroupName = Nothing
    , _rsgiDryRun = Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use '-1' to specify all ICMP types.
rsgiFromPort :: Lens' RevokeSecurityGroupIngress (Maybe Int)
rsgiFromPort = lens _rsgiFromPort (\ s a -> s{_rsgiFromPort = a});

-- | A set of IP permissions. You can\'t specify a source security group and a CIDR IP address range.
rsgiIPPermissions :: Lens' RevokeSecurityGroupIngress [IPPermission]
rsgiIPPermissions = lens _rsgiIPPermissions (\ s a -> s{_rsgiIPPermissions = a}) . _Default . _Coerce;

-- | The IP protocol name ('tcp', 'udp', 'icmp') or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>). Use '-1' to specify all.
rsgiIPProtocol :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiIPProtocol = lens _rsgiIPProtocol (\ s a -> s{_rsgiIPProtocol = a});

-- | The ID of the security group. Required for a security group in a nondefault VPC.
rsgiGroupId :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiGroupId = lens _rsgiGroupId (\ s a -> s{_rsgiGroupId = a});

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use '-1' to specify all ICMP codes for the ICMP type.
rsgiToPort :: Lens' RevokeSecurityGroupIngress (Maybe Int)
rsgiToPort = lens _rsgiToPort (\ s a -> s{_rsgiToPort = a});

-- | The CIDR IP address range. You can\'t specify this parameter when specifying a source security group.
rsgiCIdRIP :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiCIdRIP = lens _rsgiCIdRIP (\ s a -> s{_rsgiCIdRIP = a});

-- | [EC2-Classic] The AWS account ID of the source security group, if the source security group is in a different account. You can\'t specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
rsgiSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiSourceSecurityGroupOwnerId = lens _rsgiSourceSecurityGroupOwnerId (\ s a -> s{_rsgiSourceSecurityGroupOwnerId = a});

-- | [EC2-Classic, default VPC] The name of the security group.
rsgiGroupName :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiGroupName = lens _rsgiGroupName (\ s a -> s{_rsgiGroupName = a});

-- | [EC2-Classic, default VPC] The name of the source security group. You can\'t specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. For EC2-VPC, the source security group must be in the same VPC. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
rsgiSourceSecurityGroupName :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiSourceSecurityGroupName = lens _rsgiSourceSecurityGroupName (\ s a -> s{_rsgiSourceSecurityGroupName = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
rsgiDryRun :: Lens' RevokeSecurityGroupIngress (Maybe Bool)
rsgiDryRun = lens _rsgiDryRun (\ s a -> s{_rsgiDryRun = a});

instance AWSRequest RevokeSecurityGroupIngress where
        type Rs RevokeSecurityGroupIngress =
             RevokeSecurityGroupIngressResponse
        request = postQuery ec2
        response
          = receiveNull RevokeSecurityGroupIngressResponse'

instance Hashable RevokeSecurityGroupIngress

instance NFData RevokeSecurityGroupIngress

instance ToHeaders RevokeSecurityGroupIngress where
        toHeaders = const mempty

instance ToPath RevokeSecurityGroupIngress where
        toPath = const "/"

instance ToQuery RevokeSecurityGroupIngress where
        toQuery RevokeSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("RevokeSecurityGroupIngress" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "FromPort" =: _rsgiFromPort,
               toQuery
                 (toQueryList "IpPermissions" <$> _rsgiIPPermissions),
               "IpProtocol" =: _rsgiIPProtocol,
               "GroupId" =: _rsgiGroupId, "ToPort" =: _rsgiToPort,
               "CidrIp" =: _rsgiCIdRIP,
               "SourceSecurityGroupOwnerId" =:
                 _rsgiSourceSecurityGroupOwnerId,
               "GroupName" =: _rsgiGroupName,
               "SourceSecurityGroupName" =:
                 _rsgiSourceSecurityGroupName,
               "DryRun" =: _rsgiDryRun]

-- | /See:/ 'revokeSecurityGroupIngressResponse' smart constructor.
data RevokeSecurityGroupIngressResponse =
    RevokeSecurityGroupIngressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RevokeSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
revokeSecurityGroupIngressResponse
    :: RevokeSecurityGroupIngressResponse
revokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse'

instance NFData RevokeSecurityGroupIngressResponse
