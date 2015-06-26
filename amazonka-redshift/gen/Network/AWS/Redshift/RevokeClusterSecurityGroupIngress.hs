{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
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

-- | Revokes an ingress rule in an Amazon Redshift security group for a
-- previously authorized IP range or Amazon EC2 security group. To add an
-- ingress rule, see AuthorizeClusterSecurityGroupIngress. For information
-- about managing security groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_RevokeClusterSecurityGroupIngress.html>
module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
    (
    -- * Request
      RevokeClusterSecurityGroupIngress
    -- ** Request constructor
    , revokeClusterSecurityGroupIngress
    -- ** Request lenses
    , rcsgiEC2SecurityGroupOwnerId
    , rcsgiEC2SecurityGroupName
    , rcsgiCIDRIP
    , rcsgiClusterSecurityGroupName

    -- * Response
    , RevokeClusterSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , rcsgirClusterSecurityGroup
    , rcsgirStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | ???
--
-- /See:/ 'revokeClusterSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgiEC2SecurityGroupOwnerId'
--
-- * 'rcsgiEC2SecurityGroupName'
--
-- * 'rcsgiCIDRIP'
--
-- * 'rcsgiClusterSecurityGroupName'
data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress'{_rcsgiEC2SecurityGroupOwnerId :: Maybe Text, _rcsgiEC2SecurityGroupName :: Maybe Text, _rcsgiCIDRIP :: Maybe Text, _rcsgiClusterSecurityGroupName :: Text} deriving (Eq, Read, Show)

-- | 'RevokeClusterSecurityGroupIngress' smart constructor.
revokeClusterSecurityGroupIngress :: Text -> RevokeClusterSecurityGroupIngress
revokeClusterSecurityGroupIngress pClusterSecurityGroupName = RevokeClusterSecurityGroupIngress'{_rcsgiEC2SecurityGroupOwnerId = Nothing, _rcsgiEC2SecurityGroupName = Nothing, _rcsgiCIDRIP = Nothing, _rcsgiClusterSecurityGroupName = pClusterSecurityGroupName};

-- | The AWS account number of the owner of the security group specified in
-- the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an
-- acceptable value. If @EC2SecurityGroupOwnerId@ is specified,
-- @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be
-- provided.
--
-- Example: @111122223333@
rcsgiEC2SecurityGroupOwnerId :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiEC2SecurityGroupOwnerId = lens _rcsgiEC2SecurityGroupOwnerId (\ s a -> s{_rcsgiEC2SecurityGroupOwnerId = a});

-- | The name of the EC2 Security Group whose access is to be revoked. If
-- @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also
-- be provided and @CIDRIP@ cannot be provided.
rcsgiEC2SecurityGroupName :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiEC2SecurityGroupName = lens _rcsgiEC2SecurityGroupName (\ s a -> s{_rcsgiEC2SecurityGroupName = a});

-- | The IP range for which to revoke access. This range must be a valid
-- Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@
-- is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@
-- cannot be provided.
rcsgiCIDRIP :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiCIDRIP = lens _rcsgiCIDRIP (\ s a -> s{_rcsgiCIDRIP = a});

-- | The name of the security Group from which to revoke the ingress rule.
rcsgiClusterSecurityGroupName :: Lens' RevokeClusterSecurityGroupIngress Text
rcsgiClusterSecurityGroupName = lens _rcsgiClusterSecurityGroupName (\ s a -> s{_rcsgiClusterSecurityGroupName = a});

instance AWSRequest RevokeClusterSecurityGroupIngress
         where
        type Sv RevokeClusterSecurityGroupIngress = Redshift
        type Rs RevokeClusterSecurityGroupIngress =
             RevokeClusterSecurityGroupIngressResponse
        request = post
        response
          = receiveXMLWrapper
              "RevokeClusterSecurityGroupIngressResult"
              (\ s h x ->
                 RevokeClusterSecurityGroupIngressResponse' <$>
                   (x .@? "ClusterSecurityGroup") <*>
                     (pure (fromEnum s)))

instance ToHeaders RevokeClusterSecurityGroupIngress
         where
        toHeaders = const mempty

instance ToPath RevokeClusterSecurityGroupIngress
         where
        toPath = const "/"

instance ToQuery RevokeClusterSecurityGroupIngress
         where
        toQuery RevokeClusterSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("RevokeClusterSecurityGroupIngress" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "EC2SecurityGroupOwnerId" =:
                 _rcsgiEC2SecurityGroupOwnerId,
               "EC2SecurityGroupName" =: _rcsgiEC2SecurityGroupName,
               "CIDRIP" =: _rcsgiCIDRIP,
               "ClusterSecurityGroupName" =:
                 _rcsgiClusterSecurityGroupName]

-- | /See:/ 'revokeClusterSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgirClusterSecurityGroup'
--
-- * 'rcsgirStatusCode'
data RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse'{_rcsgirClusterSecurityGroup :: Maybe ClusterSecurityGroup, _rcsgirStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'RevokeClusterSecurityGroupIngressResponse' smart constructor.
revokeClusterSecurityGroupIngressResponse :: Int -> RevokeClusterSecurityGroupIngressResponse
revokeClusterSecurityGroupIngressResponse pStatusCode = RevokeClusterSecurityGroupIngressResponse'{_rcsgirClusterSecurityGroup = Nothing, _rcsgirStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
rcsgirClusterSecurityGroup :: Lens' RevokeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
rcsgirClusterSecurityGroup = lens _rcsgirClusterSecurityGroup (\ s a -> s{_rcsgirClusterSecurityGroup = a});

-- | FIXME: Undocumented member.
rcsgirStatusCode :: Lens' RevokeClusterSecurityGroupIngressResponse Int
rcsgirStatusCode = lens _rcsgirStatusCode (\ s a -> s{_rcsgirStatusCode = a});
