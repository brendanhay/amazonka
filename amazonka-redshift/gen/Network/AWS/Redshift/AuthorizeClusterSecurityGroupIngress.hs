{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Adds an inbound (ingress) rule to an Amazon Redshift security group.
-- Depending on whether the application accessing your cluster is running
-- on the Internet or an EC2 instance, you can authorize inbound access to
-- either a Classless Interdomain Routing (CIDR) IP address range or an EC2
-- security group. You can add as many as 20 ingress rules to an Amazon
-- Redshift security group.
--
-- The EC2 security group must be defined in the AWS region where the
-- cluster resides.
--
-- For an overview of CIDR blocks, see the Wikipedia article on
-- <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- You must also associate the security group with a cluster so that
-- clients running on these IP addresses or the EC2 instance are authorized
-- to connect to the cluster. For information about managing security
-- groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Working with Security Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_AuthorizeClusterSecurityGroupIngress.html>
module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
    (
    -- * Request
      AuthorizeClusterSecurityGroupIngress
    -- ** Request constructor
    , authorizeClusterSecurityGroupIngress
    -- ** Request lenses
    , acsgiEC2SecurityGroupOwnerId
    , acsgiEC2SecurityGroupName
    , acsgiCIDRIP
    , acsgiClusterSecurityGroupName

    -- * Response
    , AuthorizeClusterSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , acsgirClusterSecurityGroup
    , acsgirStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | ???
--
-- /See:/ 'authorizeClusterSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgiEC2SecurityGroupOwnerId'
--
-- * 'acsgiEC2SecurityGroupName'
--
-- * 'acsgiCIDRIP'
--
-- * 'acsgiClusterSecurityGroupName'
data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress'
    { _acsgiEC2SecurityGroupOwnerId  :: !(Maybe Text)
    , _acsgiEC2SecurityGroupName     :: !(Maybe Text)
    , _acsgiCIDRIP                   :: !(Maybe Text)
    , _acsgiClusterSecurityGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeClusterSecurityGroupIngress' smart constructor.
authorizeClusterSecurityGroupIngress :: Text -> AuthorizeClusterSecurityGroupIngress
authorizeClusterSecurityGroupIngress pClusterSecurityGroupName =
    AuthorizeClusterSecurityGroupIngress'
    { _acsgiEC2SecurityGroupOwnerId = Nothing
    , _acsgiEC2SecurityGroupName = Nothing
    , _acsgiCIDRIP = Nothing
    , _acsgiClusterSecurityGroupName = pClusterSecurityGroupName
    }

-- | The AWS account number of the owner of the security group specified by
-- the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an
-- acceptable value.
--
-- Example: @111122223333@
acsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiEC2SecurityGroupOwnerId = lens _acsgiEC2SecurityGroupOwnerId (\ s a -> s{_acsgiEC2SecurityGroupOwnerId = a});

-- | The EC2 security group to be added the Amazon Redshift security group.
acsgiEC2SecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiEC2SecurityGroupName = lens _acsgiEC2SecurityGroupName (\ s a -> s{_acsgiEC2SecurityGroupName = a});

-- | The IP range to be added the Amazon Redshift security group.
acsgiCIDRIP :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiCIDRIP = lens _acsgiCIDRIP (\ s a -> s{_acsgiCIDRIP = a});

-- | The name of the security group to which the ingress rule is added.
acsgiClusterSecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress Text
acsgiClusterSecurityGroupName = lens _acsgiClusterSecurityGroupName (\ s a -> s{_acsgiClusterSecurityGroupName = a});

instance AWSRequest
         AuthorizeClusterSecurityGroupIngress where
        type Sv AuthorizeClusterSecurityGroupIngress =
             Redshift
        type Rs AuthorizeClusterSecurityGroupIngress =
             AuthorizeClusterSecurityGroupIngressResponse
        request = post
        response
          = receiveXMLWrapper
              "AuthorizeClusterSecurityGroupIngressResult"
              (\ s h x ->
                 AuthorizeClusterSecurityGroupIngressResponse' <$>
                   (x .@? "ClusterSecurityGroup") <*>
                     (pure (fromEnum s)))

instance ToHeaders
         AuthorizeClusterSecurityGroupIngress where
        toHeaders = const mempty

instance ToPath AuthorizeClusterSecurityGroupIngress
         where
        toPath = const "/"

instance ToQuery AuthorizeClusterSecurityGroupIngress
         where
        toQuery AuthorizeClusterSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("AuthorizeClusterSecurityGroupIngress" ::
                    ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "EC2SecurityGroupOwnerId" =:
                 _acsgiEC2SecurityGroupOwnerId,
               "EC2SecurityGroupName" =: _acsgiEC2SecurityGroupName,
               "CIDRIP" =: _acsgiCIDRIP,
               "ClusterSecurityGroupName" =:
                 _acsgiClusterSecurityGroupName]

-- | /See:/ 'authorizeClusterSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgirClusterSecurityGroup'
--
-- * 'acsgirStatus'
data AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse'
    { _acsgirClusterSecurityGroup :: !(Maybe ClusterSecurityGroup)
    , _acsgirStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeClusterSecurityGroupIngressResponse' smart constructor.
authorizeClusterSecurityGroupIngressResponse :: Int -> AuthorizeClusterSecurityGroupIngressResponse
authorizeClusterSecurityGroupIngressResponse pStatus =
    AuthorizeClusterSecurityGroupIngressResponse'
    { _acsgirClusterSecurityGroup = Nothing
    , _acsgirStatus = pStatus
    }

-- | FIXME: Undocumented member.
acsgirClusterSecurityGroup :: Lens' AuthorizeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
acsgirClusterSecurityGroup = lens _acsgirClusterSecurityGroup (\ s a -> s{_acsgirClusterSecurityGroup = a});

-- | FIXME: Undocumented member.
acsgirStatus :: Lens' AuthorizeClusterSecurityGroupIngressResponse Int
acsgirStatus = lens _acsgirStatus (\ s a -> s{_acsgirStatus = a});
