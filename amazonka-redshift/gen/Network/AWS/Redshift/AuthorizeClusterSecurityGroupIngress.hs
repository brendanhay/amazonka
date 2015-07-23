{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds an inbound (ingress) rule to an Amazon Redshift security group.
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
    , acsgirqEC2SecurityGroupOwnerId
    , acsgirqEC2SecurityGroupName
    , acsgirqCIdRIP
    , acsgirqClusterSecurityGroupName

    -- * Response
    , AuthorizeClusterSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , acsgirsClusterSecurityGroup
    , acsgirsStatus
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
-- * 'acsgirqEC2SecurityGroupOwnerId'
--
-- * 'acsgirqEC2SecurityGroupName'
--
-- * 'acsgirqCIdRIP'
--
-- * 'acsgirqClusterSecurityGroupName'
data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress'
    { _acsgirqEC2SecurityGroupOwnerId  :: !(Maybe Text)
    , _acsgirqEC2SecurityGroupName     :: !(Maybe Text)
    , _acsgirqCIdRIP                   :: !(Maybe Text)
    , _acsgirqClusterSecurityGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeClusterSecurityGroupIngress' smart constructor.
authorizeClusterSecurityGroupIngress :: Text -> AuthorizeClusterSecurityGroupIngress
authorizeClusterSecurityGroupIngress pClusterSecurityGroupName_ =
    AuthorizeClusterSecurityGroupIngress'
    { _acsgirqEC2SecurityGroupOwnerId = Nothing
    , _acsgirqEC2SecurityGroupName = Nothing
    , _acsgirqCIdRIP = Nothing
    , _acsgirqClusterSecurityGroupName = pClusterSecurityGroupName_
    }

-- | The AWS account number of the owner of the security group specified by
-- the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an
-- acceptable value.
--
-- Example: @111122223333@
acsgirqEC2SecurityGroupOwnerId :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgirqEC2SecurityGroupOwnerId = lens _acsgirqEC2SecurityGroupOwnerId (\ s a -> s{_acsgirqEC2SecurityGroupOwnerId = a});

-- | The EC2 security group to be added the Amazon Redshift security group.
acsgirqEC2SecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgirqEC2SecurityGroupName = lens _acsgirqEC2SecurityGroupName (\ s a -> s{_acsgirqEC2SecurityGroupName = a});

-- | The IP range to be added the Amazon Redshift security group.
acsgirqCIdRIP :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgirqCIdRIP = lens _acsgirqCIdRIP (\ s a -> s{_acsgirqCIdRIP = a});

-- | The name of the security group to which the ingress rule is added.
acsgirqClusterSecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress Text
acsgirqClusterSecurityGroupName = lens _acsgirqClusterSecurityGroupName (\ s a -> s{_acsgirqClusterSecurityGroupName = a});

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
                 _acsgirqEC2SecurityGroupOwnerId,
               "EC2SecurityGroupName" =:
                 _acsgirqEC2SecurityGroupName,
               "CIDRIP" =: _acsgirqCIdRIP,
               "ClusterSecurityGroupName" =:
                 _acsgirqClusterSecurityGroupName]

-- | /See:/ 'authorizeClusterSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgirsClusterSecurityGroup'
--
-- * 'acsgirsStatus'
data AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse'
    { _acsgirsClusterSecurityGroup :: !(Maybe ClusterSecurityGroup)
    , _acsgirsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeClusterSecurityGroupIngressResponse' smart constructor.
authorizeClusterSecurityGroupIngressResponse :: Int -> AuthorizeClusterSecurityGroupIngressResponse
authorizeClusterSecurityGroupIngressResponse pStatus_ =
    AuthorizeClusterSecurityGroupIngressResponse'
    { _acsgirsClusterSecurityGroup = Nothing
    , _acsgirsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
acsgirsClusterSecurityGroup :: Lens' AuthorizeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
acsgirsClusterSecurityGroup = lens _acsgirsClusterSecurityGroup (\ s a -> s{_acsgirsClusterSecurityGroup = a});

-- | FIXME: Undocumented member.
acsgirsStatus :: Lens' AuthorizeClusterSecurityGroupIngressResponse Int
acsgirsStatus = lens _acsgirsStatus (\ s a -> s{_acsgirsStatus = a});
