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
-- Module      : Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_AuthorizeClusterSecurityGroupIngress.html AWS API Reference> for AuthorizeClusterSecurityGroupIngress.
module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
    (
    -- * Creating a Request
      AuthorizeClusterSecurityGroupIngress
    , authorizeClusterSecurityGroupIngress
    -- * Request Lenses
    , acsgiEC2SecurityGroupOwnerId
    , acsgiEC2SecurityGroupName
    , acsgiCIdRIP
    , acsgiClusterSecurityGroupName

    -- * Destructuring the Response
    , AuthorizeClusterSecurityGroupIngressResponse
    , authorizeClusterSecurityGroupIngressResponse
    -- * Response Lenses
    , acsgirsClusterSecurityGroup
    , acsgirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
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
-- * 'acsgiCIdRIP'
--
-- * 'acsgiClusterSecurityGroupName'
data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress'
    { _acsgiEC2SecurityGroupOwnerId  :: !(Maybe Text)
    , _acsgiEC2SecurityGroupName     :: !(Maybe Text)
    , _acsgiCIdRIP                   :: !(Maybe Text)
    , _acsgiClusterSecurityGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeClusterSecurityGroupIngress' smart constructor.
authorizeClusterSecurityGroupIngress :: Text -> AuthorizeClusterSecurityGroupIngress
authorizeClusterSecurityGroupIngress pClusterSecurityGroupName_ =
    AuthorizeClusterSecurityGroupIngress'
    { _acsgiEC2SecurityGroupOwnerId = Nothing
    , _acsgiEC2SecurityGroupName = Nothing
    , _acsgiCIdRIP = Nothing
    , _acsgiClusterSecurityGroupName = pClusterSecurityGroupName_
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
acsgiCIdRIP :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiCIdRIP = lens _acsgiCIdRIP (\ s a -> s{_acsgiCIdRIP = a});

-- | The name of the security group to which the ingress rule is added.
acsgiClusterSecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress Text
acsgiClusterSecurityGroupName = lens _acsgiClusterSecurityGroupName (\ s a -> s{_acsgiClusterSecurityGroupName = a});

instance AWSRequest
         AuthorizeClusterSecurityGroupIngress where
        type Sv AuthorizeClusterSecurityGroupIngress =
             Redshift
        type Rs AuthorizeClusterSecurityGroupIngress =
             AuthorizeClusterSecurityGroupIngressResponse
        request = postQuery
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
               "CIDRIP" =: _acsgiCIdRIP,
               "ClusterSecurityGroupName" =:
                 _acsgiClusterSecurityGroupName]

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

-- | Undocumented member.
acsgirsClusterSecurityGroup :: Lens' AuthorizeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
acsgirsClusterSecurityGroup = lens _acsgirsClusterSecurityGroup (\ s a -> s{_acsgirsClusterSecurityGroup = a});

-- | Undocumented member.
acsgirsStatus :: Lens' AuthorizeClusterSecurityGroupIngressResponse Int
acsgirsStatus = lens _acsgirsStatus (\ s a -> s{_acsgirsStatus = a});
