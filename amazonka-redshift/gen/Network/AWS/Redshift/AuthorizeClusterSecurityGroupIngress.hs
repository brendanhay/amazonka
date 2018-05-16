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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an inbound (ingress) rule to an Amazon Redshift security group. Depending on whether the application accessing your cluster is running on the Internet or an Amazon EC2 instance, you can authorize inbound access to either a Classless Interdomain Routing (CIDR)/Internet Protocol (IP) range or to an Amazon EC2 security group. You can add as many as 20 ingress rules to an Amazon Redshift security group.
--
--
-- If you authorize access to an Amazon EC2 security group, specify /EC2SecurityGroupName/ and /EC2SecurityGroupOwnerId/ . The Amazon EC2 security group and Amazon Redshift cluster must be in the same AWS region.
--
-- If you authorize access to a CIDR/IP address range, specify /CIDRIP/ . For an overview of CIDR blocks, see the Wikipedia article on <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
--
-- You must also associate the security group with a cluster so that clients running on these IP addresses or the EC2 instance are authorized to connect to the cluster. For information about managing security groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Working with Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
    (
    -- * Creating a Request
      authorizeClusterSecurityGroupIngress
    , AuthorizeClusterSecurityGroupIngress
    -- * Request Lenses
    , acsgiEC2SecurityGroupOwnerId
    , acsgiEC2SecurityGroupName
    , acsgiCIdRIP
    , acsgiClusterSecurityGroupName

    -- * Destructuring the Response
    , authorizeClusterSecurityGroupIngressResponse
    , AuthorizeClusterSecurityGroupIngressResponse
    -- * Response Lenses
    , acsgirsClusterSecurityGroup
    , acsgirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'authorizeClusterSecurityGroupIngress' smart constructor.
data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress'
  { _acsgiEC2SecurityGroupOwnerId  :: !(Maybe Text)
  , _acsgiEC2SecurityGroupName     :: !(Maybe Text)
  , _acsgiCIdRIP                   :: !(Maybe Text)
  , _acsgiClusterSecurityGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeClusterSecurityGroupIngress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsgiEC2SecurityGroupOwnerId' - The AWS account number of the owner of the security group specified by the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an acceptable value.  Example: @111122223333@
--
-- * 'acsgiEC2SecurityGroupName' - The EC2 security group to be added the Amazon Redshift security group.
--
-- * 'acsgiCIdRIP' - The IP range to be added the Amazon Redshift security group.
--
-- * 'acsgiClusterSecurityGroupName' - The name of the security group to which the ingress rule is added.
authorizeClusterSecurityGroupIngress
    :: Text -- ^ 'acsgiClusterSecurityGroupName'
    -> AuthorizeClusterSecurityGroupIngress
authorizeClusterSecurityGroupIngress pClusterSecurityGroupName_ =
  AuthorizeClusterSecurityGroupIngress'
    { _acsgiEC2SecurityGroupOwnerId = Nothing
    , _acsgiEC2SecurityGroupName = Nothing
    , _acsgiCIdRIP = Nothing
    , _acsgiClusterSecurityGroupName = pClusterSecurityGroupName_
    }


-- | The AWS account number of the owner of the security group specified by the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an acceptable value.  Example: @111122223333@
acsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiEC2SecurityGroupOwnerId = lens _acsgiEC2SecurityGroupOwnerId (\ s a -> s{_acsgiEC2SecurityGroupOwnerId = a})

-- | The EC2 security group to be added the Amazon Redshift security group.
acsgiEC2SecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiEC2SecurityGroupName = lens _acsgiEC2SecurityGroupName (\ s a -> s{_acsgiEC2SecurityGroupName = a})

-- | The IP range to be added the Amazon Redshift security group.
acsgiCIdRIP :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiCIdRIP = lens _acsgiCIdRIP (\ s a -> s{_acsgiCIdRIP = a})

-- | The name of the security group to which the ingress rule is added.
acsgiClusterSecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress Text
acsgiClusterSecurityGroupName = lens _acsgiClusterSecurityGroupName (\ s a -> s{_acsgiClusterSecurityGroupName = a})

instance AWSRequest
           AuthorizeClusterSecurityGroupIngress
         where
        type Rs AuthorizeClusterSecurityGroupIngress =
             AuthorizeClusterSecurityGroupIngressResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "AuthorizeClusterSecurityGroupIngressResult"
              (\ s h x ->
                 AuthorizeClusterSecurityGroupIngressResponse' <$>
                   (x .@? "ClusterSecurityGroup") <*>
                     (pure (fromEnum s)))

instance Hashable
           AuthorizeClusterSecurityGroupIngress
         where

instance NFData AuthorizeClusterSecurityGroupIngress
         where

instance ToHeaders
           AuthorizeClusterSecurityGroupIngress
         where
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
data AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse'
  { _acsgirsClusterSecurityGroup :: !(Maybe ClusterSecurityGroup)
  , _acsgirsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeClusterSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsgirsClusterSecurityGroup' - Undocumented member.
--
-- * 'acsgirsResponseStatus' - -- | The response status code.
authorizeClusterSecurityGroupIngressResponse
    :: Int -- ^ 'acsgirsResponseStatus'
    -> AuthorizeClusterSecurityGroupIngressResponse
authorizeClusterSecurityGroupIngressResponse pResponseStatus_ =
  AuthorizeClusterSecurityGroupIngressResponse'
    { _acsgirsClusterSecurityGroup = Nothing
    , _acsgirsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
acsgirsClusterSecurityGroup :: Lens' AuthorizeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
acsgirsClusterSecurityGroup = lens _acsgirsClusterSecurityGroup (\ s a -> s{_acsgirsClusterSecurityGroup = a})

-- | -- | The response status code.
acsgirsResponseStatus :: Lens' AuthorizeClusterSecurityGroupIngressResponse Int
acsgirsResponseStatus = lens _acsgirsResponseStatus (\ s a -> s{_acsgirsResponseStatus = a})

instance NFData
           AuthorizeClusterSecurityGroupIngressResponse
         where
