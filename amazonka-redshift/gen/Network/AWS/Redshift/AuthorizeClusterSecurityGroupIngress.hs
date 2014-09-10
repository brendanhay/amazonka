{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds an inbound (ingress) rule to an Amazon Redshift security group.
-- Depending on whether the application accessing your cluster is running on
-- the Internet or an EC2 instance, you can authorize inbound access to either
-- a Classless Interdomain Routing (CIDR) IP address range or an EC2 security
-- group. EC2SecurityGroupName and EC2SecurityGroupOwnerId --> You can add as
-- many as 20 ingress rules to an Amazon Redshift security group. The EC2
-- security group must be defined in the AWS region where the cluster resides.
-- For an overview of CIDR blocks, see the Wikipedia article on Classless
-- Inter-Domain Routing. You must also associate the security group with a
-- cluster so that clients running on these IP addresses or the EC2 instance
-- are authorized to connect to the cluster. For information about managing
-- security groups, go to Working with Security Groups in the Amazon Redshift
-- Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=AuthorizeClusterSecurityGroupIngress &CIDRIP=192.168.40.3/32
-- &ClusterSecurityGroupName=securitygroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T020649Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 192.168.40.3/32
-- authorized my security group securitygroup1
-- 8c7cd4c8-6501-11e2-a8da-655adc216806.
module Network.AWS.Redshift
    (
    -- * Request
      AuthorizeClusterSecurityGroupIngress
    -- ** Request constructor
    , mkAuthorizeClusterSecurityGroupIngress
    -- ** Request lenses
    , acsgiClusterSecurityGroupName
    , acsgiCIDRIP
    , acsgiEC2SecurityGroupName
    , acsgiEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeClusterSecurityGroupIngressResponse
    -- ** Response constructor
    , mkAuthorizeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , acsgirClusterSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | ???.
data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress
    { _acsgiClusterSecurityGroupName :: !Text
    , _acsgiCIDRIP :: !(Maybe Text)
    , _acsgiEC2SecurityGroupName :: !(Maybe Text)
    , _acsgiEC2SecurityGroupOwnerId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeClusterSecurityGroupIngress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSecurityGroupName ::@ @Text@
--
-- * @CIDRIP ::@ @Maybe Text@
--
-- * @EC2SecurityGroupName ::@ @Maybe Text@
--
-- * @EC2SecurityGroupOwnerId ::@ @Maybe Text@
--
mkAuthorizeClusterSecurityGroupIngress :: Text -- ^ 'acsgiClusterSecurityGroupName'
                                       -> AuthorizeClusterSecurityGroupIngress
mkAuthorizeClusterSecurityGroupIngress p1 = AuthorizeClusterSecurityGroupIngress
    { _acsgiClusterSecurityGroupName = p1
    , _acsgiCIDRIP = Nothing
    , _acsgiEC2SecurityGroupName = Nothing
    , _acsgiEC2SecurityGroupOwnerId = Nothing
    }

-- | The name of the security group to which the ingress rule is added.
acsgiClusterSecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress Text
acsgiClusterSecurityGroupName =
    lens _acsgiClusterSecurityGroupName
         (\s a -> s { _acsgiClusterSecurityGroupName = a })

-- | The IP range to be added the Amazon Redshift security group.
acsgiCIDRIP :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiCIDRIP = lens _acsgiCIDRIP (\s a -> s { _acsgiCIDRIP = a })

-- | The EC2 security group to be added the Amazon Redshift security group.
acsgiEC2SecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiEC2SecurityGroupName =
    lens _acsgiEC2SecurityGroupName
         (\s a -> s { _acsgiEC2SecurityGroupName = a })

-- | The AWS account number of the owner of the security group specified by the
-- EC2SecurityGroupName parameter. The AWS Access Key ID is not an acceptable
-- value. Example: 111122223333.
acsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiEC2SecurityGroupOwnerId =
    lens _acsgiEC2SecurityGroupOwnerId
         (\s a -> s { _acsgiEC2SecurityGroupOwnerId = a })

instance ToQuery AuthorizeClusterSecurityGroupIngress where
    toQuery = genericQuery def

newtype AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse
    { _acsgirClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeClusterSecurityGroupIngressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSecurityGroup ::@ @Maybe ClusterSecurityGroup@
--
mkAuthorizeClusterSecurityGroupIngressResponse :: AuthorizeClusterSecurityGroupIngressResponse
mkAuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse
    { _acsgirClusterSecurityGroup = Nothing
    }

-- | Describes a security group.
acsgirClusterSecurityGroup :: Lens' AuthorizeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
acsgirClusterSecurityGroup =
    lens _acsgirClusterSecurityGroup
         (\s a -> s { _acsgirClusterSecurityGroup = a })

instance FromXML AuthorizeClusterSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeClusterSecurityGroupIngress where
    type Sv AuthorizeClusterSecurityGroupIngress = Redshift
    type Rs AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngressResponse

    request = post "AuthorizeClusterSecurityGroupIngress"
    response _ = xmlResponse
