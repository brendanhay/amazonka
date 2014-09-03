{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.AuthorizeClusterSecurityGroupIngress
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
module Network.AWS.Redshift.V2012_12_01.AuthorizeClusterSecurityGroupIngress
    (
    -- * Request
      AuthorizeClusterSecurityGroupIngress
    -- ** Request constructor
    , authorizeClusterSecurityGroupIngress
    -- ** Request lenses
    , acsgimClusterSecurityGroupName
    , acsgimCIDRIP
    , acsgimEC2SecurityGroupName
    , acsgimEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , csgwClusterSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AuthorizeClusterSecurityGroupIngress' request.
authorizeClusterSecurityGroupIngress :: Text -- ^ 'acsgimClusterSecurityGroupName'
                                     -> AuthorizeClusterSecurityGroupIngress
authorizeClusterSecurityGroupIngress p1 = AuthorizeClusterSecurityGroupIngress
    { _acsgimClusterSecurityGroupName = p1
    , _acsgimCIDRIP = Nothing
    , _acsgimEC2SecurityGroupName = Nothing
    , _acsgimEC2SecurityGroupOwnerId = Nothing
    }

data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress
    { _acsgimClusterSecurityGroupName :: Text
      -- ^ The name of the security group to which the ingress rule is
      -- added.
    , _acsgimCIDRIP :: Maybe Text
      -- ^ The IP range to be added the Amazon Redshift security group.
    , _acsgimEC2SecurityGroupName :: Maybe Text
      -- ^ The EC2 security group to be added the Amazon Redshift security
      -- group.
    , _acsgimEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS account number of the owner of the security group
      -- specified by the EC2SecurityGroupName parameter. The AWS Access
      -- Key ID is not an acceptable value. Example: 111122223333.
    } deriving (Show, Generic)

-- | The name of the security group to which the ingress rule is added.
acsgimClusterSecurityGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> AuthorizeClusterSecurityGroupIngress
    -> f AuthorizeClusterSecurityGroupIngress
acsgimClusterSecurityGroupName f x =
    (\y -> x { _acsgimClusterSecurityGroupName = y })
       <$> f (_acsgimClusterSecurityGroupName x)
{-# INLINE acsgimClusterSecurityGroupName #-}

-- | The IP range to be added the Amazon Redshift security group.
acsgimCIDRIP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AuthorizeClusterSecurityGroupIngress
    -> f AuthorizeClusterSecurityGroupIngress
acsgimCIDRIP f x =
    (\y -> x { _acsgimCIDRIP = y })
       <$> f (_acsgimCIDRIP x)
{-# INLINE acsgimCIDRIP #-}

-- | The EC2 security group to be added the Amazon Redshift security group.
acsgimEC2SecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AuthorizeClusterSecurityGroupIngress
    -> f AuthorizeClusterSecurityGroupIngress
acsgimEC2SecurityGroupName f x =
    (\y -> x { _acsgimEC2SecurityGroupName = y })
       <$> f (_acsgimEC2SecurityGroupName x)
{-# INLINE acsgimEC2SecurityGroupName #-}

-- | The AWS account number of the owner of the security group specified by the
-- EC2SecurityGroupName parameter. The AWS Access Key ID is not an acceptable
-- value. Example: 111122223333.
acsgimEC2SecurityGroupOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AuthorizeClusterSecurityGroupIngress
    -> f AuthorizeClusterSecurityGroupIngress
acsgimEC2SecurityGroupOwnerId f x =
    (\y -> x { _acsgimEC2SecurityGroupOwnerId = y })
       <$> f (_acsgimEC2SecurityGroupOwnerId x)
{-# INLINE acsgimEC2SecurityGroupOwnerId #-}

instance ToQuery AuthorizeClusterSecurityGroupIngress where
    toQuery = genericQuery def

data AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse
    { _csgwClusterSecurityGroup :: Maybe ClusterSecurityGroup
      -- ^ Describes a security group.
    } deriving (Show, Generic)

-- | Describes a security group.
csgwClusterSecurityGroup
    :: Functor f
    => (Maybe ClusterSecurityGroup
    -> f (Maybe ClusterSecurityGroup))
    -> AuthorizeClusterSecurityGroupIngressResponse
    -> f AuthorizeClusterSecurityGroupIngressResponse
csgwClusterSecurityGroup f x =
    (\y -> x { _csgwClusterSecurityGroup = y })
       <$> f (_csgwClusterSecurityGroup x)
{-# INLINE csgwClusterSecurityGroup #-}

instance FromXML AuthorizeClusterSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeClusterSecurityGroupIngress where
    type Sv AuthorizeClusterSecurityGroupIngress = Redshift
    type Rs AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngressResponse

    request = post "AuthorizeClusterSecurityGroupIngress"
    response _ = xmlResponse
