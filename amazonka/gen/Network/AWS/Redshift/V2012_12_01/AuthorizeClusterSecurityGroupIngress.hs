{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.Redshift.V2012_12_01.AuthorizeClusterSecurityGroupIngress where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AuthorizeClusterSecurityGroupIngress' request.
authorizeClusterSecurityGroupIngress :: Text -- ^ '_acsgimClusterSecurityGroupName'
                                     -> AuthorizeClusterSecurityGroupIngress
authorizeClusterSecurityGroupIngress p1 = AuthorizeClusterSecurityGroupIngress
    { _acsgimClusterSecurityGroupName = p1
    , _acsgimEC2SecurityGroupOwnerId = Nothing
    , _acsgimEC2SecurityGroupName = Nothing
    , _acsgimCIDRIP = Nothing
    }

data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress
    { _acsgimClusterSecurityGroupName :: Text
      -- ^ The name of the security group to which the ingress rule is
      -- added.
    , _acsgimEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS account number of the owner of the security group
      -- specified by the EC2SecurityGroupName parameter. The AWS Access
      -- Key ID is not an acceptable value. Example: 111122223333.
    , _acsgimEC2SecurityGroupName :: Maybe Text
      -- ^ The EC2 security group to be added the Amazon Redshift security
      -- group.
    , _acsgimCIDRIP :: Maybe Text
      -- ^ The IP range to be added the Amazon Redshift security group.
    } deriving (Show, Generic)

makeLenses ''AuthorizeClusterSecurityGroupIngress

instance ToQuery AuthorizeClusterSecurityGroupIngress where
    toQuery = genericToQuery def

data AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse
    { _csgzClusterSecurityGroup :: Maybe ClusterSecurityGroup
      -- ^ Describes a security group.
    } deriving (Show, Generic)

makeLenses ''AuthorizeClusterSecurityGroupIngressResponse

instance AWSRequest AuthorizeClusterSecurityGroupIngress where
    type Sv AuthorizeClusterSecurityGroupIngress = Redshift
    type Rs AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngressResponse

    request = post "AuthorizeClusterSecurityGroupIngress"
    response _ = cursorResponse $ \hs xml ->
        pure AuthorizeClusterSecurityGroupIngressResponse
            <*> xml %|? "ClusterSecurityGroup"
