{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.RevokeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Revokes an ingress rule in an Amazon Redshift security group for a
-- previously authorized IP range or Amazon EC2 security group. To add an
-- ingress rule, see AuthorizeClusterSecurityGroupIngress. For information
-- about managing security groups, go to Amazon Redshift Cluster Security
-- Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=RevokeClusterSecurityGroupIngress
-- &ClusterSecurityGroupName=securitygroup1 &CIDRIP=192.168.40.3/32
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T021606Z
-- &x-amz-signedheaders=content-type;host;x-amz-date my security group
-- securitygroup1 d8eff363-6502-11e2-a8da-655adc216806.
module Network.AWS.Redshift.V2012_12_01.RevokeClusterSecurityGroupIngress
    (
    -- * Request
      RevokeClusterSecurityGroupIngress
    -- ** Request constructor
    , revokeClusterSecurityGroupIngress
    -- ** Request lenses
    , rcsgimClusterSecurityGroupName
    , rcsgimCIDRIP
    , rcsgimEC2SecurityGroupName
    , rcsgimEC2SecurityGroupOwnerId

    -- * Response
    , RevokeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , csgcrClusterSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RevokeClusterSecurityGroupIngress' request.
revokeClusterSecurityGroupIngress :: Text -- ^ 'rcsgimClusterSecurityGroupName'
                                  -> RevokeClusterSecurityGroupIngress
revokeClusterSecurityGroupIngress p1 = RevokeClusterSecurityGroupIngress
    { _rcsgimClusterSecurityGroupName = p1
    , _rcsgimCIDRIP = Nothing
    , _rcsgimEC2SecurityGroupName = Nothing
    , _rcsgimEC2SecurityGroupOwnerId = Nothing
    }
{-# INLINE revokeClusterSecurityGroupIngress #-}

data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress
    { _rcsgimClusterSecurityGroupName :: Text
      -- ^ The name of the security Group from which to revoke the ingress
      -- rule.
    , _rcsgimCIDRIP :: Maybe Text
      -- ^ The IP range for which to revoke access. This range must be a
      -- valid Classless Inter-Domain Routing (CIDR) block of IP
      -- addresses. If CIDRIP is specified, EC2SecurityGroupName and
      -- EC2SecurityGroupOwnerId cannot be provided.
    , _rcsgimEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the EC2 Security Group whose access is to be revoked.
      -- If EC2SecurityGroupName is specified, EC2SecurityGroupOwnerId
      -- must also be provided and CIDRIP cannot be provided.
    , _rcsgimEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS account number of the owner of the security group
      -- specified in the EC2SecurityGroupName parameter. The AWS access
      -- key ID is not an acceptable value. If EC2SecurityGroupOwnerId is
      -- specified, EC2SecurityGroupName must also be provided. and CIDRIP
      -- cannot be provided. Example: 111122223333.
    } deriving (Show, Generic)

-- | The name of the security Group from which to revoke the ingress rule.
rcsgimClusterSecurityGroupName :: Lens' RevokeClusterSecurityGroupIngress (Text)
rcsgimClusterSecurityGroupName f x =
    f (_rcsgimClusterSecurityGroupName x)
        <&> \y -> x { _rcsgimClusterSecurityGroupName = y }
{-# INLINE rcsgimClusterSecurityGroupName #-}

-- | The IP range for which to revoke access. This range must be a valid
-- Classless Inter-Domain Routing (CIDR) block of IP addresses. If CIDRIP is
-- specified, EC2SecurityGroupName and EC2SecurityGroupOwnerId cannot be
-- provided.
rcsgimCIDRIP :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgimCIDRIP f x =
    f (_rcsgimCIDRIP x)
        <&> \y -> x { _rcsgimCIDRIP = y }
{-# INLINE rcsgimCIDRIP #-}

-- | The name of the EC2 Security Group whose access is to be revoked. If
-- EC2SecurityGroupName is specified, EC2SecurityGroupOwnerId must also be
-- provided and CIDRIP cannot be provided.
rcsgimEC2SecurityGroupName :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgimEC2SecurityGroupName f x =
    f (_rcsgimEC2SecurityGroupName x)
        <&> \y -> x { _rcsgimEC2SecurityGroupName = y }
{-# INLINE rcsgimEC2SecurityGroupName #-}

-- | The AWS account number of the owner of the security group specified in the
-- EC2SecurityGroupName parameter. The AWS access key ID is not an acceptable
-- value. If EC2SecurityGroupOwnerId is specified, EC2SecurityGroupName must
-- also be provided. and CIDRIP cannot be provided. Example: 111122223333.
rcsgimEC2SecurityGroupOwnerId :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgimEC2SecurityGroupOwnerId f x =
    f (_rcsgimEC2SecurityGroupOwnerId x)
        <&> \y -> x { _rcsgimEC2SecurityGroupOwnerId = y }
{-# INLINE rcsgimEC2SecurityGroupOwnerId #-}

instance ToQuery RevokeClusterSecurityGroupIngress where
    toQuery = genericQuery def

data RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse
    { _csgcrClusterSecurityGroup :: Maybe ClusterSecurityGroup
      -- ^ Describes a security group.
    } deriving (Show, Generic)

-- | Describes a security group.
csgcrClusterSecurityGroup :: Lens' RevokeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
csgcrClusterSecurityGroup f x =
    f (_csgcrClusterSecurityGroup x)
        <&> \y -> x { _csgcrClusterSecurityGroup = y }
{-# INLINE csgcrClusterSecurityGroup #-}

instance FromXML RevokeClusterSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeClusterSecurityGroupIngress where
    type Sv RevokeClusterSecurityGroupIngress = Redshift
    type Rs RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngressResponse

    request = post "RevokeClusterSecurityGroupIngress"
    response _ = xmlResponse
