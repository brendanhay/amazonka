{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
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
module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
    (
    -- * Request
      RevokeClusterSecurityGroupIngress
    -- ** Request constructor
    , revokeClusterSecurityGroupIngress
    -- ** Request lenses
    , rcsgiClusterSecurityGroupName
    , rcsgiCIDRIP
    , rcsgiEC2SecurityGroupName
    , rcsgiEC2SecurityGroupOwnerId

    -- * Response
    , RevokeClusterSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , rcsgirClusterSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | ???.
data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress
    { _rcsgiClusterSecurityGroupName :: Text
    , _rcsgiCIDRIP :: Maybe Text
    , _rcsgiEC2SecurityGroupName :: Maybe Text
    , _rcsgiEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeClusterSecurityGroupIngress' request.
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
revokeClusterSecurityGroupIngress :: Text -- ^ 'rcsgiClusterSecurityGroupName'
                                  -> RevokeClusterSecurityGroupIngress
revokeClusterSecurityGroupIngress p1 = RevokeClusterSecurityGroupIngress
    { _rcsgiClusterSecurityGroupName = p1
    , _rcsgiCIDRIP = Nothing
    , _rcsgiEC2SecurityGroupName = Nothing
    , _rcsgiEC2SecurityGroupOwnerId = Nothing
    }

-- | The name of the security Group from which to revoke the ingress rule.
rcsgiClusterSecurityGroupName :: Lens' RevokeClusterSecurityGroupIngress Text
rcsgiClusterSecurityGroupName =
    lens _rcsgiClusterSecurityGroupName
         (\s a -> s { _rcsgiClusterSecurityGroupName = a })

-- | The IP range for which to revoke access. This range must be a valid
-- Classless Inter-Domain Routing (CIDR) block of IP addresses. If CIDRIP is
-- specified, EC2SecurityGroupName and EC2SecurityGroupOwnerId cannot be
-- provided.
rcsgiCIDRIP :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiCIDRIP = lens _rcsgiCIDRIP (\s a -> s { _rcsgiCIDRIP = a })

-- | The name of the EC2 Security Group whose access is to be revoked. If
-- EC2SecurityGroupName is specified, EC2SecurityGroupOwnerId must also be
-- provided and CIDRIP cannot be provided.
rcsgiEC2SecurityGroupName :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiEC2SecurityGroupName =
    lens _rcsgiEC2SecurityGroupName
         (\s a -> s { _rcsgiEC2SecurityGroupName = a })

-- | The AWS account number of the owner of the security group specified in the
-- EC2SecurityGroupName parameter. The AWS access key ID is not an acceptable
-- value. If EC2SecurityGroupOwnerId is specified, EC2SecurityGroupName must
-- also be provided. and CIDRIP cannot be provided. Example: 111122223333.
rcsgiEC2SecurityGroupOwnerId :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiEC2SecurityGroupOwnerId =
    lens _rcsgiEC2SecurityGroupOwnerId
         (\s a -> s { _rcsgiEC2SecurityGroupOwnerId = a })

instance ToQuery RevokeClusterSecurityGroupIngress where
    toQuery = genericQuery def

newtype RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse
    { _rcsgirClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeClusterSecurityGroupIngressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSecurityGroup ::@ @Maybe ClusterSecurityGroup@
--
revokeClusterSecurityGroupIngressResponse :: RevokeClusterSecurityGroupIngressResponse
revokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse
    { _rcsgirClusterSecurityGroup = Nothing
    }

-- | Describes a security group.
rcsgirClusterSecurityGroup :: Lens' RevokeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
rcsgirClusterSecurityGroup =
    lens _rcsgirClusterSecurityGroup
         (\s a -> s { _rcsgirClusterSecurityGroup = a })

instance FromXML RevokeClusterSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeClusterSecurityGroupIngress where
    type Sv RevokeClusterSecurityGroupIngress = Redshift
    type Rs RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngressResponse

    request = post "RevokeClusterSecurityGroupIngress"
    response _ = xmlResponse
