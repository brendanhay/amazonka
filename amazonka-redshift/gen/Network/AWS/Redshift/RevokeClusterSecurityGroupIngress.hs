{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
    (
    -- * Request
      RevokeClusterSecurityGroupIngress
    -- ** Request constructor
    , revokeClusterSecurityGroupIngress
    -- ** Request lenses
    , rcsgiCIDRIP
    , rcsgiClusterSecurityGroupName
    , rcsgiEC2SecurityGroupName
    , rcsgiEC2SecurityGroupOwnerId

    -- * Response
    , RevokeClusterSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , rcsgirClusterSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress
    { _rcsgiCIDRIP                   :: Maybe Text
    , _rcsgiClusterSecurityGroupName :: Text
    , _rcsgiEC2SecurityGroupName     :: Maybe Text
    , _rcsgiEC2SecurityGroupOwnerId  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RevokeClusterSecurityGroupIngress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgiCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'rcsgiClusterSecurityGroupName' @::@ 'Text'
--
-- * 'rcsgiEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rcsgiEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
revokeClusterSecurityGroupIngress :: Text -- ^ 'rcsgiClusterSecurityGroupName'
                                  -> RevokeClusterSecurityGroupIngress
revokeClusterSecurityGroupIngress p1 = RevokeClusterSecurityGroupIngress
    { _rcsgiClusterSecurityGroupName = p1
    , _rcsgiCIDRIP                   = Nothing
    , _rcsgiEC2SecurityGroupName     = Nothing
    , _rcsgiEC2SecurityGroupOwnerId  = Nothing
    }

-- | The IP range for which to revoke access. This range must be a valid
-- Classless Inter-Domain Routing (CIDR) block of IP addresses. If CIDRIP is
-- specified, EC2SecurityGroupName and EC2SecurityGroupOwnerId cannot be
-- provided.
rcsgiCIDRIP :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiCIDRIP = lens _rcsgiCIDRIP (\s a -> s { _rcsgiCIDRIP = a })

-- | The name of the security Group from which to revoke the ingress rule.
rcsgiClusterSecurityGroupName :: Lens' RevokeClusterSecurityGroupIngress Text
rcsgiClusterSecurityGroupName =
    lens _rcsgiClusterSecurityGroupName
        (\s a -> s { _rcsgiClusterSecurityGroupName = a })

-- | The name of the EC2 Security Group whose access is to be revoked. If
-- EC2SecurityGroupName is specified, EC2SecurityGroupOwnerId must also be
-- provided and CIDRIP cannot be provided.
rcsgiEC2SecurityGroupName :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiEC2SecurityGroupName =
    lens _rcsgiEC2SecurityGroupName
        (\s a -> s { _rcsgiEC2SecurityGroupName = a })

-- | The AWS account number of the owner of the security group specified in
-- the EC2SecurityGroupName parameter. The AWS access key ID is not an
-- acceptable value. If EC2SecurityGroupOwnerId is specified,
-- EC2SecurityGroupName must also be provided. and CIDRIP cannot be
-- provided. Example: 111122223333.
rcsgiEC2SecurityGroupOwnerId :: Lens' RevokeClusterSecurityGroupIngress (Maybe Text)
rcsgiEC2SecurityGroupOwnerId =
    lens _rcsgiEC2SecurityGroupOwnerId
        (\s a -> s { _rcsgiEC2SecurityGroupOwnerId = a })

newtype RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse
    { _rcsgirClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'RevokeClusterSecurityGroupIngressResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgirClusterSecurityGroup' @::@ 'Maybe' 'ClusterSecurityGroup'
--
revokeClusterSecurityGroupIngressResponse :: RevokeClusterSecurityGroupIngressResponse
revokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse
    { _rcsgirClusterSecurityGroup = Nothing
    }

rcsgirClusterSecurityGroup :: Lens' RevokeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
rcsgirClusterSecurityGroup =
    lens _rcsgirClusterSecurityGroup
        (\s a -> s { _rcsgirClusterSecurityGroup = a })

instance AWSRequest RevokeClusterSecurityGroupIngress where
    type Sv RevokeClusterSecurityGroupIngress = Redshift
    type Rs RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngressResponse

    request  = post "RevokeClusterSecurityGroupIngress"
    response = xmlResponse

instance FromXML RevokeClusterSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RevokeClusterSecurityGroupIngressResponse"

instance ToPath RevokeClusterSecurityGroupIngress where
    toPath = const "/"

instance ToHeaders RevokeClusterSecurityGroupIngress

instance ToQuery RevokeClusterSecurityGroupIngress
