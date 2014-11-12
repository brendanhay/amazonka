{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      RevokeClusterSecurityGroupIngressMessage
    -- ** Request constructor
    , revokeClusterSecurityGroupIngress
    -- ** Request lenses
    , rcsgimCIDRIP
    , rcsgimClusterSecurityGroupName
    , rcsgimEC2SecurityGroupName
    , rcsgimEC2SecurityGroupOwnerId

    -- * Response
    , RevokeClusterSecurityGroupIngressResult
    -- ** Response constructor
    , revokeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , rcsgirClusterSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data RevokeClusterSecurityGroupIngressMessage = RevokeClusterSecurityGroupIngressMessage
    { _rcsgimCIDRIP                   :: Maybe Text
    , _rcsgimClusterSecurityGroupName :: Text
    , _rcsgimEC2SecurityGroupName     :: Maybe Text
    , _rcsgimEC2SecurityGroupOwnerId  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RevokeClusterSecurityGroupIngressMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgimCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'rcsgimClusterSecurityGroupName' @::@ 'Text'
--
-- * 'rcsgimEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rcsgimEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
revokeClusterSecurityGroupIngress :: Text -- ^ 'rcsgimClusterSecurityGroupName'
                                  -> RevokeClusterSecurityGroupIngressMessage
revokeClusterSecurityGroupIngress p1 = RevokeClusterSecurityGroupIngressMessage
    { _rcsgimClusterSecurityGroupName = p1
    , _rcsgimCIDRIP                   = Nothing
    , _rcsgimEC2SecurityGroupName     = Nothing
    , _rcsgimEC2SecurityGroupOwnerId  = Nothing
    }

-- | The IP range for which to revoke access. This range must be a valid
-- Classless Inter-Domain Routing (CIDR) block of IP addresses. If CIDRIP is
-- specified, EC2SecurityGroupName and EC2SecurityGroupOwnerId cannot be
-- provided.
rcsgimCIDRIP :: Lens' RevokeClusterSecurityGroupIngressMessage (Maybe Text)
rcsgimCIDRIP = lens _rcsgimCIDRIP (\s a -> s { _rcsgimCIDRIP = a })

-- | The name of the security Group from which to revoke the ingress rule.
rcsgimClusterSecurityGroupName :: Lens' RevokeClusterSecurityGroupIngressMessage Text
rcsgimClusterSecurityGroupName =
    lens _rcsgimClusterSecurityGroupName
        (\s a -> s { _rcsgimClusterSecurityGroupName = a })

-- | The name of the EC2 Security Group whose access is to be revoked. If
-- EC2SecurityGroupName is specified, EC2SecurityGroupOwnerId must also be
-- provided and CIDRIP cannot be provided.
rcsgimEC2SecurityGroupName :: Lens' RevokeClusterSecurityGroupIngressMessage (Maybe Text)
rcsgimEC2SecurityGroupName =
    lens _rcsgimEC2SecurityGroupName
        (\s a -> s { _rcsgimEC2SecurityGroupName = a })

-- | The AWS account number of the owner of the security group specified in
-- the EC2SecurityGroupName parameter. The AWS access key ID is not an
-- acceptable value. If EC2SecurityGroupOwnerId is specified,
-- EC2SecurityGroupName must also be provided. and CIDRIP cannot be
-- provided. Example: 111122223333.
rcsgimEC2SecurityGroupOwnerId :: Lens' RevokeClusterSecurityGroupIngressMessage (Maybe Text)
rcsgimEC2SecurityGroupOwnerId =
    lens _rcsgimEC2SecurityGroupOwnerId
        (\s a -> s { _rcsgimEC2SecurityGroupOwnerId = a })

instance ToQuery RevokeClusterSecurityGroupIngressMessage

instance ToPath RevokeClusterSecurityGroupIngressMessage where
    toPath = const "/"

newtype RevokeClusterSecurityGroupIngressResult = RevokeClusterSecurityGroupIngressResult
    { _rcsgirClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'RevokeClusterSecurityGroupIngressResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgirClusterSecurityGroup' @::@ 'Maybe' 'ClusterSecurityGroup'
--
revokeClusterSecurityGroupIngressResponse :: RevokeClusterSecurityGroupIngressResult
revokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResult
    { _rcsgirClusterSecurityGroup = Nothing
    }

rcsgirClusterSecurityGroup :: Lens' RevokeClusterSecurityGroupIngressResult (Maybe ClusterSecurityGroup)
rcsgirClusterSecurityGroup =
    lens _rcsgirClusterSecurityGroup
        (\s a -> s { _rcsgirClusterSecurityGroup = a })

instance FromXML RevokeClusterSecurityGroupIngressResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RevokeClusterSecurityGroupIngressResult"

instance AWSRequest RevokeClusterSecurityGroupIngressMessage where
    type Sv RevokeClusterSecurityGroupIngressMessage = Redshift
    type Rs RevokeClusterSecurityGroupIngressMessage = RevokeClusterSecurityGroupIngressResult

    request  = post "RevokeClusterSecurityGroupIngress"
    response = xmlResponse $ \h x -> RevokeClusterSecurityGroupIngressResult
        <$> x %| "ClusterSecurityGroup"
