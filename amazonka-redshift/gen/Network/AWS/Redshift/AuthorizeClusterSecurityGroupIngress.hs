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
-- group. You can add as many as 20 ingress rules to an Amazon Redshift
-- security group. For an overview of CIDR blocks, see the Wikipedia article
-- on Classless Inter-Domain Routing. You must also associate the security
-- group with a cluster so that clients running on these IP addresses or the
-- EC2 instance are authorized to connect to the cluster. For information
-- about managing security groups, go to Working with Security Groups in the
-- Amazon Redshift Management Guide.
module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
    (
    -- * Request
      AuthorizeClusterSecurityGroupIngressMessage
    -- ** Request constructor
    , authorizeClusterSecurityGroupIngressMessage
    -- ** Request lenses
    , acsgimCIDRIP
    , acsgimClusterSecurityGroupName
    , acsgimEC2SecurityGroupName
    , acsgimEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeClusterSecurityGroupIngressResult
    -- ** Response constructor
    , authorizeClusterSecurityGroupIngressResult
    -- ** Response lenses
    , acsgirClusterSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data AuthorizeClusterSecurityGroupIngressMessage = AuthorizeClusterSecurityGroupIngressMessage
    { _acsgimCIDRIP                   :: Maybe Text
    , _acsgimClusterSecurityGroupName :: Text
    , _acsgimEC2SecurityGroupName     :: Maybe Text
    , _acsgimEC2SecurityGroupOwnerId  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AuthorizeClusterSecurityGroupIngressMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgimCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'acsgimClusterSecurityGroupName' @::@ 'Text'
--
-- * 'acsgimEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'acsgimEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
authorizeClusterSecurityGroupIngressMessage :: Text -- ^ 'acsgimClusterSecurityGroupName'
                                            -> AuthorizeClusterSecurityGroupIngressMessage
authorizeClusterSecurityGroupIngressMessage p1 = AuthorizeClusterSecurityGroupIngressMessage
    { _acsgimClusterSecurityGroupName = p1
    , _acsgimCIDRIP                   = Nothing
    , _acsgimEC2SecurityGroupName     = Nothing
    , _acsgimEC2SecurityGroupOwnerId  = Nothing
    }

-- | The IP range to be added the Amazon Redshift security group.
acsgimCIDRIP :: Lens' AuthorizeClusterSecurityGroupIngressMessage (Maybe Text)
acsgimCIDRIP = lens _acsgimCIDRIP (\s a -> s { _acsgimCIDRIP = a })

-- | The name of the security group to which the ingress rule is added.
acsgimClusterSecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngressMessage Text
acsgimClusterSecurityGroupName =
    lens _acsgimClusterSecurityGroupName
        (\s a -> s { _acsgimClusterSecurityGroupName = a })

-- | The EC2 security group to be added the Amazon Redshift security group.
acsgimEC2SecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngressMessage (Maybe Text)
acsgimEC2SecurityGroupName =
    lens _acsgimEC2SecurityGroupName
        (\s a -> s { _acsgimEC2SecurityGroupName = a })

-- | The AWS account number of the owner of the security group specified by
-- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. Example: 111122223333.
acsgimEC2SecurityGroupOwnerId :: Lens' AuthorizeClusterSecurityGroupIngressMessage (Maybe Text)
acsgimEC2SecurityGroupOwnerId =
    lens _acsgimEC2SecurityGroupOwnerId
        (\s a -> s { _acsgimEC2SecurityGroupOwnerId = a })

instance ToQuery AuthorizeClusterSecurityGroupIngressMessage

instance ToPath AuthorizeClusterSecurityGroupIngressMessage where
    toPath = const "/"

newtype AuthorizeClusterSecurityGroupIngressResult = AuthorizeClusterSecurityGroupIngressResult
    { _acsgirClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'AuthorizeClusterSecurityGroupIngressResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgirClusterSecurityGroup' @::@ 'Maybe' 'ClusterSecurityGroup'
--
authorizeClusterSecurityGroupIngressResult :: AuthorizeClusterSecurityGroupIngressResult
authorizeClusterSecurityGroupIngressResult = AuthorizeClusterSecurityGroupIngressResult
    { _acsgirClusterSecurityGroup = Nothing
    }

acsgirClusterSecurityGroup :: Lens' AuthorizeClusterSecurityGroupIngressResult (Maybe ClusterSecurityGroup)
acsgirClusterSecurityGroup =
    lens _acsgirClusterSecurityGroup
        (\s a -> s { _acsgirClusterSecurityGroup = a })

instance FromXML AuthorizeClusterSecurityGroupIngressResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AuthorizeClusterSecurityGroupIngressResult"

instance AWSRequest AuthorizeClusterSecurityGroupIngressMessage where
    type Sv AuthorizeClusterSecurityGroupIngressMessage = Redshift
    type Rs AuthorizeClusterSecurityGroupIngressMessage = AuthorizeClusterSecurityGroupIngressResult

    request  = post "AuthorizeClusterSecurityGroupIngress"
    response = xmlResponse $ \h x -> AuthorizeClusterSecurityGroupIngressResult
        <$> x %| "ClusterSecurityGroup"
