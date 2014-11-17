{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
-- group. You can add as many as 20 ingress rules to an Amazon Redshift
-- security group. For an overview of CIDR blocks, see the Wikipedia article
-- on Classless Inter-Domain Routing. You must also associate the security
-- group with a cluster so that clients running on these IP addresses or the
-- EC2 instance are authorized to connect to the cluster. For information
-- about managing security groups, go to Working with Security Groups in the
-- Amazon Redshift Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_AuthorizeClusterSecurityGroupIngress.html>
module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
    (
    -- * Request
      AuthorizeClusterSecurityGroupIngress
    -- ** Request constructor
    , authorizeClusterSecurityGroupIngress
    -- ** Request lenses
    , acsgiCIDRIP
    , acsgiClusterSecurityGroupName
    , acsgiEC2SecurityGroupName
    , acsgiEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeClusterSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , acsgirClusterSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress
    { _acsgiCIDRIP                   :: Maybe Text
    , _acsgiClusterSecurityGroupName :: Text
    , _acsgiEC2SecurityGroupName     :: Maybe Text
    , _acsgiEC2SecurityGroupOwnerId  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AuthorizeClusterSecurityGroupIngress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgiCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'acsgiClusterSecurityGroupName' @::@ 'Text'
--
-- * 'acsgiEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'acsgiEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
authorizeClusterSecurityGroupIngress :: Text -- ^ 'acsgiClusterSecurityGroupName'
                                     -> AuthorizeClusterSecurityGroupIngress
authorizeClusterSecurityGroupIngress p1 = AuthorizeClusterSecurityGroupIngress
    { _acsgiClusterSecurityGroupName = p1
    , _acsgiCIDRIP                   = Nothing
    , _acsgiEC2SecurityGroupName     = Nothing
    , _acsgiEC2SecurityGroupOwnerId  = Nothing
    }

-- | The IP range to be added the Amazon Redshift security group.
acsgiCIDRIP :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiCIDRIP = lens _acsgiCIDRIP (\s a -> s { _acsgiCIDRIP = a })

-- | The name of the security group to which the ingress rule is added.
acsgiClusterSecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress Text
acsgiClusterSecurityGroupName =
    lens _acsgiClusterSecurityGroupName
        (\s a -> s { _acsgiClusterSecurityGroupName = a })

-- | The EC2 security group to be added the Amazon Redshift security group.
acsgiEC2SecurityGroupName :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiEC2SecurityGroupName =
    lens _acsgiEC2SecurityGroupName
        (\s a -> s { _acsgiEC2SecurityGroupName = a })

-- | The AWS account number of the owner of the security group specified by
-- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. Example: 111122223333.
acsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeClusterSecurityGroupIngress (Maybe Text)
acsgiEC2SecurityGroupOwnerId =
    lens _acsgiEC2SecurityGroupOwnerId
        (\s a -> s { _acsgiEC2SecurityGroupOwnerId = a })

newtype AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse
    { _acsgirClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'AuthorizeClusterSecurityGroupIngressResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgirClusterSecurityGroup' @::@ 'Maybe' 'ClusterSecurityGroup'
--
authorizeClusterSecurityGroupIngressResponse :: AuthorizeClusterSecurityGroupIngressResponse
authorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse
    { _acsgirClusterSecurityGroup = Nothing
    }

acsgirClusterSecurityGroup :: Lens' AuthorizeClusterSecurityGroupIngressResponse (Maybe ClusterSecurityGroup)
acsgirClusterSecurityGroup =
    lens _acsgirClusterSecurityGroup
        (\s a -> s { _acsgirClusterSecurityGroup = a })

instance ToPath AuthorizeClusterSecurityGroupIngress where
    toPath = const "/"

instance ToQuery AuthorizeClusterSecurityGroupIngress

instance ToHeaders AuthorizeClusterSecurityGroupIngress

instance AWSRequest AuthorizeClusterSecurityGroupIngress where
    type Sv AuthorizeClusterSecurityGroupIngress = Redshift
    type Rs AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngressResponse

    request  = post "AuthorizeClusterSecurityGroupIngress"
    response = xmlResponse

instance FromXML AuthorizeClusterSecurityGroupIngressResponse where
    parseXML c = AuthorizeClusterSecurityGroupIngressResponse
        <$> c .:? "ClusterSecurityGroup"
