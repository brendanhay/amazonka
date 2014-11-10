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

-- Module      : Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables ingress to a DBSecurityGroup using one of two forms of
-- authorization. First, EC2 or VPC security groups can be added to the
-- DBSecurityGroup if the application using the database is running on EC2 or
-- VPC instances. Second, IP ranges are available if the application accessing
-- your database is running on the Internet. Required parameters for this API
-- are one of CIDR range, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId for non-VPC). For an overview of CIDR ranges, go to the
-- Wikipedia Tutorial.
module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
    (
    -- * Request
      AuthorizeDBSecurityGroupIngressMessage
    -- ** Request constructor
    , authorizeDBSecurityGroupIngress
    -- ** Request lenses
    , adbsgimCIDRIP
    , adbsgimDBSecurityGroupName
    , adbsgimEC2SecurityGroupId
    , adbsgimEC2SecurityGroupName
    , adbsgimEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeDBSecurityGroupIngressResult
    -- ** Response constructor
    , authorizeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , adbsgirDBSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data AuthorizeDBSecurityGroupIngressMessage = AuthorizeDBSecurityGroupIngressMessage
    { _adbsgimCIDRIP                  :: Maybe Text
    , _adbsgimDBSecurityGroupName     :: Text
    , _adbsgimEC2SecurityGroupId      :: Maybe Text
    , _adbsgimEC2SecurityGroupName    :: Maybe Text
    , _adbsgimEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AuthorizeDBSecurityGroupIngressMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adbsgimCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'adbsgimDBSecurityGroupName' @::@ 'Text'
--
-- * 'adbsgimEC2SecurityGroupId' @::@ 'Maybe' 'Text'
--
-- * 'adbsgimEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'adbsgimEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
authorizeDBSecurityGroupIngress :: Text -- ^ 'adbsgimDBSecurityGroupName'
                                -> AuthorizeDBSecurityGroupIngressMessage
authorizeDBSecurityGroupIngress p1 = AuthorizeDBSecurityGroupIngressMessage
    { _adbsgimDBSecurityGroupName     = p1
    , _adbsgimCIDRIP                  = Nothing
    , _adbsgimEC2SecurityGroupName    = Nothing
    , _adbsgimEC2SecurityGroupId      = Nothing
    , _adbsgimEC2SecurityGroupOwnerId = Nothing
    }

-- | The IP range to authorize.
adbsgimCIDRIP :: Lens' AuthorizeDBSecurityGroupIngressMessage (Maybe Text)
adbsgimCIDRIP = lens _adbsgimCIDRIP (\s a -> s { _adbsgimCIDRIP = a })

-- | The name of the DB security group to add authorization to.
adbsgimDBSecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngressMessage Text
adbsgimDBSecurityGroupName =
    lens _adbsgimDBSecurityGroupName
        (\s a -> s { _adbsgimDBSecurityGroupName = a })

-- | Id of the EC2 security group to authorize. For VPC DB security groups,
-- EC2SecurityGroupId must be provided. Otherwise, EC2SecurityGroupOwnerId
-- and either EC2SecurityGroupName or EC2SecurityGroupId must be provided.
adbsgimEC2SecurityGroupId :: Lens' AuthorizeDBSecurityGroupIngressMessage (Maybe Text)
adbsgimEC2SecurityGroupId =
    lens _adbsgimEC2SecurityGroupId
        (\s a -> s { _adbsgimEC2SecurityGroupId = a })

-- | Name of the EC2 security group to authorize. For VPC DB security groups,
-- EC2SecurityGroupId must be provided. Otherwise, EC2SecurityGroupOwnerId
-- and either EC2SecurityGroupName or EC2SecurityGroupId must be provided.
adbsgimEC2SecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngressMessage (Maybe Text)
adbsgimEC2SecurityGroupName =
    lens _adbsgimEC2SecurityGroupName
        (\s a -> s { _adbsgimEC2SecurityGroupName = a })

-- | AWS Account Number of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, EC2SecurityGroupId must be
-- provided. Otherwise, EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
adbsgimEC2SecurityGroupOwnerId :: Lens' AuthorizeDBSecurityGroupIngressMessage (Maybe Text)
adbsgimEC2SecurityGroupOwnerId =
    lens _adbsgimEC2SecurityGroupOwnerId
        (\s a -> s { _adbsgimEC2SecurityGroupOwnerId = a })

instance ToPath AuthorizeDBSecurityGroupIngressMessage where
    toPath = const "/"

instance ToQuery AuthorizeDBSecurityGroupIngressMessage

newtype AuthorizeDBSecurityGroupIngressResult = AuthorizeDBSecurityGroupIngressResult
    { _adbsgirDBSecurityGroup :: Maybe DBSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'AuthorizeDBSecurityGroupIngressResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adbsgirDBSecurityGroup' @::@ 'Maybe' 'DBSecurityGroup'
--
authorizeDBSecurityGroupIngressResponse :: AuthorizeDBSecurityGroupIngressResult
authorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResult
    { _adbsgirDBSecurityGroup = Nothing
    }

adbsgirDBSecurityGroup :: Lens' AuthorizeDBSecurityGroupIngressResult (Maybe DBSecurityGroup)
adbsgirDBSecurityGroup =
    lens _adbsgirDBSecurityGroup (\s a -> s { _adbsgirDBSecurityGroup = a })

instance AWSRequest AuthorizeDBSecurityGroupIngressMessage where
    type Sv AuthorizeDBSecurityGroupIngressMessage = RDS
    type Rs AuthorizeDBSecurityGroupIngressMessage = AuthorizeDBSecurityGroupIngressResult

    request  = post "AuthorizeDBSecurityGroupIngress"
    response = xmlResponse $ \h x -> AuthorizeDBSecurityGroupIngressResult
        <$> x %| "DBSecurityGroup"
