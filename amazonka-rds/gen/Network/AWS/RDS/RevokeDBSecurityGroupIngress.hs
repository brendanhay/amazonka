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

-- Module      : Network.AWS.RDS.RevokeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Revokes ingress from a DBSecurityGroup for previously authorized IP ranges
-- or EC2 or VPC Security Groups. Required parameters for this API are one of
-- CIDRIP, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId).
module Network.AWS.RDS.RevokeDBSecurityGroupIngress
    (
    -- * Request
      RevokeDBSecurityGroupIngressMessage
    -- ** Request constructor
    , revokeDBSecurityGroupIngressMessage
    -- ** Request lenses
    , rdbsgimCIDRIP
    , rdbsgimDBSecurityGroupName
    , rdbsgimEC2SecurityGroupId
    , rdbsgimEC2SecurityGroupName
    , rdbsgimEC2SecurityGroupOwnerId

    -- * Response
    , RevokeDBSecurityGroupIngressResult
    -- ** Response constructor
    , revokeDBSecurityGroupIngressResult
    -- ** Response lenses
    , rdbsgirDBSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data RevokeDBSecurityGroupIngressMessage = RevokeDBSecurityGroupIngressMessage
    { _rdbsgimCIDRIP                  :: Maybe Text
    , _rdbsgimDBSecurityGroupName     :: Text
    , _rdbsgimEC2SecurityGroupId      :: Maybe Text
    , _rdbsgimEC2SecurityGroupName    :: Maybe Text
    , _rdbsgimEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RevokeDBSecurityGroupIngressMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbsgimCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'rdbsgimDBSecurityGroupName' @::@ 'Text'
--
-- * 'rdbsgimEC2SecurityGroupId' @::@ 'Maybe' 'Text'
--
-- * 'rdbsgimEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbsgimEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
revokeDBSecurityGroupIngressMessage :: Text -- ^ 'rdbsgimDBSecurityGroupName'
                                    -> RevokeDBSecurityGroupIngressMessage
revokeDBSecurityGroupIngressMessage p1 = RevokeDBSecurityGroupIngressMessage
    { _rdbsgimDBSecurityGroupName     = p1
    , _rdbsgimCIDRIP                  = Nothing
    , _rdbsgimEC2SecurityGroupName    = Nothing
    , _rdbsgimEC2SecurityGroupId      = Nothing
    , _rdbsgimEC2SecurityGroupOwnerId = Nothing
    }

-- | The IP range to revoke access from. Must be a valid CIDR range. If CIDRIP
-- is specified, EC2SecurityGroupName, EC2SecurityGroupId and
-- EC2SecurityGroupOwnerId cannot be provided.
rdbsgimCIDRIP :: Lens' RevokeDBSecurityGroupIngressMessage (Maybe Text)
rdbsgimCIDRIP = lens _rdbsgimCIDRIP (\s a -> s { _rdbsgimCIDRIP = a })

-- | The name of the DB security group to revoke ingress from.
rdbsgimDBSecurityGroupName :: Lens' RevokeDBSecurityGroupIngressMessage Text
rdbsgimDBSecurityGroupName =
    lens _rdbsgimDBSecurityGroupName
        (\s a -> s { _rdbsgimDBSecurityGroupName = a })

-- | The id of the EC2 security group to revoke access from. For VPC DB
-- security groups, EC2SecurityGroupId must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupId :: Lens' RevokeDBSecurityGroupIngressMessage (Maybe Text)
rdbsgimEC2SecurityGroupId =
    lens _rdbsgimEC2SecurityGroupId
        (\s a -> s { _rdbsgimEC2SecurityGroupId = a })

-- | The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, EC2SecurityGroupId must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupName :: Lens' RevokeDBSecurityGroupIngressMessage (Maybe Text)
rdbsgimEC2SecurityGroupName =
    lens _rdbsgimEC2SecurityGroupName
        (\s a -> s { _rdbsgimEC2SecurityGroupName = a })

-- | The AWS Account Number of the owner of the EC2 security group specified
-- in the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, EC2SecurityGroupId must be
-- provided. Otherwise, EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupOwnerId :: Lens' RevokeDBSecurityGroupIngressMessage (Maybe Text)
rdbsgimEC2SecurityGroupOwnerId =
    lens _rdbsgimEC2SecurityGroupOwnerId
        (\s a -> s { _rdbsgimEC2SecurityGroupOwnerId = a })
instance ToQuery RevokeDBSecurityGroupIngressMessage

instance ToPath RevokeDBSecurityGroupIngressMessage where
    toPath = const "/"

newtype RevokeDBSecurityGroupIngressResult = RevokeDBSecurityGroupIngressResult
    { _rdbsgirDBSecurityGroup :: Maybe DBSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'RevokeDBSecurityGroupIngressResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbsgirDBSecurityGroup' @::@ 'Maybe' 'DBSecurityGroup'
--
revokeDBSecurityGroupIngressResult :: RevokeDBSecurityGroupIngressResult
revokeDBSecurityGroupIngressResult = RevokeDBSecurityGroupIngressResult
    { _rdbsgirDBSecurityGroup = Nothing
    }

rdbsgirDBSecurityGroup :: Lens' RevokeDBSecurityGroupIngressResult (Maybe DBSecurityGroup)
rdbsgirDBSecurityGroup =
    lens _rdbsgirDBSecurityGroup (\s a -> s { _rdbsgirDBSecurityGroup = a })
instance FromXML RevokeDBSecurityGroupIngressResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RevokeDBSecurityGroupIngressResult"

instance AWSRequest RevokeDBSecurityGroupIngressMessage where
    type Sv RevokeDBSecurityGroupIngressMessage = RDS
    type Rs RevokeDBSecurityGroupIngressMessage = RevokeDBSecurityGroupIngressResult

    request  = post "RevokeDBSecurityGroupIngress"
    response = xmlResponse $ \h x -> RevokeDBSecurityGroupIngressResult
        <$> x %| "DBSecurityGroup"
