{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RevokeDBSecurityGroupIngress.html>
module Network.AWS.RDS.RevokeDBSecurityGroupIngress
    (
    -- * Request
      RevokeDBSecurityGroupIngress
    -- ** Request constructor
    , revokeDBSecurityGroupIngress
    -- ** Request lenses
    , rdbsgiCIDRIP
    , rdbsgiDBSecurityGroupName
    , rdbsgiEC2SecurityGroupId
    , rdbsgiEC2SecurityGroupName
    , rdbsgiEC2SecurityGroupOwnerId

    -- * Response
    , RevokeDBSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , rdbsgirDBSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress
    { _rdbsgiCIDRIP                  :: Maybe Text
    , _rdbsgiDBSecurityGroupName     :: Text
    , _rdbsgiEC2SecurityGroupId      :: Maybe Text
    , _rdbsgiEC2SecurityGroupName    :: Maybe Text
    , _rdbsgiEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'RevokeDBSecurityGroupIngress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbsgiCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'rdbsgiDBSecurityGroupName' @::@ 'Text'
--
-- * 'rdbsgiEC2SecurityGroupId' @::@ 'Maybe' 'Text'
--
-- * 'rdbsgiEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbsgiEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
revokeDBSecurityGroupIngress :: Text -- ^ 'rdbsgiDBSecurityGroupName'
                             -> RevokeDBSecurityGroupIngress
revokeDBSecurityGroupIngress p1 = RevokeDBSecurityGroupIngress
    { _rdbsgiDBSecurityGroupName     = p1
    , _rdbsgiCIDRIP                  = Nothing
    , _rdbsgiEC2SecurityGroupName    = Nothing
    , _rdbsgiEC2SecurityGroupId      = Nothing
    , _rdbsgiEC2SecurityGroupOwnerId = Nothing
    }

-- | The IP range to revoke access from. Must be a valid CIDR range. If
-- 'CIDRIP' is specified, 'EC2SecurityGroupName', 'EC2SecurityGroupId' and
-- 'EC2SecurityGroupOwnerId' cannot be provided.
rdbsgiCIDRIP :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgiCIDRIP = lens _rdbsgiCIDRIP (\s a -> s { _rdbsgiCIDRIP = a })

-- | The name of the DB security group to revoke ingress from.
rdbsgiDBSecurityGroupName :: Lens' RevokeDBSecurityGroupIngress Text
rdbsgiDBSecurityGroupName =
    lens _rdbsgiDBSecurityGroupName
        (\s a -> s { _rdbsgiDBSecurityGroupName = a })

-- | The id of the EC2 security group to revoke access from. For VPC DB
-- security groups, 'EC2SecurityGroupId' must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either 'EC2SecurityGroupName' or
-- 'EC2SecurityGroupId' must be provided.
rdbsgiEC2SecurityGroupId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgiEC2SecurityGroupId =
    lens _rdbsgiEC2SecurityGroupId
        (\s a -> s { _rdbsgiEC2SecurityGroupId = a })

-- | The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, 'EC2SecurityGroupId' must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either 'EC2SecurityGroupName' or
-- 'EC2SecurityGroupId' must be provided.
rdbsgiEC2SecurityGroupName :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgiEC2SecurityGroupName =
    lens _rdbsgiEC2SecurityGroupName
        (\s a -> s { _rdbsgiEC2SecurityGroupName = a })

-- | The AWS Account Number of the owner of the EC2 security group specified
-- in the 'EC2SecurityGroupName' parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, 'EC2SecurityGroupId' must
-- be provided. Otherwise, EC2SecurityGroupOwnerId and either
-- 'EC2SecurityGroupName' or 'EC2SecurityGroupId' must be provided.
rdbsgiEC2SecurityGroupOwnerId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgiEC2SecurityGroupOwnerId =
    lens _rdbsgiEC2SecurityGroupOwnerId
        (\s a -> s { _rdbsgiEC2SecurityGroupOwnerId = a })

newtype RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse
    { _rdbsgirDBSecurityGroup :: Maybe DBSecurityGroup
    } deriving (Eq, Show)

-- | 'RevokeDBSecurityGroupIngressResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbsgirDBSecurityGroup' @::@ 'Maybe' 'DBSecurityGroup'
--
revokeDBSecurityGroupIngressResponse :: RevokeDBSecurityGroupIngressResponse
revokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse
    { _rdbsgirDBSecurityGroup = Nothing
    }

rdbsgirDBSecurityGroup :: Lens' RevokeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
rdbsgirDBSecurityGroup =
    lens _rdbsgirDBSecurityGroup (\s a -> s { _rdbsgirDBSecurityGroup = a })

instance ToPath RevokeDBSecurityGroupIngress where
    toPath = const "/"

instance ToQuery RevokeDBSecurityGroupIngress where
    toQuery RevokeDBSecurityGroupIngress{..} = mconcat
        [ "CIDRIP"                  =? _rdbsgiCIDRIP
        , "DBSecurityGroupName"     =? _rdbsgiDBSecurityGroupName
        , "EC2SecurityGroupId"      =? _rdbsgiEC2SecurityGroupId
        , "EC2SecurityGroupName"    =? _rdbsgiEC2SecurityGroupName
        , "EC2SecurityGroupOwnerId" =? _rdbsgiEC2SecurityGroupOwnerId
        ]

instance ToHeaders RevokeDBSecurityGroupIngress

instance AWSRequest RevokeDBSecurityGroupIngress where
    type Sv RevokeDBSecurityGroupIngress = RDS
    type Rs RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngressResponse

    request  = post "RevokeDBSecurityGroupIngress"
    response = xmlResponse

instance FromXML RevokeDBSecurityGroupIngressResponse where
    parseXML = withElement "RevokeDBSecurityGroupIngressResult" $ \x -> RevokeDBSecurityGroupIngressResponse
        <$> x .@? "DBSecurityGroup"
