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

-- Module      : Network.AWS.EC2.CreateSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a security group. A security group is for use with instances either
-- in the EC2-Classic platform or in a specific VPC. For more information, see
-- Amazon EC2 Security Groups in the Amazon Elastic Compute Cloud User Guide
-- and Security Groups for Your VPC in the Amazon Virtual Private Cloud User
-- Guide. EC2-Classic: You can have up to 500 security groups. EC2-VPC: You
-- can create up to 100 security groups per VPC. When you create a security
-- group, you specify a friendly name of your choice. You can have a security
-- group for use in EC2-Classic with the same name as a security group for use
-- in a VPC. However, you can't have two security groups for use in
-- EC2-Classic with the same name or two security groups for use in a VPC with
-- the same name. You have a default security group for use in EC2-Classic and
-- a default security group for use in your VPC. If you don't specify a
-- security group when you launch an instance, the instance is launched into
-- the appropriate default security group. A default security group includes a
-- default rule that grants instances unrestricted network access to each
-- other. You can add or remove rules from your security groups using
-- AuthorizeSecurityGroupIngress, AuthorizeSecurityGroupEgress,
-- RevokeSecurityGroupIngress, and RevokeSecurityGroupEgress.
module Network.AWS.EC2.CreateSecurityGroup
    (
    -- * Request
      CreateSecurityGroup
    -- ** Request constructor
    , createSecurityGroup
    -- ** Request lenses
    , csgDescription
    , csgDryRun
    , csgGroupName
    , csgVpcId

    -- * Response
    , CreateSecurityGroupResult
    -- ** Response constructor
    , createSecurityGroupResult
    -- ** Response lenses
    , csgrGroupId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateSecurityGroup = CreateSecurityGroup
    { _csgDescription :: Text
    , _csgDryRun      :: Maybe Bool
    , _csgGroupName   :: Text
    , _csgVpcId       :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'CreateSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgDescription' @::@ 'Text'
--
-- * 'csgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'csgGroupName' @::@ 'Text'
--
-- * 'csgVpcId' @::@ 'Maybe' 'Text'
--
createSecurityGroup :: Text -- ^ 'csgGroupName'
                    -> Text -- ^ 'csgDescription'
                    -> CreateSecurityGroup
createSecurityGroup p1 p2 = CreateSecurityGroup
    { _csgGroupName   = p1
    , _csgDescription = p2
    , _csgDryRun      = Nothing
    , _csgVpcId       = Nothing
    }

-- | A description for the security group. This is informational only.
-- Constraints: Up to 255 characters in length Constraints for EC2-Classic:
-- ASCII characters Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:/()#,@[]+=&amp;;{}!$*.
csgDescription :: Lens' CreateSecurityGroup Text
csgDescription = lens _csgDescription (\s a -> s { _csgDescription = a })

csgDryRun :: Lens' CreateSecurityGroup (Maybe Bool)
csgDryRun = lens _csgDryRun (\s a -> s { _csgDryRun = a })

-- | The name of the security group. Constraints: Up to 255 characters in
-- length Constraints for EC2-Classic: ASCII characters Constraints for
-- EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&amp;;{}!$*.
csgGroupName :: Lens' CreateSecurityGroup Text
csgGroupName = lens _csgGroupName (\s a -> s { _csgGroupName = a })

-- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
csgVpcId :: Lens' CreateSecurityGroup (Maybe Text)
csgVpcId = lens _csgVpcId (\s a -> s { _csgVpcId = a })
instance ToQuery CreateSecurityGroup

instance ToPath CreateSecurityGroup where
    toPath = const "/"

newtype CreateSecurityGroupResult = CreateSecurityGroupResult
    { _csgrGroupId :: Maybe Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateSecurityGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgrGroupId' @::@ 'Maybe' 'Text'
--
createSecurityGroupResult :: CreateSecurityGroupResult
createSecurityGroupResult = CreateSecurityGroupResult
    { _csgrGroupId = Nothing
    }

-- | The ID of the security group.
csgrGroupId :: Lens' CreateSecurityGroupResult (Maybe Text)
csgrGroupId = lens _csgrGroupId (\s a -> s { _csgrGroupId = a })

instance FromXML CreateSecurityGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateSecurityGroupResult"

instance AWSRequest CreateSecurityGroup where
    type Sv CreateSecurityGroup = EC2
    type Rs CreateSecurityGroup = CreateSecurityGroupResult

    request  = post "CreateSecurityGroup"
    response = xmlResponse $ \h x -> CreateSecurityGroupResult
        <$> x %| "groupId"
