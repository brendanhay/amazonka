{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.CreateDeploymentGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new deployment group for application revisions to be deployed to.
module Network.AWS.CodeDeploy.CreateDeploymentGroup
    (
    -- * Request
      CreateDeploymentGroup
    -- ** Request constructor
    , createDeploymentGroup
    -- ** Request lenses
    , cdgApplicationName
    , cdgAutoScalingGroups
    , cdgDeploymentConfigName
    , cdgDeploymentGroupName
    , cdgEc2TagFilters
    , cdgServiceRoleArn

    -- * Response
    , CreateDeploymentGroupResponse
    -- ** Response constructor
    , createDeploymentGroupResponse
    -- ** Response lenses
    , cdgrDeploymentGroupId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data CreateDeploymentGroup = CreateDeploymentGroup
    { _cdgApplicationName      :: Text
    , _cdgAutoScalingGroups    :: [Text]
    , _cdgDeploymentConfigName :: Maybe Text
    , _cdgDeploymentGroupName  :: Text
    , _cdgEc2TagFilters        :: [EC2TagFilter]
    , _cdgServiceRoleArn       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateDeploymentGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdgApplicationName' @::@ 'Text'
--
-- * 'cdgAutoScalingGroups' @::@ ['Text']
--
-- * 'cdgDeploymentConfigName' @::@ 'Maybe' 'Text'
--
-- * 'cdgDeploymentGroupName' @::@ 'Text'
--
-- * 'cdgEc2TagFilters' @::@ ['EC2TagFilter']
--
-- * 'cdgServiceRoleArn' @::@ 'Maybe' 'Text'
--
createDeploymentGroup :: Text -- ^ 'cdgApplicationName'
                      -> Text -- ^ 'cdgDeploymentGroupName'
                      -> CreateDeploymentGroup
createDeploymentGroup p1 p2 = CreateDeploymentGroup
    { _cdgApplicationName      = p1
    , _cdgDeploymentGroupName  = p2
    , _cdgDeploymentConfigName = Nothing
    , _cdgEc2TagFilters        = mempty
    , _cdgAutoScalingGroups    = mempty
    , _cdgServiceRoleArn       = Nothing
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
cdgApplicationName :: Lens' CreateDeploymentGroup Text
cdgApplicationName =
    lens _cdgApplicationName (\s a -> s { _cdgApplicationName = a })

-- | A list of associated Auto Scaling groups.
cdgAutoScalingGroups :: Lens' CreateDeploymentGroup [Text]
cdgAutoScalingGroups =
    lens _cdgAutoScalingGroups (\s a -> s { _cdgAutoScalingGroups = a })

-- | If specified, the deployment configuration name must be one of the
-- predefined values, or it can be a custom deployment configuration:
-- CodeDeployDefault.AllAtOnce deploys an application revision to up to all
-- of the Amazon EC2 instances at once. The overall deployment succeeds if
-- the application revision deploys to at least one of the instances. The
-- overall deployment fails after the application revision fails to deploy
-- to all of the instances. For example, for 9 instances, deploy to up to
-- all 9 instances at once. The overall deployment succeeds if any of the 9
-- instances is successfully deployed to, and it fails if all 9 instances
-- fail to be deployed to. CodeDeployDefault.HalfAtATime deploys to up to
-- half of the instances at a time (with fractions rounded down). The
-- overall deployment succeeds if the application revision deploys to at
-- least half of the instances (with fractions rounded up); otherwise, the
-- deployment fails. For example, for 9 instances, deploy to up to 4
-- instances at a time. The overall deployment succeeds if 5 or more
-- instances are successfully deployed to; otherwise, the deployment fails.
-- Note that the deployment may successfully deploy to some instances, even
-- if the overall deployment fails. CodeDeployDefault.OneAtATime deploys the
-- application revision to only one of the instances at a time. The overall
-- deployment succeeds if the application revision deploys to all of the
-- instances. The overall deployment fails after the application revision
-- first fails to deploy to any one instance. For example, for 9 instances,
-- deploy to one instance at a time. The overall deployment succeeds if all
-- 9 instances are successfully deployed to, and it fails if any of one of
-- the 9 instances fail to be deployed to. Note that the deployment may
-- successfully deploy to some instances, even if the overall deployment
-- fails. This is the default deployment configuration if a configuration
-- isn't specified for either the deployment or the deployment group. To
-- create a custom deployment configuration, call the create deployment
-- configuration operation.
cdgDeploymentConfigName :: Lens' CreateDeploymentGroup (Maybe Text)
cdgDeploymentConfigName =
    lens _cdgDeploymentConfigName (\s a -> s { _cdgDeploymentConfigName = a })

-- | The name of an existing deployment group for the specified application.
cdgDeploymentGroupName :: Lens' CreateDeploymentGroup Text
cdgDeploymentGroupName =
    lens _cdgDeploymentGroupName (\s a -> s { _cdgDeploymentGroupName = a })

-- | The Amazon EC2 tags to filter on.
cdgEc2TagFilters :: Lens' CreateDeploymentGroup [EC2TagFilter]
cdgEc2TagFilters = lens _cdgEc2TagFilters (\s a -> s { _cdgEc2TagFilters = a })

-- | A service role ARN that allows AWS CodeDeploy to act on the user's behalf
-- when interacting with AWS services.
cdgServiceRoleArn :: Lens' CreateDeploymentGroup (Maybe Text)
cdgServiceRoleArn =
    lens _cdgServiceRoleArn (\s a -> s { _cdgServiceRoleArn = a })

newtype CreateDeploymentGroupResponse = CreateDeploymentGroupResponse
    { _cdgrDeploymentGroupId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateDeploymentGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdgrDeploymentGroupId' @::@ 'Maybe' 'Text'
--
createDeploymentGroupResponse :: CreateDeploymentGroupResponse
createDeploymentGroupResponse = CreateDeploymentGroupResponse
    { _cdgrDeploymentGroupId = Nothing
    }

-- | A unique deployment group ID.
cdgrDeploymentGroupId :: Lens' CreateDeploymentGroupResponse (Maybe Text)
cdgrDeploymentGroupId =
    lens _cdgrDeploymentGroupId (\s a -> s { _cdgrDeploymentGroupId = a })

instance AWSRequest CreateDeploymentGroup where
    type Sv CreateDeploymentGroup = CodeDeploy
    type Rs CreateDeploymentGroup = CreateDeploymentGroupResponse

    request  = post
    response = jsonResponse

instance FromJSON CreateDeploymentGroupResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath CreateDeploymentGroup where
    toPath = const "/"

instance ToHeaders CreateDeploymentGroup

instance ToQuery CreateDeploymentGroup where
    toQuery = const mempty

instance ToJSON CreateDeploymentGroup where
    toJSON = genericToJSON jsonOptions
