{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.CreateDeploymentGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new deployment group for application revisions to be deployed
-- to.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeploymentGroup.html>
module Network.AWS.CodeDeploy.CreateDeploymentGroup
    (
    -- * Request
      CreateDeploymentGroup
    -- ** Request constructor
    , createDeploymentGroup
    -- ** Request lenses
    , cdgEc2TagFilters
    , cdgOnPremisesInstanceTagFilters
    , cdgAutoScalingGroups
    , cdgApplicationName
    , cdgDeploymentGroupName
    , cdgServiceRoleARN
    , cdgDeploymentConfigName

    -- * Response
    , CreateDeploymentGroupResponse
    -- ** Response constructor
    , createDeploymentGroupResponse
    -- ** Response lenses
    , cdgrDeploymentGroupId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'createDeploymentGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdgEc2TagFilters'
--
-- * 'cdgOnPremisesInstanceTagFilters'
--
-- * 'cdgAutoScalingGroups'
--
-- * 'cdgApplicationName'
--
-- * 'cdgDeploymentGroupName'
--
-- * 'cdgServiceRoleARN'
--
-- * 'cdgDeploymentConfigName'
data CreateDeploymentGroup = CreateDeploymentGroup'{_cdgEc2TagFilters :: [EC2TagFilter], _cdgOnPremisesInstanceTagFilters :: [TagFilter], _cdgAutoScalingGroups :: [Text], _cdgApplicationName :: Text, _cdgDeploymentGroupName :: Text, _cdgServiceRoleARN :: Text, _cdgDeploymentConfigName :: Text} deriving (Eq, Read, Show)

-- | 'CreateDeploymentGroup' smart constructor.
createDeploymentGroup :: Text -> Text -> Text -> Text -> CreateDeploymentGroup
createDeploymentGroup pApplicationName pDeploymentGroupName pServiceRoleARN pDeploymentConfigName = CreateDeploymentGroup'{_cdgEc2TagFilters = mempty, _cdgOnPremisesInstanceTagFilters = mempty, _cdgAutoScalingGroups = mempty, _cdgApplicationName = pApplicationName, _cdgDeploymentGroupName = pDeploymentGroupName, _cdgServiceRoleARN = pServiceRoleARN, _cdgDeploymentConfigName = pDeploymentConfigName};

-- | The Amazon EC2 tags to filter on.
cdgEc2TagFilters :: Lens' CreateDeploymentGroup [EC2TagFilter]
cdgEc2TagFilters = lens _cdgEc2TagFilters (\ s a -> s{_cdgEc2TagFilters = a});

-- | The on-premises instance tags to filter on.
cdgOnPremisesInstanceTagFilters :: Lens' CreateDeploymentGroup [TagFilter]
cdgOnPremisesInstanceTagFilters = lens _cdgOnPremisesInstanceTagFilters (\ s a -> s{_cdgOnPremisesInstanceTagFilters = a});

-- | A list of associated Auto Scaling groups.
cdgAutoScalingGroups :: Lens' CreateDeploymentGroup [Text]
cdgAutoScalingGroups = lens _cdgAutoScalingGroups (\ s a -> s{_cdgAutoScalingGroups = a});

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
cdgApplicationName :: Lens' CreateDeploymentGroup Text
cdgApplicationName = lens _cdgApplicationName (\ s a -> s{_cdgApplicationName = a});

-- | The name of an existing deployment group for the specified application.
cdgDeploymentGroupName :: Lens' CreateDeploymentGroup Text
cdgDeploymentGroupName = lens _cdgDeploymentGroupName (\ s a -> s{_cdgDeploymentGroupName = a});

-- | A service role ARN that allows AWS CodeDeploy to act on the user\'s
-- behalf when interacting with AWS services.
cdgServiceRoleARN :: Lens' CreateDeploymentGroup Text
cdgServiceRoleARN = lens _cdgServiceRoleARN (\ s a -> s{_cdgServiceRoleARN = a});

-- | If specified, the deployment configuration name must be one of the
-- predefined values, or it can be a custom deployment configuration:
--
-- -   CodeDeployDefault.AllAtOnce deploys an application revision to up to
--     all of the instances at once. The overall deployment succeeds if the
--     application revision deploys to at least one of the instances. The
--     overall deployment fails after the application revision fails to
--     deploy to all of the instances. For example, for 9 instances, deploy
--     to up to all 9 instances at once. The overall deployment succeeds if
--     any of the 9 instances is successfully deployed to, and it fails if
--     all 9 instances fail to be deployed to.
-- -   CodeDeployDefault.HalfAtATime deploys to up to half of the instances
--     at a time (with fractions rounded down). The overall deployment
--     succeeds if the application revision deploys to at least half of the
--     instances (with fractions rounded up); otherwise, the deployment
--     fails. For example, for 9 instances, deploy to up to 4 instances at
--     a time. The overall deployment succeeds if 5 or more instances are
--     successfully deployed to; otherwise, the deployment fails. Note that
--     the deployment may successfully deploy to some instances, even if
--     the overall deployment fails.
-- -   CodeDeployDefault.OneAtATime deploys the application revision to
--     only one of the instances at a time. The overall deployment succeeds
--     if the application revision deploys to all of the instances. The
--     overall deployment fails after the application revision first fails
--     to deploy to any one instances. For example, for 9 instances, deploy
--     to one instance at a time. The overall deployment succeeds if all 9
--     instances are successfully deployed to, and it fails if any of one
--     of the 9 instances fail to be deployed to. Note that the deployment
--     may successfully deploy to some instances, even if the overall
--     deployment fails. This is the default deployment configuration if a
--     configuration isn\'t specified for either the deployment or the
--     deployment group.
--
-- To create a custom deployment configuration, call the create deployment
-- configuration operation.
cdgDeploymentConfigName :: Lens' CreateDeploymentGroup Text
cdgDeploymentConfigName = lens _cdgDeploymentConfigName (\ s a -> s{_cdgDeploymentConfigName = a});

instance AWSRequest CreateDeploymentGroup where
        type Sv CreateDeploymentGroup = CodeDeploy
        type Rs CreateDeploymentGroup =
             CreateDeploymentGroupResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentGroupResponse' <$>
                   x .?> "deploymentGroupId")

instance ToHeaders CreateDeploymentGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.CreateDeploymentGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDeploymentGroup where
        toJSON CreateDeploymentGroup'{..}
          = object
              ["ec2TagFilters" .= _cdgEc2TagFilters,
               "onPremisesInstanceTagFilters" .=
                 _cdgOnPremisesInstanceTagFilters,
               "autoScalingGroups" .= _cdgAutoScalingGroups,
               "applicationName" .= _cdgApplicationName,
               "deploymentGroupName" .= _cdgDeploymentGroupName,
               "serviceRoleArn" .= _cdgServiceRoleARN,
               "deploymentConfigName" .= _cdgDeploymentConfigName]

instance ToPath CreateDeploymentGroup where
        toPath = const "/"

instance ToQuery CreateDeploymentGroup where
        toQuery = const mempty

-- | /See:/ 'createDeploymentGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdgrDeploymentGroupId'
newtype CreateDeploymentGroupResponse = CreateDeploymentGroupResponse'{_cdgrDeploymentGroupId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateDeploymentGroupResponse' smart constructor.
createDeploymentGroupResponse :: CreateDeploymentGroupResponse
createDeploymentGroupResponse = CreateDeploymentGroupResponse'{_cdgrDeploymentGroupId = Nothing};

-- | A unique deployment group ID.
cdgrDeploymentGroupId :: Lens' CreateDeploymentGroupResponse (Maybe Text)
cdgrDeploymentGroupId = lens _cdgrDeploymentGroupId (\ s a -> s{_cdgrDeploymentGroupId = a});
