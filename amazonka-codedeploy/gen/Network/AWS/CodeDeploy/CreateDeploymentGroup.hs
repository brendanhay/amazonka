{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeploymentGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new deployment group for application revisions to be deployed
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
    , cdgrqDeploymentConfigName
    , cdgrqEc2TagFilters
    , cdgrqOnPremisesInstanceTagFilters
    , cdgrqAutoScalingGroups
    , cdgrqApplicationName
    , cdgrqDeploymentGroupName
    , cdgrqServiceRoleARN

    -- * Response
    , CreateDeploymentGroupResponse
    -- ** Response constructor
    , createDeploymentGroupResponse
    -- ** Response lenses
    , cdgrsDeploymentGroupId
    , cdgrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create deployment group operation.
--
-- /See:/ 'createDeploymentGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdgrqDeploymentConfigName'
--
-- * 'cdgrqEc2TagFilters'
--
-- * 'cdgrqOnPremisesInstanceTagFilters'
--
-- * 'cdgrqAutoScalingGroups'
--
-- * 'cdgrqApplicationName'
--
-- * 'cdgrqDeploymentGroupName'
--
-- * 'cdgrqServiceRoleARN'
data CreateDeploymentGroup = CreateDeploymentGroup'
    { _cdgrqDeploymentConfigName         :: !(Maybe Text)
    , _cdgrqEc2TagFilters                :: !(Maybe [EC2TagFilter])
    , _cdgrqOnPremisesInstanceTagFilters :: !(Maybe [TagFilter])
    , _cdgrqAutoScalingGroups            :: !(Maybe [Text])
    , _cdgrqApplicationName              :: !Text
    , _cdgrqDeploymentGroupName          :: !Text
    , _cdgrqServiceRoleARN               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDeploymentGroup' smart constructor.
createDeploymentGroup :: Text -> Text -> Text -> CreateDeploymentGroup
createDeploymentGroup pApplicationName_ pDeploymentGroupName_ pServiceRoleARN_ =
    CreateDeploymentGroup'
    { _cdgrqDeploymentConfigName = Nothing
    , _cdgrqEc2TagFilters = Nothing
    , _cdgrqOnPremisesInstanceTagFilters = Nothing
    , _cdgrqAutoScalingGroups = Nothing
    , _cdgrqApplicationName = pApplicationName_
    , _cdgrqDeploymentGroupName = pDeploymentGroupName_
    , _cdgrqServiceRoleARN = pServiceRoleARN_
    }

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
cdgrqDeploymentConfigName :: Lens' CreateDeploymentGroup (Maybe Text)
cdgrqDeploymentConfigName = lens _cdgrqDeploymentConfigName (\ s a -> s{_cdgrqDeploymentConfigName = a});

-- | The Amazon EC2 tags to filter on.
cdgrqEc2TagFilters :: Lens' CreateDeploymentGroup [EC2TagFilter]
cdgrqEc2TagFilters = lens _cdgrqEc2TagFilters (\ s a -> s{_cdgrqEc2TagFilters = a}) . _Default;

-- | The on-premises instance tags to filter on.
cdgrqOnPremisesInstanceTagFilters :: Lens' CreateDeploymentGroup [TagFilter]
cdgrqOnPremisesInstanceTagFilters = lens _cdgrqOnPremisesInstanceTagFilters (\ s a -> s{_cdgrqOnPremisesInstanceTagFilters = a}) . _Default;

-- | A list of associated Auto Scaling groups.
cdgrqAutoScalingGroups :: Lens' CreateDeploymentGroup [Text]
cdgrqAutoScalingGroups = lens _cdgrqAutoScalingGroups (\ s a -> s{_cdgrqAutoScalingGroups = a}) . _Default;

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
cdgrqApplicationName :: Lens' CreateDeploymentGroup Text
cdgrqApplicationName = lens _cdgrqApplicationName (\ s a -> s{_cdgrqApplicationName = a});

-- | The name of an existing deployment group for the specified application.
cdgrqDeploymentGroupName :: Lens' CreateDeploymentGroup Text
cdgrqDeploymentGroupName = lens _cdgrqDeploymentGroupName (\ s a -> s{_cdgrqDeploymentGroupName = a});

-- | A service role ARN that allows AWS CodeDeploy to act on the user\'s
-- behalf when interacting with AWS services.
cdgrqServiceRoleARN :: Lens' CreateDeploymentGroup Text
cdgrqServiceRoleARN = lens _cdgrqServiceRoleARN (\ s a -> s{_cdgrqServiceRoleARN = a});

instance AWSRequest CreateDeploymentGroup where
        type Sv CreateDeploymentGroup = CodeDeploy
        type Rs CreateDeploymentGroup =
             CreateDeploymentGroupResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentGroupResponse' <$>
                   (x .?> "deploymentGroupId") <*> (pure (fromEnum s)))

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
              ["deploymentConfigName" .=
                 _cdgrqDeploymentConfigName,
               "ec2TagFilters" .= _cdgrqEc2TagFilters,
               "onPremisesInstanceTagFilters" .=
                 _cdgrqOnPremisesInstanceTagFilters,
               "autoScalingGroups" .= _cdgrqAutoScalingGroups,
               "applicationName" .= _cdgrqApplicationName,
               "deploymentGroupName" .= _cdgrqDeploymentGroupName,
               "serviceRoleArn" .= _cdgrqServiceRoleARN]

instance ToPath CreateDeploymentGroup where
        toPath = const "/"

instance ToQuery CreateDeploymentGroup where
        toQuery = const mempty

-- | Represents the output of a create deployment group operation.
--
-- /See:/ 'createDeploymentGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdgrsDeploymentGroupId'
--
-- * 'cdgrsStatus'
data CreateDeploymentGroupResponse = CreateDeploymentGroupResponse'
    { _cdgrsDeploymentGroupId :: !(Maybe Text)
    , _cdgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDeploymentGroupResponse' smart constructor.
createDeploymentGroupResponse :: Int -> CreateDeploymentGroupResponse
createDeploymentGroupResponse pStatus_ =
    CreateDeploymentGroupResponse'
    { _cdgrsDeploymentGroupId = Nothing
    , _cdgrsStatus = pStatus_
    }

-- | A unique deployment group ID.
cdgrsDeploymentGroupId :: Lens' CreateDeploymentGroupResponse (Maybe Text)
cdgrsDeploymentGroupId = lens _cdgrsDeploymentGroupId (\ s a -> s{_cdgrsDeploymentGroupId = a});

-- | FIXME: Undocumented member.
cdgrsStatus :: Lens' CreateDeploymentGroupResponse Int
cdgrsStatus = lens _cdgrsStatus (\ s a -> s{_cdgrsStatus = a});
