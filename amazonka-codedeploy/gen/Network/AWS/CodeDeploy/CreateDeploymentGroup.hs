{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeploymentGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new deployment group for application revisions to be deployed
-- to.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeploymentGroup.html AWS API Reference> for CreateDeploymentGroup.
module Network.AWS.CodeDeploy.CreateDeploymentGroup
    (
    -- * Creating a Request
      createDeploymentGroup
    , CreateDeploymentGroup
    -- * Request Lenses
    , cdgDeploymentConfigName
    , cdgEc2TagFilters
    , cdgOnPremisesInstanceTagFilters
    , cdgAutoScalingGroups
    , cdgApplicationName
    , cdgDeploymentGroupName
    , cdgServiceRoleARN

    -- * Destructuring the Response
    , createDeploymentGroupResponse
    , CreateDeploymentGroupResponse
    -- * Response Lenses
    , cdgrsDeploymentGroupId
    , cdgrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create deployment group operation.
--
-- /See:/ 'createDeploymentGroup' smart constructor.
data CreateDeploymentGroup = CreateDeploymentGroup'
    { _cdgDeploymentConfigName         :: !(Maybe Text)
    , _cdgEc2TagFilters                :: !(Maybe [EC2TagFilter])
    , _cdgOnPremisesInstanceTagFilters :: !(Maybe [TagFilter])
    , _cdgAutoScalingGroups            :: !(Maybe [Text])
    , _cdgApplicationName              :: !Text
    , _cdgDeploymentGroupName          :: !Text
    , _cdgServiceRoleARN               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDeploymentGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdgDeploymentConfigName'
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
createDeploymentGroup
    :: Text -- ^ 'cdgApplicationName'
    -> Text -- ^ 'cdgDeploymentGroupName'
    -> Text -- ^ 'cdgServiceRoleARN'
    -> CreateDeploymentGroup
createDeploymentGroup pApplicationName_ pDeploymentGroupName_ pServiceRoleARN_ =
    CreateDeploymentGroup'
    { _cdgDeploymentConfigName = Nothing
    , _cdgEc2TagFilters = Nothing
    , _cdgOnPremisesInstanceTagFilters = Nothing
    , _cdgAutoScalingGroups = Nothing
    , _cdgApplicationName = pApplicationName_
    , _cdgDeploymentGroupName = pDeploymentGroupName_
    , _cdgServiceRoleARN = pServiceRoleARN_
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
cdgDeploymentConfigName :: Lens' CreateDeploymentGroup (Maybe Text)
cdgDeploymentConfigName = lens _cdgDeploymentConfigName (\ s a -> s{_cdgDeploymentConfigName = a});

-- | The Amazon EC2 tags to filter on.
cdgEc2TagFilters :: Lens' CreateDeploymentGroup [EC2TagFilter]
cdgEc2TagFilters = lens _cdgEc2TagFilters (\ s a -> s{_cdgEc2TagFilters = a}) . _Default . _Coerce;

-- | The on-premises instance tags to filter on.
cdgOnPremisesInstanceTagFilters :: Lens' CreateDeploymentGroup [TagFilter]
cdgOnPremisesInstanceTagFilters = lens _cdgOnPremisesInstanceTagFilters (\ s a -> s{_cdgOnPremisesInstanceTagFilters = a}) . _Default . _Coerce;

-- | A list of associated Auto Scaling groups.
cdgAutoScalingGroups :: Lens' CreateDeploymentGroup [Text]
cdgAutoScalingGroups = lens _cdgAutoScalingGroups (\ s a -> s{_cdgAutoScalingGroups = a}) . _Default . _Coerce;

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

instance AWSRequest CreateDeploymentGroup where
        type Rs CreateDeploymentGroup =
             CreateDeploymentGroupResponse
        request = postJSON codeDeploy
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
              (catMaybes
                 [("deploymentConfigName" .=) <$>
                    _cdgDeploymentConfigName,
                  ("ec2TagFilters" .=) <$> _cdgEc2TagFilters,
                  ("onPremisesInstanceTagFilters" .=) <$>
                    _cdgOnPremisesInstanceTagFilters,
                  ("autoScalingGroups" .=) <$> _cdgAutoScalingGroups,
                  Just ("applicationName" .= _cdgApplicationName),
                  Just
                    ("deploymentGroupName" .= _cdgDeploymentGroupName),
                  Just ("serviceRoleArn" .= _cdgServiceRoleARN)])

instance ToPath CreateDeploymentGroup where
        toPath = const "/"

instance ToQuery CreateDeploymentGroup where
        toQuery = const mempty

-- | Represents the output of a create deployment group operation.
--
-- /See:/ 'createDeploymentGroupResponse' smart constructor.
data CreateDeploymentGroupResponse = CreateDeploymentGroupResponse'
    { _cdgrsDeploymentGroupId :: !(Maybe Text)
    , _cdgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDeploymentGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdgrsDeploymentGroupId'
--
-- * 'cdgrsStatus'
createDeploymentGroupResponse
    :: Int -- ^ 'cdgrsStatus'
    -> CreateDeploymentGroupResponse
createDeploymentGroupResponse pStatus_ =
    CreateDeploymentGroupResponse'
    { _cdgrsDeploymentGroupId = Nothing
    , _cdgrsStatus = pStatus_
    }

-- | A unique deployment group ID.
cdgrsDeploymentGroupId :: Lens' CreateDeploymentGroupResponse (Maybe Text)
cdgrsDeploymentGroupId = lens _cdgrsDeploymentGroupId (\ s a -> s{_cdgrsDeploymentGroupId = a});

-- | The response status code.
cdgrsStatus :: Lens' CreateDeploymentGroupResponse Int
cdgrsStatus = lens _cdgrsStatus (\ s a -> s{_cdgrsStatus = a});
