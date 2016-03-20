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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment group to which application revisions will be
-- deployed.
module Network.AWS.CodeDeploy.CreateDeploymentGroup
    (
    -- * Creating a Request
      createDeploymentGroup
    , CreateDeploymentGroup
    -- * Request Lenses
    , cdgDeploymentConfigName
    , cdgEc2TagFilters
    , cdgOnPremisesInstanceTagFilters
    , cdgTriggerConfigurations
    , cdgAutoScalingGroups
    , cdgApplicationName
    , cdgDeploymentGroupName
    , cdgServiceRoleARN

    -- * Destructuring the Response
    , createDeploymentGroupResponse
    , CreateDeploymentGroupResponse
    -- * Response Lenses
    , cdgrsDeploymentGroupId
    , cdgrsResponseStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Lens
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
    , _cdgTriggerConfigurations        :: !(Maybe [TriggerConfig])
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
-- * 'cdgTriggerConfigurations'
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
    , _cdgTriggerConfigurations = Nothing
    , _cdgAutoScalingGroups = Nothing
    , _cdgApplicationName = pApplicationName_
    , _cdgDeploymentGroupName = pDeploymentGroupName_
    , _cdgServiceRoleARN = pServiceRoleARN_
    }

-- | If specified, the deployment configuration name can be either one of the
-- predefined configurations provided with AWS CodeDeploy or a custom
-- deployment configuration that you create by calling the create
-- deployment configuration operation.
--
-- CodeDeployDefault.OneAtATime is the default deployment configuration. It
-- is used if a configuration isn\'t specified for the deployment or the
-- deployment group.
--
-- The predefined deployment configurations include the following:
--
-- -   __CodeDeployDefault.AllAtOnce__ attempts to deploy an application
--     revision to as many instance as possible at once. The status of the
--     overall deployment will be displayed as __Succeeded__ if the
--     application revision is deployed to one or more of the instances.
--     The status of the overall deployment will be displayed as __Failed__
--     if the application revision is not deployed to any of the instances.
--     Using an example of nine instance, CodeDeployDefault.AllAtOnce will
--     attempt to deploy to all nine instance at once. The overall
--     deployment will succeed if deployment to even a single instance is
--     successful; it will fail only if deployments to all nine instance
--     fail.
--
-- -   __CodeDeployDefault.HalfAtATime__ deploys to up to half of the
--     instances at a time (with fractions rounded down). The overall
--     deployment succeeds if the application revision is deployed to at
--     least half of the instances (with fractions rounded up); otherwise,
--     the deployment fails. In the example of nine instances, it will
--     deploy to up to four instance at a time. The overall deployment
--     succeeds if deployment to five or more instances succeed; otherwise,
--     the deployment fails. The deployment may be successfully deployed to
--     some instances even if the overall deployment fails.
--
-- -   __CodeDeployDefault.OneAtATime__ deploys the application revision to
--     only one instance at a time.
--
--     For deployment groups that contain more than one instance:
--
--     -   The overall deployment succeeds if the application revision is
--         deployed to all of the instances. The exception to this rule is
--         if deployment to the last instance fails, the overall deployment
--         still succeeds. This is because AWS CodeDeploy allows only one
--         instance at a time to be taken offline with the
--         CodeDeployDefault.OneAtATime configuration.
--
--     -   The overall deployment fails as soon as the application revision
--         fails to be deployed to any but the last instance. The
--         deployment may be successfully deployed to some instances even
--         if the overall deployment fails.
--
--     -   In an example using nine instance, it will deploy to one
--         instance at a time. The overall deployment succeeds if
--         deployment to the first eight instance is successful; the
--         overall deployment fails if deployment to any of the first eight
--         instance fails.
--
--     For deployment groups that contain only one instance, the overall
--     deployment is successful only if deployment to the single instance
--     is successful
--
cdgDeploymentConfigName :: Lens' CreateDeploymentGroup (Maybe Text)
cdgDeploymentConfigName = lens _cdgDeploymentConfigName (\ s a -> s{_cdgDeploymentConfigName = a});

-- | The Amazon EC2 tags on which to filter.
cdgEc2TagFilters :: Lens' CreateDeploymentGroup [EC2TagFilter]
cdgEc2TagFilters = lens _cdgEc2TagFilters (\ s a -> s{_cdgEc2TagFilters = a}) . _Default . _Coerce;

-- | The on-premises instance tags on which to filter.
cdgOnPremisesInstanceTagFilters :: Lens' CreateDeploymentGroup [TagFilter]
cdgOnPremisesInstanceTagFilters = lens _cdgOnPremisesInstanceTagFilters (\ s a -> s{_cdgOnPremisesInstanceTagFilters = a}) . _Default . _Coerce;

-- | Information about triggers to create when the deployment group is
-- created.
cdgTriggerConfigurations :: Lens' CreateDeploymentGroup [TriggerConfig]
cdgTriggerConfigurations = lens _cdgTriggerConfigurations (\ s a -> s{_cdgTriggerConfigurations = a}) . _Default . _Coerce;

-- | A list of associated Auto Scaling groups.
cdgAutoScalingGroups :: Lens' CreateDeploymentGroup [Text]
cdgAutoScalingGroups = lens _cdgAutoScalingGroups (\ s a -> s{_cdgAutoScalingGroups = a}) . _Default . _Coerce;

-- | The name of an AWS CodeDeploy application associated with the applicable
-- IAM user or AWS account.
cdgApplicationName :: Lens' CreateDeploymentGroup Text
cdgApplicationName = lens _cdgApplicationName (\ s a -> s{_cdgApplicationName = a});

-- | The name of a new deployment group for the specified application.
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

instance Hashable CreateDeploymentGroup

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
                  ("triggerConfigurations" .=) <$>
                    _cdgTriggerConfigurations,
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
    , _cdgrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDeploymentGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdgrsDeploymentGroupId'
--
-- * 'cdgrsResponseStatus'
createDeploymentGroupResponse
    :: Int -- ^ 'cdgrsResponseStatus'
    -> CreateDeploymentGroupResponse
createDeploymentGroupResponse pResponseStatus_ =
    CreateDeploymentGroupResponse'
    { _cdgrsDeploymentGroupId = Nothing
    , _cdgrsResponseStatus = pResponseStatus_
    }

-- | A unique deployment group ID.
cdgrsDeploymentGroupId :: Lens' CreateDeploymentGroupResponse (Maybe Text)
cdgrsDeploymentGroupId = lens _cdgrsDeploymentGroupId (\ s a -> s{_cdgrsDeploymentGroupId = a});

-- | The response status code.
cdgrsResponseStatus :: Lens' CreateDeploymentGroupResponse Int
cdgrsResponseStatus = lens _cdgrsResponseStatus (\ s a -> s{_cdgrsResponseStatus = a});
