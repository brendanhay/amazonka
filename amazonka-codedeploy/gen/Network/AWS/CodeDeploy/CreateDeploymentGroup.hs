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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment group to which application revisions will be deployed.
--
--
module Network.AWS.CodeDeploy.CreateDeploymentGroup
    (
    -- * Creating a Request
      createDeploymentGroup
    , CreateDeploymentGroup
    -- * Request Lenses
    , cdgEc2TagSet
    , cdgDeploymentConfigName
    , cdgOnPremisesTagSet
    , cdgEc2TagFilters
    , cdgBlueGreenDeploymentConfiguration
    , cdgLoadBalancerInfo
    , cdgOnPremisesInstanceTagFilters
    , cdgAlarmConfiguration
    , cdgTriggerConfigurations
    , cdgAutoScalingGroups
    , cdgDeploymentStyle
    , cdgAutoRollbackConfiguration
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

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a CreateDeploymentGroup operation.
--
--
--
-- /See:/ 'createDeploymentGroup' smart constructor.
data CreateDeploymentGroup = CreateDeploymentGroup'
  { _cdgEc2TagSet :: !(Maybe EC2TagSet)
  , _cdgDeploymentConfigName :: !(Maybe Text)
  , _cdgOnPremisesTagSet :: !(Maybe OnPremisesTagSet)
  , _cdgEc2TagFilters :: !(Maybe [EC2TagFilter])
  , _cdgBlueGreenDeploymentConfiguration :: !(Maybe BlueGreenDeploymentConfiguration)
  , _cdgLoadBalancerInfo :: !(Maybe LoadBalancerInfo)
  , _cdgOnPremisesInstanceTagFilters :: !(Maybe [TagFilter])
  , _cdgAlarmConfiguration :: !(Maybe AlarmConfiguration)
  , _cdgTriggerConfigurations :: !(Maybe [TriggerConfig])
  , _cdgAutoScalingGroups :: !(Maybe [Text])
  , _cdgDeploymentStyle :: !(Maybe DeploymentStyle)
  , _cdgAutoRollbackConfiguration :: !(Maybe AutoRollbackConfiguration)
  , _cdgApplicationName :: !Text
  , _cdgDeploymentGroupName :: !Text
  , _cdgServiceRoleARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeploymentGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdgEc2TagSet' - Information about groups of tags applied to EC2 instances. The deployment group will include only EC2 instances identified by all the tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- * 'cdgDeploymentConfigName' - If specified, the deployment configuration name can be either one of the predefined configurations provided with AWS CodeDeploy or a custom deployment configuration that you create by calling the create deployment configuration operation. CodeDeployDefault.OneAtATime is the default deployment configuration. It is used if a configuration isn't specified for the deployment or the deployment group. For more information about the predefined deployment configurations in AWS CodeDeploy, see <http://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Groups in AWS CodeDeploy> in the AWS CodeDeploy User Guide.
--
-- * 'cdgOnPremisesTagSet' - Information about groups of tags applied to on-premises instances. The deployment group will include only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
--
-- * 'cdgEc2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group will include EC2 instances with any of the specified tags. Cannot be used in the same call as ec2TagSet.
--
-- * 'cdgBlueGreenDeploymentConfiguration' - Information about blue/green deployment options for a deployment group.
--
-- * 'cdgLoadBalancerInfo' - Information about the load balancer used in a deployment.
--
-- * 'cdgOnPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group will include on-premises instances with any of the specified tags. Cannot be used in the same call as OnPremisesTagSet.
--
-- * 'cdgAlarmConfiguration' - Information to add about Amazon CloudWatch alarms when the deployment group is created.
--
-- * 'cdgTriggerConfigurations' - Information about triggers to create when the deployment group is created. For examples, see <http://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event> in the AWS CodeDeploy User Guide.
--
-- * 'cdgAutoScalingGroups' - A list of associated Auto Scaling groups.
--
-- * 'cdgDeploymentStyle' - Information about the type of deployment, in-place or blue/green, that you want to run and whether to route deployment traffic behind a load balancer.
--
-- * 'cdgAutoRollbackConfiguration' - Configuration information for an automatic rollback that is added when a deployment group is created.
--
-- * 'cdgApplicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
--
-- * 'cdgDeploymentGroupName' - The name of a new deployment group for the specified application.
--
-- * 'cdgServiceRoleARN' - A service role ARN that allows AWS CodeDeploy to act on the user's behalf when interacting with AWS services.
createDeploymentGroup
    :: Text -- ^ 'cdgApplicationName'
    -> Text -- ^ 'cdgDeploymentGroupName'
    -> Text -- ^ 'cdgServiceRoleARN'
    -> CreateDeploymentGroup
createDeploymentGroup pApplicationName_ pDeploymentGroupName_ pServiceRoleARN_ =
  CreateDeploymentGroup'
    { _cdgEc2TagSet = Nothing
    , _cdgDeploymentConfigName = Nothing
    , _cdgOnPremisesTagSet = Nothing
    , _cdgEc2TagFilters = Nothing
    , _cdgBlueGreenDeploymentConfiguration = Nothing
    , _cdgLoadBalancerInfo = Nothing
    , _cdgOnPremisesInstanceTagFilters = Nothing
    , _cdgAlarmConfiguration = Nothing
    , _cdgTriggerConfigurations = Nothing
    , _cdgAutoScalingGroups = Nothing
    , _cdgDeploymentStyle = Nothing
    , _cdgAutoRollbackConfiguration = Nothing
    , _cdgApplicationName = pApplicationName_
    , _cdgDeploymentGroupName = pDeploymentGroupName_
    , _cdgServiceRoleARN = pServiceRoleARN_
    }


-- | Information about groups of tags applied to EC2 instances. The deployment group will include only EC2 instances identified by all the tag groups. Cannot be used in the same call as ec2TagFilters.
cdgEc2TagSet :: Lens' CreateDeploymentGroup (Maybe EC2TagSet)
cdgEc2TagSet = lens _cdgEc2TagSet (\ s a -> s{_cdgEc2TagSet = a})

-- | If specified, the deployment configuration name can be either one of the predefined configurations provided with AWS CodeDeploy or a custom deployment configuration that you create by calling the create deployment configuration operation. CodeDeployDefault.OneAtATime is the default deployment configuration. It is used if a configuration isn't specified for the deployment or the deployment group. For more information about the predefined deployment configurations in AWS CodeDeploy, see <http://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Groups in AWS CodeDeploy> in the AWS CodeDeploy User Guide.
cdgDeploymentConfigName :: Lens' CreateDeploymentGroup (Maybe Text)
cdgDeploymentConfigName = lens _cdgDeploymentConfigName (\ s a -> s{_cdgDeploymentConfigName = a})

-- | Information about groups of tags applied to on-premises instances. The deployment group will include only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
cdgOnPremisesTagSet :: Lens' CreateDeploymentGroup (Maybe OnPremisesTagSet)
cdgOnPremisesTagSet = lens _cdgOnPremisesTagSet (\ s a -> s{_cdgOnPremisesTagSet = a})

-- | The Amazon EC2 tags on which to filter. The deployment group will include EC2 instances with any of the specified tags. Cannot be used in the same call as ec2TagSet.
cdgEc2TagFilters :: Lens' CreateDeploymentGroup [EC2TagFilter]
cdgEc2TagFilters = lens _cdgEc2TagFilters (\ s a -> s{_cdgEc2TagFilters = a}) . _Default . _Coerce

-- | Information about blue/green deployment options for a deployment group.
cdgBlueGreenDeploymentConfiguration :: Lens' CreateDeploymentGroup (Maybe BlueGreenDeploymentConfiguration)
cdgBlueGreenDeploymentConfiguration = lens _cdgBlueGreenDeploymentConfiguration (\ s a -> s{_cdgBlueGreenDeploymentConfiguration = a})

-- | Information about the load balancer used in a deployment.
cdgLoadBalancerInfo :: Lens' CreateDeploymentGroup (Maybe LoadBalancerInfo)
cdgLoadBalancerInfo = lens _cdgLoadBalancerInfo (\ s a -> s{_cdgLoadBalancerInfo = a})

-- | The on-premises instance tags on which to filter. The deployment group will include on-premises instances with any of the specified tags. Cannot be used in the same call as OnPremisesTagSet.
cdgOnPremisesInstanceTagFilters :: Lens' CreateDeploymentGroup [TagFilter]
cdgOnPremisesInstanceTagFilters = lens _cdgOnPremisesInstanceTagFilters (\ s a -> s{_cdgOnPremisesInstanceTagFilters = a}) . _Default . _Coerce

-- | Information to add about Amazon CloudWatch alarms when the deployment group is created.
cdgAlarmConfiguration :: Lens' CreateDeploymentGroup (Maybe AlarmConfiguration)
cdgAlarmConfiguration = lens _cdgAlarmConfiguration (\ s a -> s{_cdgAlarmConfiguration = a})

-- | Information about triggers to create when the deployment group is created. For examples, see <http://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event> in the AWS CodeDeploy User Guide.
cdgTriggerConfigurations :: Lens' CreateDeploymentGroup [TriggerConfig]
cdgTriggerConfigurations = lens _cdgTriggerConfigurations (\ s a -> s{_cdgTriggerConfigurations = a}) . _Default . _Coerce

-- | A list of associated Auto Scaling groups.
cdgAutoScalingGroups :: Lens' CreateDeploymentGroup [Text]
cdgAutoScalingGroups = lens _cdgAutoScalingGroups (\ s a -> s{_cdgAutoScalingGroups = a}) . _Default . _Coerce

-- | Information about the type of deployment, in-place or blue/green, that you want to run and whether to route deployment traffic behind a load balancer.
cdgDeploymentStyle :: Lens' CreateDeploymentGroup (Maybe DeploymentStyle)
cdgDeploymentStyle = lens _cdgDeploymentStyle (\ s a -> s{_cdgDeploymentStyle = a})

-- | Configuration information for an automatic rollback that is added when a deployment group is created.
cdgAutoRollbackConfiguration :: Lens' CreateDeploymentGroup (Maybe AutoRollbackConfiguration)
cdgAutoRollbackConfiguration = lens _cdgAutoRollbackConfiguration (\ s a -> s{_cdgAutoRollbackConfiguration = a})

-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
cdgApplicationName :: Lens' CreateDeploymentGroup Text
cdgApplicationName = lens _cdgApplicationName (\ s a -> s{_cdgApplicationName = a})

-- | The name of a new deployment group for the specified application.
cdgDeploymentGroupName :: Lens' CreateDeploymentGroup Text
cdgDeploymentGroupName = lens _cdgDeploymentGroupName (\ s a -> s{_cdgDeploymentGroupName = a})

-- | A service role ARN that allows AWS CodeDeploy to act on the user's behalf when interacting with AWS services.
cdgServiceRoleARN :: Lens' CreateDeploymentGroup Text
cdgServiceRoleARN = lens _cdgServiceRoleARN (\ s a -> s{_cdgServiceRoleARN = a})

instance AWSRequest CreateDeploymentGroup where
        type Rs CreateDeploymentGroup =
             CreateDeploymentGroupResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentGroupResponse' <$>
                   (x .?> "deploymentGroupId") <*> (pure (fromEnum s)))

instance Hashable CreateDeploymentGroup where

instance NFData CreateDeploymentGroup where

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
                 [("ec2TagSet" .=) <$> _cdgEc2TagSet,
                  ("deploymentConfigName" .=) <$>
                    _cdgDeploymentConfigName,
                  ("onPremisesTagSet" .=) <$> _cdgOnPremisesTagSet,
                  ("ec2TagFilters" .=) <$> _cdgEc2TagFilters,
                  ("blueGreenDeploymentConfiguration" .=) <$>
                    _cdgBlueGreenDeploymentConfiguration,
                  ("loadBalancerInfo" .=) <$> _cdgLoadBalancerInfo,
                  ("onPremisesInstanceTagFilters" .=) <$>
                    _cdgOnPremisesInstanceTagFilters,
                  ("alarmConfiguration" .=) <$> _cdgAlarmConfiguration,
                  ("triggerConfigurations" .=) <$>
                    _cdgTriggerConfigurations,
                  ("autoScalingGroups" .=) <$> _cdgAutoScalingGroups,
                  ("deploymentStyle" .=) <$> _cdgDeploymentStyle,
                  ("autoRollbackConfiguration" .=) <$>
                    _cdgAutoRollbackConfiguration,
                  Just ("applicationName" .= _cdgApplicationName),
                  Just
                    ("deploymentGroupName" .= _cdgDeploymentGroupName),
                  Just ("serviceRoleArn" .= _cdgServiceRoleARN)])

instance ToPath CreateDeploymentGroup where
        toPath = const "/"

instance ToQuery CreateDeploymentGroup where
        toQuery = const mempty

-- | Represents the output of a CreateDeploymentGroup operation.
--
--
--
-- /See:/ 'createDeploymentGroupResponse' smart constructor.
data CreateDeploymentGroupResponse = CreateDeploymentGroupResponse'
  { _cdgrsDeploymentGroupId :: !(Maybe Text)
  , _cdgrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeploymentGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdgrsDeploymentGroupId' - A unique deployment group ID.
--
-- * 'cdgrsResponseStatus' - -- | The response status code.
createDeploymentGroupResponse
    :: Int -- ^ 'cdgrsResponseStatus'
    -> CreateDeploymentGroupResponse
createDeploymentGroupResponse pResponseStatus_ =
  CreateDeploymentGroupResponse'
    {_cdgrsDeploymentGroupId = Nothing, _cdgrsResponseStatus = pResponseStatus_}


-- | A unique deployment group ID.
cdgrsDeploymentGroupId :: Lens' CreateDeploymentGroupResponse (Maybe Text)
cdgrsDeploymentGroupId = lens _cdgrsDeploymentGroupId (\ s a -> s{_cdgrsDeploymentGroupId = a})

-- | -- | The response status code.
cdgrsResponseStatus :: Lens' CreateDeploymentGroupResponse Int
cdgrsResponseStatus = lens _cdgrsResponseStatus (\ s a -> s{_cdgrsResponseStatus = a})

instance NFData CreateDeploymentGroupResponse where
