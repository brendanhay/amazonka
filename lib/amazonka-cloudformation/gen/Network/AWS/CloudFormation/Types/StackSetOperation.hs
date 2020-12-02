{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperation where

import Network.AWS.CloudFormation.Types.DeploymentTargets
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetOperationAction
import Network.AWS.CloudFormation.Types.StackSetOperationPreferences
import Network.AWS.CloudFormation.Types.StackSetOperationStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structure that contains information about a stack set operation.
--
--
--
-- /See:/ 'stackSetOperation' smart constructor.
data StackSetOperation = StackSetOperation'
  { _ssoStackSetDriftDetectionDetails ::
      !(Maybe StackSetDriftDetectionDetails),
    _ssoStatus :: !(Maybe StackSetOperationStatus),
    _ssoAdministrationRoleARN :: !(Maybe Text),
    _ssoAction :: !(Maybe StackSetOperationAction),
    _ssoEndTimestamp :: !(Maybe ISO8601),
    _ssoCreationTimestamp :: !(Maybe ISO8601),
    _ssoOperationPreferences ::
      !(Maybe StackSetOperationPreferences),
    _ssoOperationId :: !(Maybe Text),
    _ssoRetainStacks :: !(Maybe Bool),
    _ssoDeploymentTargets :: !(Maybe DeploymentTargets),
    _ssoStackSetId :: !(Maybe Text),
    _ssoExecutionRoleName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSetOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssoStackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set. This includes information about drift operations currently being performed on the stack set. this information will only be present for stack set operations whose @Action@ type is @DETECT_DRIFT@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the AWS CloudFormation User Guide.
--
-- * 'ssoStatus' - The status of the operation.      * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each Region during stack create and update operations. If the number of failed stacks within a Region exceeds the failure tolerance, the status of the operation in the Region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining Regions.     * @QUEUED@ : [@Service-managed@ permissions] For automatic deployments that require a sequence of operations, the operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.     * @RUNNING@ : The operation is currently being performed.     * @STOPPED@ : The user has cancelled the operation.     * @STOPPING@ : The operation is in the process of stopping, at user request.      * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
--
-- * 'ssoAdministrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role used to perform this stack set operation.  Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
--
-- * 'ssoAction' - The type of stack set operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack set instances that are associated with the specified stack set. Update operations affect both the stack set itself, as well as /all/ associated stack set instances.
--
-- * 'ssoEndTimestamp' - The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
--
-- * 'ssoCreationTimestamp' - The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
--
-- * 'ssoOperationPreferences' - The preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'ssoOperationId' - The unique ID of a stack set operation.
--
-- * 'ssoRetainStacks' - For stack set operations of action type @DELETE@ , specifies whether to remove the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack, or add an existing, saved stack to a new stack set.
--
-- * 'ssoDeploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts affected by the stack operation.
--
-- * 'ssoStackSetId' - The ID of the stack set.
--
-- * 'ssoExecutionRoleName' - The name of the IAM execution role used to create or update the stack set. Use customized execution roles to control which stack resources users and groups can include in their stack sets.
stackSetOperation ::
  StackSetOperation
stackSetOperation =
  StackSetOperation'
    { _ssoStackSetDriftDetectionDetails = Nothing,
      _ssoStatus = Nothing,
      _ssoAdministrationRoleARN = Nothing,
      _ssoAction = Nothing,
      _ssoEndTimestamp = Nothing,
      _ssoCreationTimestamp = Nothing,
      _ssoOperationPreferences = Nothing,
      _ssoOperationId = Nothing,
      _ssoRetainStacks = Nothing,
      _ssoDeploymentTargets = Nothing,
      _ssoStackSetId = Nothing,
      _ssoExecutionRoleName = Nothing
    }

-- | Detailed information about the drift status of the stack set. This includes information about drift operations currently being performed on the stack set. this information will only be present for stack set operations whose @Action@ type is @DETECT_DRIFT@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the AWS CloudFormation User Guide.
ssoStackSetDriftDetectionDetails :: Lens' StackSetOperation (Maybe StackSetDriftDetectionDetails)
ssoStackSetDriftDetectionDetails = lens _ssoStackSetDriftDetectionDetails (\s a -> s {_ssoStackSetDriftDetectionDetails = a})

-- | The status of the operation.      * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each Region during stack create and update operations. If the number of failed stacks within a Region exceeds the failure tolerance, the status of the operation in the Region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining Regions.     * @QUEUED@ : [@Service-managed@ permissions] For automatic deployments that require a sequence of operations, the operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.     * @RUNNING@ : The operation is currently being performed.     * @STOPPED@ : The user has cancelled the operation.     * @STOPPING@ : The operation is in the process of stopping, at user request.      * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
ssoStatus :: Lens' StackSetOperation (Maybe StackSetOperationStatus)
ssoStatus = lens _ssoStatus (\s a -> s {_ssoStatus = a})

-- | The Amazon Resource Number (ARN) of the IAM role used to perform this stack set operation.  Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
ssoAdministrationRoleARN :: Lens' StackSetOperation (Maybe Text)
ssoAdministrationRoleARN = lens _ssoAdministrationRoleARN (\s a -> s {_ssoAdministrationRoleARN = a})

-- | The type of stack set operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack set instances that are associated with the specified stack set. Update operations affect both the stack set itself, as well as /all/ associated stack set instances.
ssoAction :: Lens' StackSetOperation (Maybe StackSetOperationAction)
ssoAction = lens _ssoAction (\s a -> s {_ssoAction = a})

-- | The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
ssoEndTimestamp :: Lens' StackSetOperation (Maybe UTCTime)
ssoEndTimestamp = lens _ssoEndTimestamp (\s a -> s {_ssoEndTimestamp = a}) . mapping _Time

-- | The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
ssoCreationTimestamp :: Lens' StackSetOperation (Maybe UTCTime)
ssoCreationTimestamp = lens _ssoCreationTimestamp (\s a -> s {_ssoCreationTimestamp = a}) . mapping _Time

-- | The preferences for how AWS CloudFormation performs this stack set operation.
ssoOperationPreferences :: Lens' StackSetOperation (Maybe StackSetOperationPreferences)
ssoOperationPreferences = lens _ssoOperationPreferences (\s a -> s {_ssoOperationPreferences = a})

-- | The unique ID of a stack set operation.
ssoOperationId :: Lens' StackSetOperation (Maybe Text)
ssoOperationId = lens _ssoOperationId (\s a -> s {_ssoOperationId = a})

-- | For stack set operations of action type @DELETE@ , specifies whether to remove the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack, or add an existing, saved stack to a new stack set.
ssoRetainStacks :: Lens' StackSetOperation (Maybe Bool)
ssoRetainStacks = lens _ssoRetainStacks (\s a -> s {_ssoRetainStacks = a})

-- | [@Service-managed@ permissions] The AWS Organizations accounts affected by the stack operation.
ssoDeploymentTargets :: Lens' StackSetOperation (Maybe DeploymentTargets)
ssoDeploymentTargets = lens _ssoDeploymentTargets (\s a -> s {_ssoDeploymentTargets = a})

-- | The ID of the stack set.
ssoStackSetId :: Lens' StackSetOperation (Maybe Text)
ssoStackSetId = lens _ssoStackSetId (\s a -> s {_ssoStackSetId = a})

-- | The name of the IAM execution role used to create or update the stack set. Use customized execution roles to control which stack resources users and groups can include in their stack sets.
ssoExecutionRoleName :: Lens' StackSetOperation (Maybe Text)
ssoExecutionRoleName = lens _ssoExecutionRoleName (\s a -> s {_ssoExecutionRoleName = a})

instance FromXML StackSetOperation where
  parseXML x =
    StackSetOperation'
      <$> (x .@? "StackSetDriftDetectionDetails")
      <*> (x .@? "Status")
      <*> (x .@? "AdministrationRoleARN")
      <*> (x .@? "Action")
      <*> (x .@? "EndTimestamp")
      <*> (x .@? "CreationTimestamp")
      <*> (x .@? "OperationPreferences")
      <*> (x .@? "OperationId")
      <*> (x .@? "RetainStacks")
      <*> (x .@? "DeploymentTargets")
      <*> (x .@? "StackSetId")
      <*> (x .@? "ExecutionRoleName")

instance Hashable StackSetOperation

instance NFData StackSetOperation
