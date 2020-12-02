{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Stack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Stack where

import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.Output
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.RollbackConfiguration
import Network.AWS.CloudFormation.Types.StackDriftInformation
import Network.AWS.CloudFormation.Types.StackStatus
import Network.AWS.CloudFormation.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Stack data type.
--
--
--
-- /See:/ 'stack' smart constructor.
data Stack = Stack'
  { _staDisableRollback :: !(Maybe Bool),
    _staLastUpdatedTime :: !(Maybe ISO8601),
    _staRootId :: !(Maybe Text),
    _staNotificationARNs :: !(Maybe [Text]),
    _staStackStatusReason :: !(Maybe Text),
    _staEnableTerminationProtection :: !(Maybe Bool),
    _staDriftInformation :: !(Maybe StackDriftInformation),
    _staChangeSetId :: !(Maybe Text),
    _staDeletionTime :: !(Maybe ISO8601),
    _staOutputs :: !(Maybe [Output]),
    _staParameters :: !(Maybe [Parameter]),
    _staStackId :: !(Maybe Text),
    _staDescription :: !(Maybe Text),
    _staCapabilities :: !(Maybe [Capability]),
    _staRollbackConfiguration :: !(Maybe RollbackConfiguration),
    _staTags :: !(Maybe [Tag]),
    _staTimeoutInMinutes :: !(Maybe Nat),
    _staParentId :: !(Maybe Text),
    _staRoleARN :: !(Maybe Text),
    _staStackName :: !Text,
    _staCreationTime :: !ISO8601,
    _staStackStatus :: !StackStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staDisableRollback' - Boolean to enable or disable rollback on stack creation failures:     * @true@ : disable rollback     * @false@ : enable rollback
--
-- * 'staLastUpdatedTime' - The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- * 'staRootId' - For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- * 'staNotificationARNs' - SNS topic ARNs to which stack related events are published.
--
-- * 'staStackStatusReason' - Success/failure message associated with the stack status.
--
-- * 'staEnableTerminationProtection' - Whether termination protection is enabled for the stack. For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
--
-- * 'staDriftInformation' - Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'staChangeSetId' - The unique ID of the change set.
--
-- * 'staDeletionTime' - The time the stack was deleted.
--
-- * 'staOutputs' - A list of output structures.
--
-- * 'staParameters' - A list of @Parameter@ structures.
--
-- * 'staStackId' - Unique identifier of the stack.
--
-- * 'staDescription' - A user-defined description associated with the stack.
--
-- * 'staCapabilities' - The capabilities allowed in the stack.
--
-- * 'staRollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- * 'staTags' - A list of @Tag@ s that specify information about the stack.
--
-- * 'staTimeoutInMinutes' - The amount of time within which stack creation should complete.
--
-- * 'staParentId' - For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- * 'staRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
--
-- * 'staStackName' - The name associated with the stack.
--
-- * 'staCreationTime' - The time at which the stack was created.
--
-- * 'staStackStatus' - Current status of the stack.
stack ::
  -- | 'staStackName'
  Text ->
  -- | 'staCreationTime'
  UTCTime ->
  -- | 'staStackStatus'
  StackStatus ->
  Stack
stack pStackName_ pCreationTime_ pStackStatus_ =
  Stack'
    { _staDisableRollback = Nothing,
      _staLastUpdatedTime = Nothing,
      _staRootId = Nothing,
      _staNotificationARNs = Nothing,
      _staStackStatusReason = Nothing,
      _staEnableTerminationProtection = Nothing,
      _staDriftInformation = Nothing,
      _staChangeSetId = Nothing,
      _staDeletionTime = Nothing,
      _staOutputs = Nothing,
      _staParameters = Nothing,
      _staStackId = Nothing,
      _staDescription = Nothing,
      _staCapabilities = Nothing,
      _staRollbackConfiguration = Nothing,
      _staTags = Nothing,
      _staTimeoutInMinutes = Nothing,
      _staParentId = Nothing,
      _staRoleARN = Nothing,
      _staStackName = pStackName_,
      _staCreationTime = _Time # pCreationTime_,
      _staStackStatus = pStackStatus_
    }

-- | Boolean to enable or disable rollback on stack creation failures:     * @true@ : disable rollback     * @false@ : enable rollback
staDisableRollback :: Lens' Stack (Maybe Bool)
staDisableRollback = lens _staDisableRollback (\s a -> s {_staDisableRollback = a})

-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
staLastUpdatedTime :: Lens' Stack (Maybe UTCTime)
staLastUpdatedTime = lens _staLastUpdatedTime (\s a -> s {_staLastUpdatedTime = a}) . mapping _Time

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
staRootId :: Lens' Stack (Maybe Text)
staRootId = lens _staRootId (\s a -> s {_staRootId = a})

-- | SNS topic ARNs to which stack related events are published.
staNotificationARNs :: Lens' Stack [Text]
staNotificationARNs = lens _staNotificationARNs (\s a -> s {_staNotificationARNs = a}) . _Default . _Coerce

-- | Success/failure message associated with the stack status.
staStackStatusReason :: Lens' Stack (Maybe Text)
staStackStatusReason = lens _staStackStatusReason (\s a -> s {_staStackStatusReason = a})

-- | Whether termination protection is enabled for the stack. For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
staEnableTerminationProtection :: Lens' Stack (Maybe Bool)
staEnableTerminationProtection = lens _staEnableTerminationProtection (\s a -> s {_staEnableTerminationProtection = a})

-- | Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
staDriftInformation :: Lens' Stack (Maybe StackDriftInformation)
staDriftInformation = lens _staDriftInformation (\s a -> s {_staDriftInformation = a})

-- | The unique ID of the change set.
staChangeSetId :: Lens' Stack (Maybe Text)
staChangeSetId = lens _staChangeSetId (\s a -> s {_staChangeSetId = a})

-- | The time the stack was deleted.
staDeletionTime :: Lens' Stack (Maybe UTCTime)
staDeletionTime = lens _staDeletionTime (\s a -> s {_staDeletionTime = a}) . mapping _Time

-- | A list of output structures.
staOutputs :: Lens' Stack [Output]
staOutputs = lens _staOutputs (\s a -> s {_staOutputs = a}) . _Default . _Coerce

-- | A list of @Parameter@ structures.
staParameters :: Lens' Stack [Parameter]
staParameters = lens _staParameters (\s a -> s {_staParameters = a}) . _Default . _Coerce

-- | Unique identifier of the stack.
staStackId :: Lens' Stack (Maybe Text)
staStackId = lens _staStackId (\s a -> s {_staStackId = a})

-- | A user-defined description associated with the stack.
staDescription :: Lens' Stack (Maybe Text)
staDescription = lens _staDescription (\s a -> s {_staDescription = a})

-- | The capabilities allowed in the stack.
staCapabilities :: Lens' Stack [Capability]
staCapabilities = lens _staCapabilities (\s a -> s {_staCapabilities = a}) . _Default . _Coerce

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
staRollbackConfiguration :: Lens' Stack (Maybe RollbackConfiguration)
staRollbackConfiguration = lens _staRollbackConfiguration (\s a -> s {_staRollbackConfiguration = a})

-- | A list of @Tag@ s that specify information about the stack.
staTags :: Lens' Stack [Tag]
staTags = lens _staTags (\s a -> s {_staTags = a}) . _Default . _Coerce

-- | The amount of time within which stack creation should complete.
staTimeoutInMinutes :: Lens' Stack (Maybe Natural)
staTimeoutInMinutes = lens _staTimeoutInMinutes (\s a -> s {_staTimeoutInMinutes = a}) . mapping _Nat

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
staParentId :: Lens' Stack (Maybe Text)
staParentId = lens _staParentId (\s a -> s {_staParentId = a})

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
staRoleARN :: Lens' Stack (Maybe Text)
staRoleARN = lens _staRoleARN (\s a -> s {_staRoleARN = a})

-- | The name associated with the stack.
staStackName :: Lens' Stack Text
staStackName = lens _staStackName (\s a -> s {_staStackName = a})

-- | The time at which the stack was created.
staCreationTime :: Lens' Stack UTCTime
staCreationTime = lens _staCreationTime (\s a -> s {_staCreationTime = a}) . _Time

-- | Current status of the stack.
staStackStatus :: Lens' Stack StackStatus
staStackStatus = lens _staStackStatus (\s a -> s {_staStackStatus = a})

instance FromXML Stack where
  parseXML x =
    Stack'
      <$> (x .@? "DisableRollback")
      <*> (x .@? "LastUpdatedTime")
      <*> (x .@? "RootId")
      <*> ( x .@? "NotificationARNs" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "StackStatusReason")
      <*> (x .@? "EnableTerminationProtection")
      <*> (x .@? "DriftInformation")
      <*> (x .@? "ChangeSetId")
      <*> (x .@? "DeletionTime")
      <*> (x .@? "Outputs" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Parameters" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "StackId")
      <*> (x .@? "Description")
      <*> (x .@? "Capabilities" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "RollbackConfiguration")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "TimeoutInMinutes")
      <*> (x .@? "ParentId")
      <*> (x .@? "RoleARN")
      <*> (x .@ "StackName")
      <*> (x .@ "CreationTime")
      <*> (x .@ "StackStatus")

instance Hashable Stack

instance NFData Stack
