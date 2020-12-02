{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResource where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The StackResource data type.
--
--
--
-- /See:/ 'stackResource' smart constructor.
data StackResource = StackResource'
  { _srPhysicalResourceId ::
      !(Maybe Text),
    _srResourceStatusReason :: !(Maybe Text),
    _srDriftInformation :: !(Maybe StackResourceDriftInformation),
    _srModuleInfo :: !(Maybe ModuleInfo),
    _srStackId :: !(Maybe Text),
    _srDescription :: !(Maybe Text),
    _srStackName :: !(Maybe Text),
    _srLogicalResourceId :: !Text,
    _srResourceType :: !Text,
    _srTimestamp :: !ISO8601,
    _srResourceStatus :: !ResourceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srPhysicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- * 'srResourceStatusReason' - Success/failure message associated with the resource.
--
-- * 'srDriftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'srModuleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- * 'srStackId' - Unique identifier of the stack.
--
-- * 'srDescription' - User defined description associated with the resource.
--
-- * 'srStackName' - The name associated with the stack.
--
-- * 'srLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'srResourceType' - Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- * 'srTimestamp' - Time the status was updated.
--
-- * 'srResourceStatus' - Current status of the resource.
stackResource ::
  -- | 'srLogicalResourceId'
  Text ->
  -- | 'srResourceType'
  Text ->
  -- | 'srTimestamp'
  UTCTime ->
  -- | 'srResourceStatus'
  ResourceStatus ->
  StackResource
stackResource
  pLogicalResourceId_
  pResourceType_
  pTimestamp_
  pResourceStatus_ =
    StackResource'
      { _srPhysicalResourceId = Nothing,
        _srResourceStatusReason = Nothing,
        _srDriftInformation = Nothing,
        _srModuleInfo = Nothing,
        _srStackId = Nothing,
        _srDescription = Nothing,
        _srStackName = Nothing,
        _srLogicalResourceId = pLogicalResourceId_,
        _srResourceType = pResourceType_,
        _srTimestamp = _Time # pTimestamp_,
        _srResourceStatus = pResourceStatus_
      }

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
srPhysicalResourceId :: Lens' StackResource (Maybe Text)
srPhysicalResourceId = lens _srPhysicalResourceId (\s a -> s {_srPhysicalResourceId = a})

-- | Success/failure message associated with the resource.
srResourceStatusReason :: Lens' StackResource (Maybe Text)
srResourceStatusReason = lens _srResourceStatusReason (\s a -> s {_srResourceStatusReason = a})

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
srDriftInformation :: Lens' StackResource (Maybe StackResourceDriftInformation)
srDriftInformation = lens _srDriftInformation (\s a -> s {_srDriftInformation = a})

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
srModuleInfo :: Lens' StackResource (Maybe ModuleInfo)
srModuleInfo = lens _srModuleInfo (\s a -> s {_srModuleInfo = a})

-- | Unique identifier of the stack.
srStackId :: Lens' StackResource (Maybe Text)
srStackId = lens _srStackId (\s a -> s {_srStackId = a})

-- | User defined description associated with the resource.
srDescription :: Lens' StackResource (Maybe Text)
srDescription = lens _srDescription (\s a -> s {_srDescription = a})

-- | The name associated with the stack.
srStackName :: Lens' StackResource (Maybe Text)
srStackName = lens _srStackName (\s a -> s {_srStackName = a})

-- | The logical name of the resource specified in the template.
srLogicalResourceId :: Lens' StackResource Text
srLogicalResourceId = lens _srLogicalResourceId (\s a -> s {_srLogicalResourceId = a})

-- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
srResourceType :: Lens' StackResource Text
srResourceType = lens _srResourceType (\s a -> s {_srResourceType = a})

-- | Time the status was updated.
srTimestamp :: Lens' StackResource UTCTime
srTimestamp = lens _srTimestamp (\s a -> s {_srTimestamp = a}) . _Time

-- | Current status of the resource.
srResourceStatus :: Lens' StackResource ResourceStatus
srResourceStatus = lens _srResourceStatus (\s a -> s {_srResourceStatus = a})

instance FromXML StackResource where
  parseXML x =
    StackResource'
      <$> (x .@? "PhysicalResourceId")
      <*> (x .@? "ResourceStatusReason")
      <*> (x .@? "DriftInformation")
      <*> (x .@? "ModuleInfo")
      <*> (x .@? "StackId")
      <*> (x .@? "Description")
      <*> (x .@? "StackName")
      <*> (x .@ "LogicalResourceId")
      <*> (x .@ "ResourceType")
      <*> (x .@ "Timestamp")
      <*> (x .@ "ResourceStatus")

instance Hashable StackResource

instance NFData StackResource
