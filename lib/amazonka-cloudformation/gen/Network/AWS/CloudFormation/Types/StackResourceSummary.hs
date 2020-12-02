{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceSummary where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains high-level information about the specified stack resource.
--
--
--
-- /See:/ 'stackResourceSummary' smart constructor.
data StackResourceSummary = StackResourceSummary'
  { _srsPhysicalResourceId ::
      !(Maybe Text),
    _srsResourceStatusReason :: !(Maybe Text),
    _srsDriftInformation ::
      !(Maybe StackResourceDriftInformationSummary),
    _srsModuleInfo :: !(Maybe ModuleInfo),
    _srsLogicalResourceId :: !Text,
    _srsResourceType :: !Text,
    _srsLastUpdatedTimestamp :: !ISO8601,
    _srsResourceStatus :: !ResourceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackResourceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsPhysicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of the resource.
--
-- * 'srsResourceStatusReason' - Success/failure message associated with the resource.
--
-- * 'srsDriftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'srsModuleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- * 'srsLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'srsResourceType' - Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- * 'srsLastUpdatedTimestamp' - Time the status was updated.
--
-- * 'srsResourceStatus' - Current status of the resource.
stackResourceSummary ::
  -- | 'srsLogicalResourceId'
  Text ->
  -- | 'srsResourceType'
  Text ->
  -- | 'srsLastUpdatedTimestamp'
  UTCTime ->
  -- | 'srsResourceStatus'
  ResourceStatus ->
  StackResourceSummary
stackResourceSummary
  pLogicalResourceId_
  pResourceType_
  pLastUpdatedTimestamp_
  pResourceStatus_ =
    StackResourceSummary'
      { _srsPhysicalResourceId = Nothing,
        _srsResourceStatusReason = Nothing,
        _srsDriftInformation = Nothing,
        _srsModuleInfo = Nothing,
        _srsLogicalResourceId = pLogicalResourceId_,
        _srsResourceType = pResourceType_,
        _srsLastUpdatedTimestamp = _Time # pLastUpdatedTimestamp_,
        _srsResourceStatus = pResourceStatus_
      }

-- | The name or unique identifier that corresponds to a physical instance ID of the resource.
srsPhysicalResourceId :: Lens' StackResourceSummary (Maybe Text)
srsPhysicalResourceId = lens _srsPhysicalResourceId (\s a -> s {_srsPhysicalResourceId = a})

-- | Success/failure message associated with the resource.
srsResourceStatusReason :: Lens' StackResourceSummary (Maybe Text)
srsResourceStatusReason = lens _srsResourceStatusReason (\s a -> s {_srsResourceStatusReason = a})

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
srsDriftInformation :: Lens' StackResourceSummary (Maybe StackResourceDriftInformationSummary)
srsDriftInformation = lens _srsDriftInformation (\s a -> s {_srsDriftInformation = a})

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
srsModuleInfo :: Lens' StackResourceSummary (Maybe ModuleInfo)
srsModuleInfo = lens _srsModuleInfo (\s a -> s {_srsModuleInfo = a})

-- | The logical name of the resource specified in the template.
srsLogicalResourceId :: Lens' StackResourceSummary Text
srsLogicalResourceId = lens _srsLogicalResourceId (\s a -> s {_srsLogicalResourceId = a})

-- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
srsResourceType :: Lens' StackResourceSummary Text
srsResourceType = lens _srsResourceType (\s a -> s {_srsResourceType = a})

-- | Time the status was updated.
srsLastUpdatedTimestamp :: Lens' StackResourceSummary UTCTime
srsLastUpdatedTimestamp = lens _srsLastUpdatedTimestamp (\s a -> s {_srsLastUpdatedTimestamp = a}) . _Time

-- | Current status of the resource.
srsResourceStatus :: Lens' StackResourceSummary ResourceStatus
srsResourceStatus = lens _srsResourceStatus (\s a -> s {_srsResourceStatus = a})

instance FromXML StackResourceSummary where
  parseXML x =
    StackResourceSummary'
      <$> (x .@? "PhysicalResourceId")
      <*> (x .@? "ResourceStatusReason")
      <*> (x .@? "DriftInformation")
      <*> (x .@? "ModuleInfo")
      <*> (x .@ "LogicalResourceId")
      <*> (x .@ "ResourceType")
      <*> (x .@ "LastUpdatedTimestamp")
      <*> (x .@ "ResourceStatus")

instance Hashable StackResourceSummary

instance NFData StackResourceSummary
