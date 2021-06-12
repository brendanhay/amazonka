{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDetail where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains detailed information about the specified stack resource.
--
-- /See:/ 'newStackResourceDetail' smart constructor.
data StackResourceDetail = StackResourceDetail'
  { -- | Information about whether the resource\'s actual configuration differs,
    -- or has /drifted/, from its expected configuration, as defined in the
    -- stack template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Core.Maybe StackResourceDriftInformation,
    -- | The name associated with the stack.
    stackName :: Core.Maybe Core.Text,
    -- | Unique identifier of the stack.
    stackId :: Core.Maybe Core.Text,
    -- | The content of the @Metadata@ attribute declared for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute>
    -- in the AWS CloudFormation User Guide.
    metadata :: Core.Maybe Core.Text,
    -- | The name or unique identifier that corresponds to a physical instance ID
    -- of a resource supported by AWS CloudFormation.
    physicalResourceId :: Core.Maybe Core.Text,
    -- | Success\/failure message associated with the resource.
    resourceStatusReason :: Core.Maybe Core.Text,
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Core.Maybe ModuleInfo,
    -- | User defined description associated with the resource.
    description :: Core.Maybe Core.Text,
    -- | The logical name of the resource specified in the template.
    logicalResourceId :: Core.Text,
    -- | Type of resource. ((For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
    -- in the AWS CloudFormation User Guide.)
    resourceType :: Core.Text,
    -- | Time the status was updated.
    lastUpdatedTimestamp :: Core.ISO8601,
    -- | Current status of the resource.
    resourceStatus :: ResourceStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackResourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'driftInformation', 'stackResourceDetail_driftInformation' - Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'stackName', 'stackResourceDetail_stackName' - The name associated with the stack.
--
-- 'stackId', 'stackResourceDetail_stackId' - Unique identifier of the stack.
--
-- 'metadata', 'stackResourceDetail_metadata' - The content of the @Metadata@ attribute declared for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute>
-- in the AWS CloudFormation User Guide.
--
-- 'physicalResourceId', 'stackResourceDetail_physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
--
-- 'resourceStatusReason', 'stackResourceDetail_resourceStatusReason' - Success\/failure message associated with the resource.
--
-- 'moduleInfo', 'stackResourceDetail_moduleInfo' - Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- 'description', 'stackResourceDetail_description' - User defined description associated with the resource.
--
-- 'logicalResourceId', 'stackResourceDetail_logicalResourceId' - The logical name of the resource specified in the template.
--
-- 'resourceType', 'stackResourceDetail_resourceType' - Type of resource. ((For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
--
-- 'lastUpdatedTimestamp', 'stackResourceDetail_lastUpdatedTimestamp' - Time the status was updated.
--
-- 'resourceStatus', 'stackResourceDetail_resourceStatus' - Current status of the resource.
newStackResourceDetail ::
  -- | 'logicalResourceId'
  Core.Text ->
  -- | 'resourceType'
  Core.Text ->
  -- | 'lastUpdatedTimestamp'
  Core.UTCTime ->
  -- | 'resourceStatus'
  ResourceStatus ->
  StackResourceDetail
newStackResourceDetail
  pLogicalResourceId_
  pResourceType_
  pLastUpdatedTimestamp_
  pResourceStatus_ =
    StackResourceDetail'
      { driftInformation =
          Core.Nothing,
        stackName = Core.Nothing,
        stackId = Core.Nothing,
        metadata = Core.Nothing,
        physicalResourceId = Core.Nothing,
        resourceStatusReason = Core.Nothing,
        moduleInfo = Core.Nothing,
        description = Core.Nothing,
        logicalResourceId = pLogicalResourceId_,
        resourceType = pResourceType_,
        lastUpdatedTimestamp =
          Core._Time Lens.# pLastUpdatedTimestamp_,
        resourceStatus = pResourceStatus_
      }

-- | Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stackResourceDetail_driftInformation :: Lens.Lens' StackResourceDetail (Core.Maybe StackResourceDriftInformation)
stackResourceDetail_driftInformation = Lens.lens (\StackResourceDetail' {driftInformation} -> driftInformation) (\s@StackResourceDetail' {} a -> s {driftInformation = a} :: StackResourceDetail)

-- | The name associated with the stack.
stackResourceDetail_stackName :: Lens.Lens' StackResourceDetail (Core.Maybe Core.Text)
stackResourceDetail_stackName = Lens.lens (\StackResourceDetail' {stackName} -> stackName) (\s@StackResourceDetail' {} a -> s {stackName = a} :: StackResourceDetail)

-- | Unique identifier of the stack.
stackResourceDetail_stackId :: Lens.Lens' StackResourceDetail (Core.Maybe Core.Text)
stackResourceDetail_stackId = Lens.lens (\StackResourceDetail' {stackId} -> stackId) (\s@StackResourceDetail' {} a -> s {stackId = a} :: StackResourceDetail)

-- | The content of the @Metadata@ attribute declared for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute>
-- in the AWS CloudFormation User Guide.
stackResourceDetail_metadata :: Lens.Lens' StackResourceDetail (Core.Maybe Core.Text)
stackResourceDetail_metadata = Lens.lens (\StackResourceDetail' {metadata} -> metadata) (\s@StackResourceDetail' {} a -> s {metadata = a} :: StackResourceDetail)

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
stackResourceDetail_physicalResourceId :: Lens.Lens' StackResourceDetail (Core.Maybe Core.Text)
stackResourceDetail_physicalResourceId = Lens.lens (\StackResourceDetail' {physicalResourceId} -> physicalResourceId) (\s@StackResourceDetail' {} a -> s {physicalResourceId = a} :: StackResourceDetail)

-- | Success\/failure message associated with the resource.
stackResourceDetail_resourceStatusReason :: Lens.Lens' StackResourceDetail (Core.Maybe Core.Text)
stackResourceDetail_resourceStatusReason = Lens.lens (\StackResourceDetail' {resourceStatusReason} -> resourceStatusReason) (\s@StackResourceDetail' {} a -> s {resourceStatusReason = a} :: StackResourceDetail)

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
stackResourceDetail_moduleInfo :: Lens.Lens' StackResourceDetail (Core.Maybe ModuleInfo)
stackResourceDetail_moduleInfo = Lens.lens (\StackResourceDetail' {moduleInfo} -> moduleInfo) (\s@StackResourceDetail' {} a -> s {moduleInfo = a} :: StackResourceDetail)

-- | User defined description associated with the resource.
stackResourceDetail_description :: Lens.Lens' StackResourceDetail (Core.Maybe Core.Text)
stackResourceDetail_description = Lens.lens (\StackResourceDetail' {description} -> description) (\s@StackResourceDetail' {} a -> s {description = a} :: StackResourceDetail)

-- | The logical name of the resource specified in the template.
stackResourceDetail_logicalResourceId :: Lens.Lens' StackResourceDetail Core.Text
stackResourceDetail_logicalResourceId = Lens.lens (\StackResourceDetail' {logicalResourceId} -> logicalResourceId) (\s@StackResourceDetail' {} a -> s {logicalResourceId = a} :: StackResourceDetail)

-- | Type of resource. ((For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
stackResourceDetail_resourceType :: Lens.Lens' StackResourceDetail Core.Text
stackResourceDetail_resourceType = Lens.lens (\StackResourceDetail' {resourceType} -> resourceType) (\s@StackResourceDetail' {} a -> s {resourceType = a} :: StackResourceDetail)

-- | Time the status was updated.
stackResourceDetail_lastUpdatedTimestamp :: Lens.Lens' StackResourceDetail Core.UTCTime
stackResourceDetail_lastUpdatedTimestamp = Lens.lens (\StackResourceDetail' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@StackResourceDetail' {} a -> s {lastUpdatedTimestamp = a} :: StackResourceDetail) Core.. Core._Time

-- | Current status of the resource.
stackResourceDetail_resourceStatus :: Lens.Lens' StackResourceDetail ResourceStatus
stackResourceDetail_resourceStatus = Lens.lens (\StackResourceDetail' {resourceStatus} -> resourceStatus) (\s@StackResourceDetail' {} a -> s {resourceStatus = a} :: StackResourceDetail)

instance Core.FromXML StackResourceDetail where
  parseXML x =
    StackResourceDetail'
      Core.<$> (x Core..@? "DriftInformation")
      Core.<*> (x Core..@? "StackName")
      Core.<*> (x Core..@? "StackId")
      Core.<*> (x Core..@? "Metadata")
      Core.<*> (x Core..@? "PhysicalResourceId")
      Core.<*> (x Core..@? "ResourceStatusReason")
      Core.<*> (x Core..@? "ModuleInfo")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@ "LogicalResourceId")
      Core.<*> (x Core..@ "ResourceType")
      Core.<*> (x Core..@ "LastUpdatedTimestamp")
      Core.<*> (x Core..@ "ResourceStatus")

instance Core.Hashable StackResourceDetail

instance Core.NFData StackResourceDetail
