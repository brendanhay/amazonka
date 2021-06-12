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
-- Module      : Network.AWS.CloudFormation.Types.StackResourceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceSummary where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains high-level information about the specified stack resource.
--
-- /See:/ 'newStackResourceSummary' smart constructor.
data StackResourceSummary = StackResourceSummary'
  { -- | Information about whether the resource\'s actual configuration differs,
    -- or has /drifted/, from its expected configuration, as defined in the
    -- stack template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Core.Maybe StackResourceDriftInformationSummary,
    -- | The name or unique identifier that corresponds to a physical instance ID
    -- of the resource.
    physicalResourceId :: Core.Maybe Core.Text,
    -- | Success\/failure message associated with the resource.
    resourceStatusReason :: Core.Maybe Core.Text,
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Core.Maybe ModuleInfo,
    -- | The logical name of the resource specified in the template.
    logicalResourceId :: Core.Text,
    -- | Type of resource. (For more information, go to
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
-- Create a value of 'StackResourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'driftInformation', 'stackResourceSummary_driftInformation' - Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'physicalResourceId', 'stackResourceSummary_physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID
-- of the resource.
--
-- 'resourceStatusReason', 'stackResourceSummary_resourceStatusReason' - Success\/failure message associated with the resource.
--
-- 'moduleInfo', 'stackResourceSummary_moduleInfo' - Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- 'logicalResourceId', 'stackResourceSummary_logicalResourceId' - The logical name of the resource specified in the template.
--
-- 'resourceType', 'stackResourceSummary_resourceType' - Type of resource. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
--
-- 'lastUpdatedTimestamp', 'stackResourceSummary_lastUpdatedTimestamp' - Time the status was updated.
--
-- 'resourceStatus', 'stackResourceSummary_resourceStatus' - Current status of the resource.
newStackResourceSummary ::
  -- | 'logicalResourceId'
  Core.Text ->
  -- | 'resourceType'
  Core.Text ->
  -- | 'lastUpdatedTimestamp'
  Core.UTCTime ->
  -- | 'resourceStatus'
  ResourceStatus ->
  StackResourceSummary
newStackResourceSummary
  pLogicalResourceId_
  pResourceType_
  pLastUpdatedTimestamp_
  pResourceStatus_ =
    StackResourceSummary'
      { driftInformation =
          Core.Nothing,
        physicalResourceId = Core.Nothing,
        resourceStatusReason = Core.Nothing,
        moduleInfo = Core.Nothing,
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
stackResourceSummary_driftInformation :: Lens.Lens' StackResourceSummary (Core.Maybe StackResourceDriftInformationSummary)
stackResourceSummary_driftInformation = Lens.lens (\StackResourceSummary' {driftInformation} -> driftInformation) (\s@StackResourceSummary' {} a -> s {driftInformation = a} :: StackResourceSummary)

-- | The name or unique identifier that corresponds to a physical instance ID
-- of the resource.
stackResourceSummary_physicalResourceId :: Lens.Lens' StackResourceSummary (Core.Maybe Core.Text)
stackResourceSummary_physicalResourceId = Lens.lens (\StackResourceSummary' {physicalResourceId} -> physicalResourceId) (\s@StackResourceSummary' {} a -> s {physicalResourceId = a} :: StackResourceSummary)

-- | Success\/failure message associated with the resource.
stackResourceSummary_resourceStatusReason :: Lens.Lens' StackResourceSummary (Core.Maybe Core.Text)
stackResourceSummary_resourceStatusReason = Lens.lens (\StackResourceSummary' {resourceStatusReason} -> resourceStatusReason) (\s@StackResourceSummary' {} a -> s {resourceStatusReason = a} :: StackResourceSummary)

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
stackResourceSummary_moduleInfo :: Lens.Lens' StackResourceSummary (Core.Maybe ModuleInfo)
stackResourceSummary_moduleInfo = Lens.lens (\StackResourceSummary' {moduleInfo} -> moduleInfo) (\s@StackResourceSummary' {} a -> s {moduleInfo = a} :: StackResourceSummary)

-- | The logical name of the resource specified in the template.
stackResourceSummary_logicalResourceId :: Lens.Lens' StackResourceSummary Core.Text
stackResourceSummary_logicalResourceId = Lens.lens (\StackResourceSummary' {logicalResourceId} -> logicalResourceId) (\s@StackResourceSummary' {} a -> s {logicalResourceId = a} :: StackResourceSummary)

-- | Type of resource. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
stackResourceSummary_resourceType :: Lens.Lens' StackResourceSummary Core.Text
stackResourceSummary_resourceType = Lens.lens (\StackResourceSummary' {resourceType} -> resourceType) (\s@StackResourceSummary' {} a -> s {resourceType = a} :: StackResourceSummary)

-- | Time the status was updated.
stackResourceSummary_lastUpdatedTimestamp :: Lens.Lens' StackResourceSummary Core.UTCTime
stackResourceSummary_lastUpdatedTimestamp = Lens.lens (\StackResourceSummary' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@StackResourceSummary' {} a -> s {lastUpdatedTimestamp = a} :: StackResourceSummary) Core.. Core._Time

-- | Current status of the resource.
stackResourceSummary_resourceStatus :: Lens.Lens' StackResourceSummary ResourceStatus
stackResourceSummary_resourceStatus = Lens.lens (\StackResourceSummary' {resourceStatus} -> resourceStatus) (\s@StackResourceSummary' {} a -> s {resourceStatus = a} :: StackResourceSummary)

instance Core.FromXML StackResourceSummary where
  parseXML x =
    StackResourceSummary'
      Core.<$> (x Core..@? "DriftInformation")
      Core.<*> (x Core..@? "PhysicalResourceId")
      Core.<*> (x Core..@? "ResourceStatusReason")
      Core.<*> (x Core..@? "ModuleInfo")
      Core.<*> (x Core..@ "LogicalResourceId")
      Core.<*> (x Core..@ "ResourceType")
      Core.<*> (x Core..@ "LastUpdatedTimestamp")
      Core.<*> (x Core..@ "ResourceStatus")

instance Core.Hashable StackResourceSummary

instance Core.NFData StackResourceSummary
