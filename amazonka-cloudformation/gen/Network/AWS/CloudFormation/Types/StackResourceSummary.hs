{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains high-level information about the specified stack resource.
--
-- /See:/ 'newStackResourceSummary' smart constructor.
data StackResourceSummary = StackResourceSummary'
  { -- | Information about whether the resource\'s actual configuration differs,
    -- or has /drifted/, from its expected configuration, as defined in the
    -- stack template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Prelude.Maybe StackResourceDriftInformationSummary,
    -- | The name or unique identifier that corresponds to a physical instance ID
    -- of the resource.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | Success\/failure message associated with the resource.
    resourceStatusReason :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Prelude.Maybe ModuleInfo,
    -- | The logical name of the resource specified in the template.
    logicalResourceId :: Prelude.Text,
    -- | Type of resource. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
    -- in the AWS CloudFormation User Guide.)
    resourceType :: Prelude.Text,
    -- | Time the status was updated.
    lastUpdatedTimestamp :: Prelude.ISO8601,
    -- | Current status of the resource.
    resourceStatus :: ResourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'lastUpdatedTimestamp'
  Prelude.UTCTime ->
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
          Prelude.Nothing,
        physicalResourceId = Prelude.Nothing,
        resourceStatusReason = Prelude.Nothing,
        moduleInfo = Prelude.Nothing,
        logicalResourceId = pLogicalResourceId_,
        resourceType = pResourceType_,
        lastUpdatedTimestamp =
          Prelude._Time Lens.# pLastUpdatedTimestamp_,
        resourceStatus = pResourceStatus_
      }

-- | Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stackResourceSummary_driftInformation :: Lens.Lens' StackResourceSummary (Prelude.Maybe StackResourceDriftInformationSummary)
stackResourceSummary_driftInformation = Lens.lens (\StackResourceSummary' {driftInformation} -> driftInformation) (\s@StackResourceSummary' {} a -> s {driftInformation = a} :: StackResourceSummary)

-- | The name or unique identifier that corresponds to a physical instance ID
-- of the resource.
stackResourceSummary_physicalResourceId :: Lens.Lens' StackResourceSummary (Prelude.Maybe Prelude.Text)
stackResourceSummary_physicalResourceId = Lens.lens (\StackResourceSummary' {physicalResourceId} -> physicalResourceId) (\s@StackResourceSummary' {} a -> s {physicalResourceId = a} :: StackResourceSummary)

-- | Success\/failure message associated with the resource.
stackResourceSummary_resourceStatusReason :: Lens.Lens' StackResourceSummary (Prelude.Maybe Prelude.Text)
stackResourceSummary_resourceStatusReason = Lens.lens (\StackResourceSummary' {resourceStatusReason} -> resourceStatusReason) (\s@StackResourceSummary' {} a -> s {resourceStatusReason = a} :: StackResourceSummary)

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
stackResourceSummary_moduleInfo :: Lens.Lens' StackResourceSummary (Prelude.Maybe ModuleInfo)
stackResourceSummary_moduleInfo = Lens.lens (\StackResourceSummary' {moduleInfo} -> moduleInfo) (\s@StackResourceSummary' {} a -> s {moduleInfo = a} :: StackResourceSummary)

-- | The logical name of the resource specified in the template.
stackResourceSummary_logicalResourceId :: Lens.Lens' StackResourceSummary Prelude.Text
stackResourceSummary_logicalResourceId = Lens.lens (\StackResourceSummary' {logicalResourceId} -> logicalResourceId) (\s@StackResourceSummary' {} a -> s {logicalResourceId = a} :: StackResourceSummary)

-- | Type of resource. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
stackResourceSummary_resourceType :: Lens.Lens' StackResourceSummary Prelude.Text
stackResourceSummary_resourceType = Lens.lens (\StackResourceSummary' {resourceType} -> resourceType) (\s@StackResourceSummary' {} a -> s {resourceType = a} :: StackResourceSummary)

-- | Time the status was updated.
stackResourceSummary_lastUpdatedTimestamp :: Lens.Lens' StackResourceSummary Prelude.UTCTime
stackResourceSummary_lastUpdatedTimestamp = Lens.lens (\StackResourceSummary' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@StackResourceSummary' {} a -> s {lastUpdatedTimestamp = a} :: StackResourceSummary) Prelude.. Prelude._Time

-- | Current status of the resource.
stackResourceSummary_resourceStatus :: Lens.Lens' StackResourceSummary ResourceStatus
stackResourceSummary_resourceStatus = Lens.lens (\StackResourceSummary' {resourceStatus} -> resourceStatus) (\s@StackResourceSummary' {} a -> s {resourceStatus = a} :: StackResourceSummary)

instance Prelude.FromXML StackResourceSummary where
  parseXML x =
    StackResourceSummary'
      Prelude.<$> (x Prelude..@? "DriftInformation")
      Prelude.<*> (x Prelude..@? "PhysicalResourceId")
      Prelude.<*> (x Prelude..@? "ResourceStatusReason")
      Prelude.<*> (x Prelude..@? "ModuleInfo")
      Prelude.<*> (x Prelude..@ "LogicalResourceId")
      Prelude.<*> (x Prelude..@ "ResourceType")
      Prelude.<*> (x Prelude..@ "LastUpdatedTimestamp")
      Prelude.<*> (x Prelude..@ "ResourceStatus")

instance Prelude.Hashable StackResourceSummary

instance Prelude.NFData StackResourceSummary
