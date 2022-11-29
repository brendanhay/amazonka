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
-- Module      : Amazonka.CloudFormation.Types.StackResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackResource where

import Amazonka.CloudFormation.Types.ModuleInfo
import Amazonka.CloudFormation.Types.ResourceStatus
import Amazonka.CloudFormation.Types.StackResourceDriftInformation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The StackResource data type.
--
-- /See:/ 'newStackResource' smart constructor.
data StackResource = StackResource'
  { -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | Success\/failure message associated with the resource.
    resourceStatusReason :: Prelude.Maybe Prelude.Text,
    -- | User defined description associated with the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name associated with the stack.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Prelude.Maybe ModuleInfo,
    -- | Information about whether the resource\'s actual configuration differs,
    -- or has /drifted/, from its expected configuration, as defined in the
    -- stack template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Prelude.Maybe StackResourceDriftInformation,
    -- | The name or unique identifier that corresponds to a physical instance ID
    -- of a resource supported by CloudFormation.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The logical name of the resource specified in the template.
    logicalResourceId :: Prelude.Text,
    -- | Type of resource. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>
    -- in the CloudFormation User Guide.
    resourceType :: Prelude.Text,
    -- | Time the status was updated.
    timestamp :: Core.ISO8601,
    -- | Current status of the resource.
    resourceStatus :: ResourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'stackResource_stackId' - Unique identifier of the stack.
--
-- 'resourceStatusReason', 'stackResource_resourceStatusReason' - Success\/failure message associated with the resource.
--
-- 'description', 'stackResource_description' - User defined description associated with the resource.
--
-- 'stackName', 'stackResource_stackName' - The name associated with the stack.
--
-- 'moduleInfo', 'stackResource_moduleInfo' - Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- 'driftInformation', 'stackResource_driftInformation' - Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'physicalResourceId', 'stackResource_physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by CloudFormation.
--
-- 'logicalResourceId', 'stackResource_logicalResourceId' - The logical name of the resource specified in the template.
--
-- 'resourceType', 'stackResource_resourceType' - Type of resource. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>
-- in the CloudFormation User Guide.
--
-- 'timestamp', 'stackResource_timestamp' - Time the status was updated.
--
-- 'resourceStatus', 'stackResource_resourceStatus' - Current status of the resource.
newStackResource ::
  -- | 'logicalResourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'resourceStatus'
  ResourceStatus ->
  StackResource
newStackResource
  pLogicalResourceId_
  pResourceType_
  pTimestamp_
  pResourceStatus_ =
    StackResource'
      { stackId = Prelude.Nothing,
        resourceStatusReason = Prelude.Nothing,
        description = Prelude.Nothing,
        stackName = Prelude.Nothing,
        moduleInfo = Prelude.Nothing,
        driftInformation = Prelude.Nothing,
        physicalResourceId = Prelude.Nothing,
        logicalResourceId = pLogicalResourceId_,
        resourceType = pResourceType_,
        timestamp = Core._Time Lens.# pTimestamp_,
        resourceStatus = pResourceStatus_
      }

-- | Unique identifier of the stack.
stackResource_stackId :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_stackId = Lens.lens (\StackResource' {stackId} -> stackId) (\s@StackResource' {} a -> s {stackId = a} :: StackResource)

-- | Success\/failure message associated with the resource.
stackResource_resourceStatusReason :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_resourceStatusReason = Lens.lens (\StackResource' {resourceStatusReason} -> resourceStatusReason) (\s@StackResource' {} a -> s {resourceStatusReason = a} :: StackResource)

-- | User defined description associated with the resource.
stackResource_description :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_description = Lens.lens (\StackResource' {description} -> description) (\s@StackResource' {} a -> s {description = a} :: StackResource)

-- | The name associated with the stack.
stackResource_stackName :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_stackName = Lens.lens (\StackResource' {stackName} -> stackName) (\s@StackResource' {} a -> s {stackName = a} :: StackResource)

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
stackResource_moduleInfo :: Lens.Lens' StackResource (Prelude.Maybe ModuleInfo)
stackResource_moduleInfo = Lens.lens (\StackResource' {moduleInfo} -> moduleInfo) (\s@StackResource' {} a -> s {moduleInfo = a} :: StackResource)

-- | Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stackResource_driftInformation :: Lens.Lens' StackResource (Prelude.Maybe StackResourceDriftInformation)
stackResource_driftInformation = Lens.lens (\StackResource' {driftInformation} -> driftInformation) (\s@StackResource' {} a -> s {driftInformation = a} :: StackResource)

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by CloudFormation.
stackResource_physicalResourceId :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_physicalResourceId = Lens.lens (\StackResource' {physicalResourceId} -> physicalResourceId) (\s@StackResource' {} a -> s {physicalResourceId = a} :: StackResource)

-- | The logical name of the resource specified in the template.
stackResource_logicalResourceId :: Lens.Lens' StackResource Prelude.Text
stackResource_logicalResourceId = Lens.lens (\StackResource' {logicalResourceId} -> logicalResourceId) (\s@StackResource' {} a -> s {logicalResourceId = a} :: StackResource)

-- | Type of resource. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>
-- in the CloudFormation User Guide.
stackResource_resourceType :: Lens.Lens' StackResource Prelude.Text
stackResource_resourceType = Lens.lens (\StackResource' {resourceType} -> resourceType) (\s@StackResource' {} a -> s {resourceType = a} :: StackResource)

-- | Time the status was updated.
stackResource_timestamp :: Lens.Lens' StackResource Prelude.UTCTime
stackResource_timestamp = Lens.lens (\StackResource' {timestamp} -> timestamp) (\s@StackResource' {} a -> s {timestamp = a} :: StackResource) Prelude.. Core._Time

-- | Current status of the resource.
stackResource_resourceStatus :: Lens.Lens' StackResource ResourceStatus
stackResource_resourceStatus = Lens.lens (\StackResource' {resourceStatus} -> resourceStatus) (\s@StackResource' {} a -> s {resourceStatus = a} :: StackResource)

instance Core.FromXML StackResource where
  parseXML x =
    StackResource'
      Prelude.<$> (x Core..@? "StackId")
      Prelude.<*> (x Core..@? "ResourceStatusReason")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "StackName")
      Prelude.<*> (x Core..@? "ModuleInfo")
      Prelude.<*> (x Core..@? "DriftInformation")
      Prelude.<*> (x Core..@? "PhysicalResourceId")
      Prelude.<*> (x Core..@ "LogicalResourceId")
      Prelude.<*> (x Core..@ "ResourceType")
      Prelude.<*> (x Core..@ "Timestamp")
      Prelude.<*> (x Core..@ "ResourceStatus")

instance Prelude.Hashable StackResource where
  hashWithSalt _salt StackResource' {..} =
    _salt `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` resourceStatusReason
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` moduleInfo
      `Prelude.hashWithSalt` driftInformation
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` resourceStatus

instance Prelude.NFData StackResource where
  rnf StackResource' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf resourceStatusReason
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf moduleInfo
      `Prelude.seq` Prelude.rnf driftInformation
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf resourceStatus
