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
-- Module      : Network.AWS.CloudFormation.Types.StackResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResource where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The StackResource data type.
--
-- /See:/ 'newStackResource' smart constructor.
data StackResource = StackResource'
  { -- | Information about whether the resource\'s actual configuration differs,
    -- or has /drifted/, from its expected configuration, as defined in the
    -- stack template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Prelude.Maybe StackResourceDriftInformation,
    -- | The name associated with the stack.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The name or unique identifier that corresponds to a physical instance ID
    -- of a resource supported by AWS CloudFormation.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | Success\/failure message associated with the resource.
    resourceStatusReason :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Prelude.Maybe ModuleInfo,
    -- | User defined description associated with the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The logical name of the resource specified in the template.
    logicalResourceId :: Prelude.Text,
    -- | Type of resource. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
    -- in the AWS CloudFormation User Guide.)
    resourceType :: Prelude.Text,
    -- | Time the status was updated.
    timestamp :: Prelude.ISO8601,
    -- | Current status of the resource.
    resourceStatus :: ResourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StackResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'driftInformation', 'stackResource_driftInformation' - Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'stackName', 'stackResource_stackName' - The name associated with the stack.
--
-- 'stackId', 'stackResource_stackId' - Unique identifier of the stack.
--
-- 'physicalResourceId', 'stackResource_physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
--
-- 'resourceStatusReason', 'stackResource_resourceStatusReason' - Success\/failure message associated with the resource.
--
-- 'moduleInfo', 'stackResource_moduleInfo' - Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- 'description', 'stackResource_description' - User defined description associated with the resource.
--
-- 'logicalResourceId', 'stackResource_logicalResourceId' - The logical name of the resource specified in the template.
--
-- 'resourceType', 'stackResource_resourceType' - Type of resource. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
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
      { driftInformation = Prelude.Nothing,
        stackName = Prelude.Nothing,
        stackId = Prelude.Nothing,
        physicalResourceId = Prelude.Nothing,
        resourceStatusReason = Prelude.Nothing,
        moduleInfo = Prelude.Nothing,
        description = Prelude.Nothing,
        logicalResourceId = pLogicalResourceId_,
        resourceType = pResourceType_,
        timestamp = Prelude._Time Lens.# pTimestamp_,
        resourceStatus = pResourceStatus_
      }

-- | Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stackResource_driftInformation :: Lens.Lens' StackResource (Prelude.Maybe StackResourceDriftInformation)
stackResource_driftInformation = Lens.lens (\StackResource' {driftInformation} -> driftInformation) (\s@StackResource' {} a -> s {driftInformation = a} :: StackResource)

-- | The name associated with the stack.
stackResource_stackName :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_stackName = Lens.lens (\StackResource' {stackName} -> stackName) (\s@StackResource' {} a -> s {stackName = a} :: StackResource)

-- | Unique identifier of the stack.
stackResource_stackId :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_stackId = Lens.lens (\StackResource' {stackId} -> stackId) (\s@StackResource' {} a -> s {stackId = a} :: StackResource)

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
stackResource_physicalResourceId :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_physicalResourceId = Lens.lens (\StackResource' {physicalResourceId} -> physicalResourceId) (\s@StackResource' {} a -> s {physicalResourceId = a} :: StackResource)

-- | Success\/failure message associated with the resource.
stackResource_resourceStatusReason :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_resourceStatusReason = Lens.lens (\StackResource' {resourceStatusReason} -> resourceStatusReason) (\s@StackResource' {} a -> s {resourceStatusReason = a} :: StackResource)

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
stackResource_moduleInfo :: Lens.Lens' StackResource (Prelude.Maybe ModuleInfo)
stackResource_moduleInfo = Lens.lens (\StackResource' {moduleInfo} -> moduleInfo) (\s@StackResource' {} a -> s {moduleInfo = a} :: StackResource)

-- | User defined description associated with the resource.
stackResource_description :: Lens.Lens' StackResource (Prelude.Maybe Prelude.Text)
stackResource_description = Lens.lens (\StackResource' {description} -> description) (\s@StackResource' {} a -> s {description = a} :: StackResource)

-- | The logical name of the resource specified in the template.
stackResource_logicalResourceId :: Lens.Lens' StackResource Prelude.Text
stackResource_logicalResourceId = Lens.lens (\StackResource' {logicalResourceId} -> logicalResourceId) (\s@StackResource' {} a -> s {logicalResourceId = a} :: StackResource)

-- | Type of resource. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
stackResource_resourceType :: Lens.Lens' StackResource Prelude.Text
stackResource_resourceType = Lens.lens (\StackResource' {resourceType} -> resourceType) (\s@StackResource' {} a -> s {resourceType = a} :: StackResource)

-- | Time the status was updated.
stackResource_timestamp :: Lens.Lens' StackResource Prelude.UTCTime
stackResource_timestamp = Lens.lens (\StackResource' {timestamp} -> timestamp) (\s@StackResource' {} a -> s {timestamp = a} :: StackResource) Prelude.. Prelude._Time

-- | Current status of the resource.
stackResource_resourceStatus :: Lens.Lens' StackResource ResourceStatus
stackResource_resourceStatus = Lens.lens (\StackResource' {resourceStatus} -> resourceStatus) (\s@StackResource' {} a -> s {resourceStatus = a} :: StackResource)

instance Prelude.FromXML StackResource where
  parseXML x =
    StackResource'
      Prelude.<$> (x Prelude..@? "DriftInformation")
      Prelude.<*> (x Prelude..@? "StackName")
      Prelude.<*> (x Prelude..@? "StackId")
      Prelude.<*> (x Prelude..@? "PhysicalResourceId")
      Prelude.<*> (x Prelude..@? "ResourceStatusReason")
      Prelude.<*> (x Prelude..@? "ModuleInfo")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@ "LogicalResourceId")
      Prelude.<*> (x Prelude..@ "ResourceType")
      Prelude.<*> (x Prelude..@ "Timestamp")
      Prelude.<*> (x Prelude..@ "ResourceStatus")

instance Prelude.Hashable StackResource

instance Prelude.NFData StackResource
