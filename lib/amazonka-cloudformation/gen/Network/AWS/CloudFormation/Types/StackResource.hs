{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResource
  ( StackResource (..),

    -- * Smart constructor
    mkStackResource,

    -- * Lenses
    srLogicalResourceId,
    srResourceType,
    srTimestamp,
    srResourceStatus,
    srDescription,
    srDriftInformation,
    srModuleInfo,
    srPhysicalResourceId,
    srResourceStatusReason,
    srStackId,
    srStackName,
  )
where

import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.LogicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.ModuleInfo as Types
import qualified Network.AWS.CloudFormation.Types.PhysicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.ResourceStatus as Types
import qualified Network.AWS.CloudFormation.Types.ResourceStatusReason as Types
import qualified Network.AWS.CloudFormation.Types.ResourceType as Types
import qualified Network.AWS.CloudFormation.Types.StackId as Types
import qualified Network.AWS.CloudFormation.Types.StackName as Types
import qualified Network.AWS.CloudFormation.Types.StackResourceDriftInformation as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The StackResource data type.
--
-- /See:/ 'mkStackResource' smart constructor.
data StackResource = StackResource'
  { -- | The logical name of the resource specified in the template.
    logicalResourceId :: Types.LogicalResourceId,
    -- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
    resourceType :: Types.ResourceType,
    -- | Time the status was updated.
    timestamp :: Core.UTCTime,
    -- | Current status of the resource.
    resourceStatus :: Types.ResourceStatus,
    -- | User defined description associated with the resource.
    description :: Core.Maybe Types.Description,
    -- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
    driftInformation :: Core.Maybe Types.StackResourceDriftInformation,
    -- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
    moduleInfo :: Core.Maybe Types.ModuleInfo,
    -- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
    physicalResourceId :: Core.Maybe Types.PhysicalResourceId,
    -- | Success/failure message associated with the resource.
    resourceStatusReason :: Core.Maybe Types.ResourceStatusReason,
    -- | Unique identifier of the stack.
    stackId :: Core.Maybe Types.StackId,
    -- | The name associated with the stack.
    stackName :: Core.Maybe Types.StackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StackResource' value with any optional fields omitted.
mkStackResource ::
  -- | 'logicalResourceId'
  Types.LogicalResourceId ->
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'timestamp'
  Core.UTCTime ->
  -- | 'resourceStatus'
  Types.ResourceStatus ->
  StackResource
mkStackResource
  logicalResourceId
  resourceType
  timestamp
  resourceStatus =
    StackResource'
      { logicalResourceId,
        resourceType,
        timestamp,
        resourceStatus,
        description = Core.Nothing,
        driftInformation = Core.Nothing,
        moduleInfo = Core.Nothing,
        physicalResourceId = Core.Nothing,
        resourceStatusReason = Core.Nothing,
        stackId = Core.Nothing,
        stackName = Core.Nothing
      }

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srLogicalResourceId :: Lens.Lens' StackResource Types.LogicalResourceId
srLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED srLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srResourceType :: Lens.Lens' StackResource Types.ResourceType
srResourceType = Lens.field @"resourceType"
{-# DEPRECATED srResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Time the status was updated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTimestamp :: Lens.Lens' StackResource Core.UTCTime
srTimestamp = Lens.field @"timestamp"
{-# DEPRECATED srTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | Current status of the resource.
--
-- /Note:/ Consider using 'resourceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srResourceStatus :: Lens.Lens' StackResource Types.ResourceStatus
srResourceStatus = Lens.field @"resourceStatus"
{-# DEPRECATED srResourceStatus "Use generic-lens or generic-optics with 'resourceStatus' instead." #-}

-- | User defined description associated with the resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDescription :: Lens.Lens' StackResource (Core.Maybe Types.Description)
srDescription = Lens.field @"description"
{-# DEPRECATED srDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDriftInformation :: Lens.Lens' StackResource (Core.Maybe Types.StackResourceDriftInformation)
srDriftInformation = Lens.field @"driftInformation"
{-# DEPRECATED srDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srModuleInfo :: Lens.Lens' StackResource (Core.Maybe Types.ModuleInfo)
srModuleInfo = Lens.field @"moduleInfo"
{-# DEPRECATED srModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srPhysicalResourceId :: Lens.Lens' StackResource (Core.Maybe Types.PhysicalResourceId)
srPhysicalResourceId = Lens.field @"physicalResourceId"
{-# DEPRECATED srPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | Success/failure message associated with the resource.
--
-- /Note:/ Consider using 'resourceStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srResourceStatusReason :: Lens.Lens' StackResource (Core.Maybe Types.ResourceStatusReason)
srResourceStatusReason = Lens.field @"resourceStatusReason"
{-# DEPRECATED srResourceStatusReason "Use generic-lens or generic-optics with 'resourceStatusReason' instead." #-}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStackId :: Lens.Lens' StackResource (Core.Maybe Types.StackId)
srStackId = Lens.field @"stackId"
{-# DEPRECATED srStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStackName :: Lens.Lens' StackResource (Core.Maybe Types.StackName)
srStackName = Lens.field @"stackName"
{-# DEPRECATED srStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.FromXML StackResource where
  parseXML x =
    StackResource'
      Core.<$> (x Core..@ "LogicalResourceId")
      Core.<*> (x Core..@ "ResourceType")
      Core.<*> (x Core..@ "Timestamp")
      Core.<*> (x Core..@ "ResourceStatus")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "DriftInformation")
      Core.<*> (x Core..@? "ModuleInfo")
      Core.<*> (x Core..@? "PhysicalResourceId")
      Core.<*> (x Core..@? "ResourceStatusReason")
      Core.<*> (x Core..@? "StackId")
      Core.<*> (x Core..@? "StackName")
