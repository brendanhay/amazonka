{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDetail
  ( StackResourceDetail (..),

    -- * Smart constructor
    mkStackResourceDetail,

    -- * Lenses
    srdLogicalResourceId,
    srdResourceType,
    srdLastUpdatedTimestamp,
    srdResourceStatus,
    srdDescription,
    srdDriftInformation,
    srdMetadata,
    srdModuleInfo,
    srdPhysicalResourceId,
    srdResourceStatusReason,
    srdStackId,
    srdStackName,
  )
where

import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.LogicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.Metadata as Types
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

-- | Contains detailed information about the specified stack resource.
--
-- /See:/ 'mkStackResourceDetail' smart constructor.
data StackResourceDetail = StackResourceDetail'
  { -- | The logical name of the resource specified in the template.
    logicalResourceId :: Types.LogicalResourceId,
    -- | Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
    resourceType :: Types.ResourceType,
    -- | Time the status was updated.
    lastUpdatedTimestamp :: Core.UTCTime,
    -- | Current status of the resource.
    resourceStatus :: Types.ResourceStatus,
    -- | User defined description associated with the resource.
    description :: Core.Maybe Types.Description,
    -- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
    driftInformation :: Core.Maybe Types.StackResourceDriftInformation,
    -- | The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
    metadata :: Core.Maybe Types.Metadata,
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

-- | Creates a 'StackResourceDetail' value with any optional fields omitted.
mkStackResourceDetail ::
  -- | 'logicalResourceId'
  Types.LogicalResourceId ->
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'lastUpdatedTimestamp'
  Core.UTCTime ->
  -- | 'resourceStatus'
  Types.ResourceStatus ->
  StackResourceDetail
mkStackResourceDetail
  logicalResourceId
  resourceType
  lastUpdatedTimestamp
  resourceStatus =
    StackResourceDetail'
      { logicalResourceId,
        resourceType,
        lastUpdatedTimestamp,
        resourceStatus,
        description = Core.Nothing,
        driftInformation = Core.Nothing,
        metadata = Core.Nothing,
        moduleInfo = Core.Nothing,
        physicalResourceId = Core.Nothing,
        resourceStatusReason = Core.Nothing,
        stackId = Core.Nothing,
        stackName = Core.Nothing
      }

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdLogicalResourceId :: Lens.Lens' StackResourceDetail Types.LogicalResourceId
srdLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED srdLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdResourceType :: Lens.Lens' StackResourceDetail Types.ResourceType
srdResourceType = Lens.field @"resourceType"
{-# DEPRECATED srdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Time the status was updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdLastUpdatedTimestamp :: Lens.Lens' StackResourceDetail Core.UTCTime
srdLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED srdLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Current status of the resource.
--
-- /Note:/ Consider using 'resourceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdResourceStatus :: Lens.Lens' StackResourceDetail Types.ResourceStatus
srdResourceStatus = Lens.field @"resourceStatus"
{-# DEPRECATED srdResourceStatus "Use generic-lens or generic-optics with 'resourceStatus' instead." #-}

-- | User defined description associated with the resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdDescription :: Lens.Lens' StackResourceDetail (Core.Maybe Types.Description)
srdDescription = Lens.field @"description"
{-# DEPRECATED srdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdDriftInformation :: Lens.Lens' StackResourceDetail (Core.Maybe Types.StackResourceDriftInformation)
srdDriftInformation = Lens.field @"driftInformation"
{-# DEPRECATED srdDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdMetadata :: Lens.Lens' StackResourceDetail (Core.Maybe Types.Metadata)
srdMetadata = Lens.field @"metadata"
{-# DEPRECATED srdMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdModuleInfo :: Lens.Lens' StackResourceDetail (Core.Maybe Types.ModuleInfo)
srdModuleInfo = Lens.field @"moduleInfo"
{-# DEPRECATED srdModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdPhysicalResourceId :: Lens.Lens' StackResourceDetail (Core.Maybe Types.PhysicalResourceId)
srdPhysicalResourceId = Lens.field @"physicalResourceId"
{-# DEPRECATED srdPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | Success/failure message associated with the resource.
--
-- /Note:/ Consider using 'resourceStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdResourceStatusReason :: Lens.Lens' StackResourceDetail (Core.Maybe Types.ResourceStatusReason)
srdResourceStatusReason = Lens.field @"resourceStatusReason"
{-# DEPRECATED srdResourceStatusReason "Use generic-lens or generic-optics with 'resourceStatusReason' instead." #-}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdStackId :: Lens.Lens' StackResourceDetail (Core.Maybe Types.StackId)
srdStackId = Lens.field @"stackId"
{-# DEPRECATED srdStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdStackName :: Lens.Lens' StackResourceDetail (Core.Maybe Types.StackName)
srdStackName = Lens.field @"stackName"
{-# DEPRECATED srdStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.FromXML StackResourceDetail where
  parseXML x =
    StackResourceDetail'
      Core.<$> (x Core..@ "LogicalResourceId")
      Core.<*> (x Core..@ "ResourceType")
      Core.<*> (x Core..@ "LastUpdatedTimestamp")
      Core.<*> (x Core..@ "ResourceStatus")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "DriftInformation")
      Core.<*> (x Core..@? "Metadata")
      Core.<*> (x Core..@? "ModuleInfo")
      Core.<*> (x Core..@? "PhysicalResourceId")
      Core.<*> (x Core..@? "ResourceStatusReason")
      Core.<*> (x Core..@? "StackId")
      Core.<*> (x Core..@? "StackName")
