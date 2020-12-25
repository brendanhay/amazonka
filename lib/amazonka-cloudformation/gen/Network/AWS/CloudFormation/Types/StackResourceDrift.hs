{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDrift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDrift
  ( StackResourceDrift (..),

    -- * Smart constructor
    mkStackResourceDrift,

    -- * Lenses
    sStackId,
    sLogicalResourceId,
    sResourceType,
    sStackResourceDriftStatus,
    sTimestamp,
    sActualProperties,
    sExpectedProperties,
    sModuleInfo,
    sPhysicalResourceId,
    sPhysicalResourceIdContext,
    sPropertyDifferences,
  )
where

import qualified Network.AWS.CloudFormation.Types.ActualProperties as Types
import qualified Network.AWS.CloudFormation.Types.ExpectedProperties as Types
import qualified Network.AWS.CloudFormation.Types.LogicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.ModuleInfo as Types
import qualified Network.AWS.CloudFormation.Types.PhysicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair as Types
import qualified Network.AWS.CloudFormation.Types.PropertyDifference as Types
import qualified Network.AWS.CloudFormation.Types.ResourceType as Types
import qualified Network.AWS.CloudFormation.Types.StackId as Types
import qualified Network.AWS.CloudFormation.Types.StackResourceDriftStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the drift information for a resource that has been checked for drift. This includes actual and expected property values for resources in which AWS CloudFormation has detected drift. Only resource properties explicitly defined in the stack template are checked for drift. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- Resources that do not currently support drift detection cannot be checked. For a list of resources that support drift detection, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
-- Use 'DetectStackResourceDrift' to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all resources in a given stack that support drift detection.
--
-- /See:/ 'mkStackResourceDrift' smart constructor.
data StackResourceDrift = StackResourceDrift'
  { -- | The ID of the stack.
    stackId :: Types.StackId,
    -- | The logical name of the resource specified in the template.
    logicalResourceId :: Types.LogicalResourceId,
    -- | The type of the resource.
    resourceType :: Types.ResourceType,
    -- | Status of the resource's actual configuration compared to its expected configuration
    --
    --
    --     * @DELETED@ : The resource differs from its expected template configuration because the resource has been deleted.
    --
    --
    --     * @MODIFIED@ : One or more resource properties differ from their expected values (as defined in the stack template and any values specified as template parameters).
    --
    --
    --     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.
    --
    --
    --     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
    stackResourceDriftStatus :: Types.StackResourceDriftStatus,
    -- | Time at which AWS CloudFormation performed drift detection on the stack resource.
    timestamp :: Core.UTCTime,
    -- | A JSON structure containing the actual property values of the stack resource.
    --
    -- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
    actualProperties :: Core.Maybe Types.ActualProperties,
    -- | A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.
    --
    -- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
    expectedProperties :: Core.Maybe Types.ExpectedProperties,
    -- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
    moduleInfo :: Core.Maybe Types.ModuleInfo,
    -- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
    physicalResourceId :: Core.Maybe Types.PhysicalResourceId,
    -- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
    physicalResourceIdContext :: Core.Maybe [Types.PhysicalResourceIdContextKeyValuePair],
    -- | A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
    propertyDifferences :: Core.Maybe [Types.PropertyDifference]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StackResourceDrift' value with any optional fields omitted.
mkStackResourceDrift ::
  -- | 'stackId'
  Types.StackId ->
  -- | 'logicalResourceId'
  Types.LogicalResourceId ->
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'stackResourceDriftStatus'
  Types.StackResourceDriftStatus ->
  -- | 'timestamp'
  Core.UTCTime ->
  StackResourceDrift
mkStackResourceDrift
  stackId
  logicalResourceId
  resourceType
  stackResourceDriftStatus
  timestamp =
    StackResourceDrift'
      { stackId,
        logicalResourceId,
        resourceType,
        stackResourceDriftStatus,
        timestamp,
        actualProperties = Core.Nothing,
        expectedProperties = Core.Nothing,
        moduleInfo = Core.Nothing,
        physicalResourceId = Core.Nothing,
        physicalResourceIdContext = Core.Nothing,
        propertyDifferences = Core.Nothing
      }

-- | The ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackId :: Lens.Lens' StackResourceDrift Types.StackId
sStackId = Lens.field @"stackId"
{-# DEPRECATED sStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLogicalResourceId :: Lens.Lens' StackResourceDrift Types.LogicalResourceId
sLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED sLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The type of the resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceType :: Lens.Lens' StackResourceDrift Types.ResourceType
sResourceType = Lens.field @"resourceType"
{-# DEPRECATED sResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Status of the resource's actual configuration compared to its expected configuration
--
--
--     * @DELETED@ : The resource differs from its expected template configuration because the resource has been deleted.
--
--
--     * @MODIFIED@ : One or more resource properties differ from their expected values (as defined in the stack template and any values specified as template parameters).
--
--
--     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
--
--
--
-- /Note:/ Consider using 'stackResourceDriftStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackResourceDriftStatus :: Lens.Lens' StackResourceDrift Types.StackResourceDriftStatus
sStackResourceDriftStatus = Lens.field @"stackResourceDriftStatus"
{-# DEPRECATED sStackResourceDriftStatus "Use generic-lens or generic-optics with 'stackResourceDriftStatus' instead." #-}

-- | Time at which AWS CloudFormation performed drift detection on the stack resource.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimestamp :: Lens.Lens' StackResourceDrift Core.UTCTime
sTimestamp = Lens.field @"timestamp"
{-# DEPRECATED sTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | A JSON structure containing the actual property values of the stack resource.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- /Note:/ Consider using 'actualProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActualProperties :: Lens.Lens' StackResourceDrift (Core.Maybe Types.ActualProperties)
sActualProperties = Lens.field @"actualProperties"
{-# DEPRECATED sActualProperties "Use generic-lens or generic-optics with 'actualProperties' instead." #-}

-- | A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- /Note:/ Consider using 'expectedProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExpectedProperties :: Lens.Lens' StackResourceDrift (Core.Maybe Types.ExpectedProperties)
sExpectedProperties = Lens.field @"expectedProperties"
{-# DEPRECATED sExpectedProperties "Use generic-lens or generic-optics with 'expectedProperties' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sModuleInfo :: Lens.Lens' StackResourceDrift (Core.Maybe Types.ModuleInfo)
sModuleInfo = Lens.field @"moduleInfo"
{-# DEPRECATED sModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPhysicalResourceId :: Lens.Lens' StackResourceDrift (Core.Maybe Types.PhysicalResourceId)
sPhysicalResourceId = Lens.field @"physicalResourceId"
{-# DEPRECATED sPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
--
-- /Note:/ Consider using 'physicalResourceIdContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPhysicalResourceIdContext :: Lens.Lens' StackResourceDrift (Core.Maybe [Types.PhysicalResourceIdContextKeyValuePair])
sPhysicalResourceIdContext = Lens.field @"physicalResourceIdContext"
{-# DEPRECATED sPhysicalResourceIdContext "Use generic-lens or generic-optics with 'physicalResourceIdContext' instead." #-}

-- | A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
--
-- /Note:/ Consider using 'propertyDifferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPropertyDifferences :: Lens.Lens' StackResourceDrift (Core.Maybe [Types.PropertyDifference])
sPropertyDifferences = Lens.field @"propertyDifferences"
{-# DEPRECATED sPropertyDifferences "Use generic-lens or generic-optics with 'propertyDifferences' instead." #-}

instance Core.FromXML StackResourceDrift where
  parseXML x =
    StackResourceDrift'
      Core.<$> (x Core..@ "StackId")
      Core.<*> (x Core..@ "LogicalResourceId")
      Core.<*> (x Core..@ "ResourceType")
      Core.<*> (x Core..@ "StackResourceDriftStatus")
      Core.<*> (x Core..@ "Timestamp")
      Core.<*> (x Core..@? "ActualProperties")
      Core.<*> (x Core..@? "ExpectedProperties")
      Core.<*> (x Core..@? "ModuleInfo")
      Core.<*> (x Core..@? "PhysicalResourceId")
      Core.<*> ( x Core..@? "PhysicalResourceIdContext"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "PropertyDifferences"
                   Core..<@> Core.parseXMLList "member"
               )
