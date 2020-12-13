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
    srdfLogicalResourceId,
    srdfActualProperties,
    srdfPhysicalResourceId,
    srdfResourceType,
    srdfPhysicalResourceIdContext,
    srdfPropertyDifferences,
    srdfModuleInfo,
    srdfExpectedProperties,
    srdfStackResourceDriftStatus,
    srdfStackId,
    srdfTimestamp,
  )
where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Network.AWS.CloudFormation.Types.PropertyDifference
import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the drift information for a resource that has been checked for drift. This includes actual and expected property values for resources in which AWS CloudFormation has detected drift. Only resource properties explicitly defined in the stack template are checked for drift. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- Resources that do not currently support drift detection cannot be checked. For a list of resources that support drift detection, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
-- Use 'DetectStackResourceDrift' to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all resources in a given stack that support drift detection.
--
-- /See:/ 'mkStackResourceDrift' smart constructor.
data StackResourceDrift = StackResourceDrift'
  { -- | The logical name of the resource specified in the template.
    logicalResourceId :: Lude.Text,
    -- | A JSON structure containing the actual property values of the stack resource.
    --
    -- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
    actualProperties :: Lude.Maybe Lude.Text,
    -- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
    physicalResourceId :: Lude.Maybe Lude.Text,
    -- | The type of the resource.
    resourceType :: Lude.Text,
    -- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
    physicalResourceIdContext :: Lude.Maybe [PhysicalResourceIdContextKeyValuePair],
    -- | A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
    propertyDifferences :: Lude.Maybe [PropertyDifference],
    -- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
    moduleInfo :: Lude.Maybe ModuleInfo,
    -- | A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.
    --
    -- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
    expectedProperties :: Lude.Maybe Lude.Text,
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
    stackResourceDriftStatus :: StackResourceDriftStatus,
    -- | The ID of the stack.
    stackId :: Lude.Text,
    -- | Time at which AWS CloudFormation performed drift detection on the stack resource.
    timestamp :: Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackResourceDrift' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The logical name of the resource specified in the template.
-- * 'actualProperties' - A JSON structure containing the actual property values of the stack resource.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
-- * 'physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
-- * 'resourceType' - The type of the resource.
-- * 'physicalResourceIdContext' - Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
-- * 'propertyDifferences' - A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
-- * 'moduleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
-- * 'expectedProperties' - A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
-- * 'stackResourceDriftStatus' - Status of the resource's actual configuration compared to its expected configuration
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
-- * 'stackId' - The ID of the stack.
-- * 'timestamp' - Time at which AWS CloudFormation performed drift detection on the stack resource.
mkStackResourceDrift ::
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'stackResourceDriftStatus'
  StackResourceDriftStatus ->
  -- | 'stackId'
  Lude.Text ->
  -- | 'timestamp'
  Lude.DateTime ->
  StackResourceDrift
mkStackResourceDrift
  pLogicalResourceId_
  pResourceType_
  pStackResourceDriftStatus_
  pStackId_
  pTimestamp_ =
    StackResourceDrift'
      { logicalResourceId = pLogicalResourceId_,
        actualProperties = Lude.Nothing,
        physicalResourceId = Lude.Nothing,
        resourceType = pResourceType_,
        physicalResourceIdContext = Lude.Nothing,
        propertyDifferences = Lude.Nothing,
        moduleInfo = Lude.Nothing,
        expectedProperties = Lude.Nothing,
        stackResourceDriftStatus = pStackResourceDriftStatus_,
        stackId = pStackId_,
        timestamp = pTimestamp_
      }

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfLogicalResourceId :: Lens.Lens' StackResourceDrift Lude.Text
srdfLogicalResourceId = Lens.lens (logicalResourceId :: StackResourceDrift -> Lude.Text) (\s a -> s {logicalResourceId = a} :: StackResourceDrift)
{-# DEPRECATED srdfLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | A JSON structure containing the actual property values of the stack resource.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- /Note:/ Consider using 'actualProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfActualProperties :: Lens.Lens' StackResourceDrift (Lude.Maybe Lude.Text)
srdfActualProperties = Lens.lens (actualProperties :: StackResourceDrift -> Lude.Maybe Lude.Text) (\s a -> s {actualProperties = a} :: StackResourceDrift)
{-# DEPRECATED srdfActualProperties "Use generic-lens or generic-optics with 'actualProperties' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfPhysicalResourceId :: Lens.Lens' StackResourceDrift (Lude.Maybe Lude.Text)
srdfPhysicalResourceId = Lens.lens (physicalResourceId :: StackResourceDrift -> Lude.Maybe Lude.Text) (\s a -> s {physicalResourceId = a} :: StackResourceDrift)
{-# DEPRECATED srdfPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | The type of the resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfResourceType :: Lens.Lens' StackResourceDrift Lude.Text
srdfResourceType = Lens.lens (resourceType :: StackResourceDrift -> Lude.Text) (\s a -> s {resourceType = a} :: StackResourceDrift)
{-# DEPRECATED srdfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
--
-- /Note:/ Consider using 'physicalResourceIdContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfPhysicalResourceIdContext :: Lens.Lens' StackResourceDrift (Lude.Maybe [PhysicalResourceIdContextKeyValuePair])
srdfPhysicalResourceIdContext = Lens.lens (physicalResourceIdContext :: StackResourceDrift -> Lude.Maybe [PhysicalResourceIdContextKeyValuePair]) (\s a -> s {physicalResourceIdContext = a} :: StackResourceDrift)
{-# DEPRECATED srdfPhysicalResourceIdContext "Use generic-lens or generic-optics with 'physicalResourceIdContext' instead." #-}

-- | A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
--
-- /Note:/ Consider using 'propertyDifferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfPropertyDifferences :: Lens.Lens' StackResourceDrift (Lude.Maybe [PropertyDifference])
srdfPropertyDifferences = Lens.lens (propertyDifferences :: StackResourceDrift -> Lude.Maybe [PropertyDifference]) (\s a -> s {propertyDifferences = a} :: StackResourceDrift)
{-# DEPRECATED srdfPropertyDifferences "Use generic-lens or generic-optics with 'propertyDifferences' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfModuleInfo :: Lens.Lens' StackResourceDrift (Lude.Maybe ModuleInfo)
srdfModuleInfo = Lens.lens (moduleInfo :: StackResourceDrift -> Lude.Maybe ModuleInfo) (\s a -> s {moduleInfo = a} :: StackResourceDrift)
{-# DEPRECATED srdfModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- /Note:/ Consider using 'expectedProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfExpectedProperties :: Lens.Lens' StackResourceDrift (Lude.Maybe Lude.Text)
srdfExpectedProperties = Lens.lens (expectedProperties :: StackResourceDrift -> Lude.Maybe Lude.Text) (\s a -> s {expectedProperties = a} :: StackResourceDrift)
{-# DEPRECATED srdfExpectedProperties "Use generic-lens or generic-optics with 'expectedProperties' instead." #-}

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
srdfStackResourceDriftStatus :: Lens.Lens' StackResourceDrift StackResourceDriftStatus
srdfStackResourceDriftStatus = Lens.lens (stackResourceDriftStatus :: StackResourceDrift -> StackResourceDriftStatus) (\s a -> s {stackResourceDriftStatus = a} :: StackResourceDrift)
{-# DEPRECATED srdfStackResourceDriftStatus "Use generic-lens or generic-optics with 'stackResourceDriftStatus' instead." #-}

-- | The ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfStackId :: Lens.Lens' StackResourceDrift Lude.Text
srdfStackId = Lens.lens (stackId :: StackResourceDrift -> Lude.Text) (\s a -> s {stackId = a} :: StackResourceDrift)
{-# DEPRECATED srdfStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | Time at which AWS CloudFormation performed drift detection on the stack resource.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdfTimestamp :: Lens.Lens' StackResourceDrift Lude.DateTime
srdfTimestamp = Lens.lens (timestamp :: StackResourceDrift -> Lude.DateTime) (\s a -> s {timestamp = a} :: StackResourceDrift)
{-# DEPRECATED srdfTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML StackResourceDrift where
  parseXML x =
    StackResourceDrift'
      Lude.<$> (x Lude..@ "LogicalResourceId")
      Lude.<*> (x Lude..@? "ActualProperties")
      Lude.<*> (x Lude..@? "PhysicalResourceId")
      Lude.<*> (x Lude..@ "ResourceType")
      Lude.<*> ( x Lude..@? "PhysicalResourceIdContext" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "PropertyDifferences" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ModuleInfo")
      Lude.<*> (x Lude..@? "ExpectedProperties")
      Lude.<*> (x Lude..@ "StackResourceDriftStatus")
      Lude.<*> (x Lude..@ "StackId")
      Lude.<*> (x Lude..@ "Timestamp")
