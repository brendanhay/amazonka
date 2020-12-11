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
    srdActualProperties,
    srdPhysicalResourceId,
    srdPhysicalResourceIdContext,
    srdPropertyDifferences,
    srdModuleInfo,
    srdExpectedProperties,
    srdStackId,
    srdLogicalResourceId,
    srdResourceType,
    srdStackResourceDriftStatus,
    srdTimestamp,
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
  { actualProperties ::
      Lude.Maybe Lude.Text,
    physicalResourceId :: Lude.Maybe Lude.Text,
    physicalResourceIdContext ::
      Lude.Maybe [PhysicalResourceIdContextKeyValuePair],
    propertyDifferences ::
      Lude.Maybe [PropertyDifference],
    moduleInfo :: Lude.Maybe ModuleInfo,
    expectedProperties :: Lude.Maybe Lude.Text,
    stackId :: Lude.Text,
    logicalResourceId :: Lude.Text,
    resourceType :: Lude.Text,
    stackResourceDriftStatus :: StackResourceDriftStatus,
    timestamp :: Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackResourceDrift' with the minimum fields required to make a request.
--
-- * 'actualProperties' - A JSON structure containing the actual property values of the stack resource.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
-- * 'expectedProperties' - A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
-- * 'logicalResourceId' - The logical name of the resource specified in the template.
-- * 'moduleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
-- * 'physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
-- * 'physicalResourceIdContext' - Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
-- * 'propertyDifferences' - A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
-- * 'resourceType' - The type of the resource.
-- * 'stackId' - The ID of the stack.
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
-- * 'timestamp' - Time at which AWS CloudFormation performed drift detection on the stack resource.
mkStackResourceDrift ::
  -- | 'stackId'
  Lude.Text ->
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'stackResourceDriftStatus'
  StackResourceDriftStatus ->
  -- | 'timestamp'
  Lude.ISO8601 ->
  StackResourceDrift
mkStackResourceDrift
  pStackId_
  pLogicalResourceId_
  pResourceType_
  pStackResourceDriftStatus_
  pTimestamp_ =
    StackResourceDrift'
      { actualProperties = Lude.Nothing,
        physicalResourceId = Lude.Nothing,
        physicalResourceIdContext = Lude.Nothing,
        propertyDifferences = Lude.Nothing,
        moduleInfo = Lude.Nothing,
        expectedProperties = Lude.Nothing,
        stackId = pStackId_,
        logicalResourceId = pLogicalResourceId_,
        resourceType = pResourceType_,
        stackResourceDriftStatus = pStackResourceDriftStatus_,
        timestamp = pTimestamp_
      }

-- | A JSON structure containing the actual property values of the stack resource.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- /Note:/ Consider using 'actualProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdActualProperties :: Lens.Lens' StackResourceDrift (Lude.Maybe Lude.Text)
srdActualProperties = Lens.lens (actualProperties :: StackResourceDrift -> Lude.Maybe Lude.Text) (\s a -> s {actualProperties = a} :: StackResourceDrift)
{-# DEPRECATED srdActualProperties "Use generic-lens or generic-optics with 'actualProperties' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdPhysicalResourceId :: Lens.Lens' StackResourceDrift (Lude.Maybe Lude.Text)
srdPhysicalResourceId = Lens.lens (physicalResourceId :: StackResourceDrift -> Lude.Maybe Lude.Text) (\s a -> s {physicalResourceId = a} :: StackResourceDrift)
{-# DEPRECATED srdPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
--
-- /Note:/ Consider using 'physicalResourceIdContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdPhysicalResourceIdContext :: Lens.Lens' StackResourceDrift (Lude.Maybe [PhysicalResourceIdContextKeyValuePair])
srdPhysicalResourceIdContext = Lens.lens (physicalResourceIdContext :: StackResourceDrift -> Lude.Maybe [PhysicalResourceIdContextKeyValuePair]) (\s a -> s {physicalResourceIdContext = a} :: StackResourceDrift)
{-# DEPRECATED srdPhysicalResourceIdContext "Use generic-lens or generic-optics with 'physicalResourceIdContext' instead." #-}

-- | A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
--
-- /Note:/ Consider using 'propertyDifferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdPropertyDifferences :: Lens.Lens' StackResourceDrift (Lude.Maybe [PropertyDifference])
srdPropertyDifferences = Lens.lens (propertyDifferences :: StackResourceDrift -> Lude.Maybe [PropertyDifference]) (\s a -> s {propertyDifferences = a} :: StackResourceDrift)
{-# DEPRECATED srdPropertyDifferences "Use generic-lens or generic-optics with 'propertyDifferences' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdModuleInfo :: Lens.Lens' StackResourceDrift (Lude.Maybe ModuleInfo)
srdModuleInfo = Lens.lens (moduleInfo :: StackResourceDrift -> Lude.Maybe ModuleInfo) (\s a -> s {moduleInfo = a} :: StackResourceDrift)
{-# DEPRECATED srdModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- /Note:/ Consider using 'expectedProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdExpectedProperties :: Lens.Lens' StackResourceDrift (Lude.Maybe Lude.Text)
srdExpectedProperties = Lens.lens (expectedProperties :: StackResourceDrift -> Lude.Maybe Lude.Text) (\s a -> s {expectedProperties = a} :: StackResourceDrift)
{-# DEPRECATED srdExpectedProperties "Use generic-lens or generic-optics with 'expectedProperties' instead." #-}

-- | The ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdStackId :: Lens.Lens' StackResourceDrift Lude.Text
srdStackId = Lens.lens (stackId :: StackResourceDrift -> Lude.Text) (\s a -> s {stackId = a} :: StackResourceDrift)
{-# DEPRECATED srdStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdLogicalResourceId :: Lens.Lens' StackResourceDrift Lude.Text
srdLogicalResourceId = Lens.lens (logicalResourceId :: StackResourceDrift -> Lude.Text) (\s a -> s {logicalResourceId = a} :: StackResourceDrift)
{-# DEPRECATED srdLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The type of the resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdResourceType :: Lens.Lens' StackResourceDrift Lude.Text
srdResourceType = Lens.lens (resourceType :: StackResourceDrift -> Lude.Text) (\s a -> s {resourceType = a} :: StackResourceDrift)
{-# DEPRECATED srdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

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
srdStackResourceDriftStatus :: Lens.Lens' StackResourceDrift StackResourceDriftStatus
srdStackResourceDriftStatus = Lens.lens (stackResourceDriftStatus :: StackResourceDrift -> StackResourceDriftStatus) (\s a -> s {stackResourceDriftStatus = a} :: StackResourceDrift)
{-# DEPRECATED srdStackResourceDriftStatus "Use generic-lens or generic-optics with 'stackResourceDriftStatus' instead." #-}

-- | Time at which AWS CloudFormation performed drift detection on the stack resource.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdTimestamp :: Lens.Lens' StackResourceDrift Lude.ISO8601
srdTimestamp = Lens.lens (timestamp :: StackResourceDrift -> Lude.ISO8601) (\s a -> s {timestamp = a} :: StackResourceDrift)
{-# DEPRECATED srdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML StackResourceDrift where
  parseXML x =
    StackResourceDrift'
      Lude.<$> (x Lude..@? "ActualProperties")
      Lude.<*> (x Lude..@? "PhysicalResourceId")
      Lude.<*> ( x Lude..@? "PhysicalResourceIdContext" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "PropertyDifferences" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ModuleInfo")
      Lude.<*> (x Lude..@? "ExpectedProperties")
      Lude.<*> (x Lude..@ "StackId")
      Lude.<*> (x Lude..@ "LogicalResourceId")
      Lude.<*> (x Lude..@ "ResourceType")
      Lude.<*> (x Lude..@ "StackResourceDriftStatus")
      Lude.<*> (x Lude..@ "Timestamp")
