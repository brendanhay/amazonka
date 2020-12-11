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
    sPhysicalResourceId,
    sResourceStatusReason,
    sDriftInformation,
    sModuleInfo,
    sMetadata,
    sStackId,
    sDescription,
    sStackName,
    sLogicalResourceId,
    sResourceType,
    sLastUpdatedTimestamp,
    sResourceStatus,
  )
where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains detailed information about the specified stack resource.
--
-- /See:/ 'mkStackResourceDetail' smart constructor.
data StackResourceDetail = StackResourceDetail'
  { physicalResourceId ::
      Lude.Maybe Lude.Text,
    resourceStatusReason :: Lude.Maybe Lude.Text,
    driftInformation ::
      Lude.Maybe StackResourceDriftInformation,
    moduleInfo :: Lude.Maybe ModuleInfo,
    metadata :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    stackName :: Lude.Maybe Lude.Text,
    logicalResourceId :: Lude.Text,
    resourceType :: Lude.Text,
    lastUpdatedTimestamp :: Lude.ISO8601,
    resourceStatus :: ResourceStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackResourceDetail' with the minimum fields required to make a request.
--
-- * 'description' - User defined description associated with the resource.
-- * 'driftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
-- * 'lastUpdatedTimestamp' - Time the status was updated.
-- * 'logicalResourceId' - The logical name of the resource specified in the template.
-- * 'metadata' - The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
-- * 'moduleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
-- * 'physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
-- * 'resourceStatus' - Current status of the resource.
-- * 'resourceStatusReason' - Success/failure message associated with the resource.
-- * 'resourceType' - Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
-- * 'stackId' - Unique identifier of the stack.
-- * 'stackName' - The name associated with the stack.
mkStackResourceDetail ::
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'lastUpdatedTimestamp'
  Lude.ISO8601 ->
  -- | 'resourceStatus'
  ResourceStatus ->
  StackResourceDetail
mkStackResourceDetail
  pLogicalResourceId_
  pResourceType_
  pLastUpdatedTimestamp_
  pResourceStatus_ =
    StackResourceDetail'
      { physicalResourceId = Lude.Nothing,
        resourceStatusReason = Lude.Nothing,
        driftInformation = Lude.Nothing,
        moduleInfo = Lude.Nothing,
        metadata = Lude.Nothing,
        stackId = Lude.Nothing,
        description = Lude.Nothing,
        stackName = Lude.Nothing,
        logicalResourceId = pLogicalResourceId_,
        resourceType = pResourceType_,
        lastUpdatedTimestamp = pLastUpdatedTimestamp_,
        resourceStatus = pResourceStatus_
      }

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPhysicalResourceId :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
sPhysicalResourceId = Lens.lens (physicalResourceId :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {physicalResourceId = a} :: StackResourceDetail)
{-# DEPRECATED sPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | Success/failure message associated with the resource.
--
-- /Note:/ Consider using 'resourceStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceStatusReason :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
sResourceStatusReason = Lens.lens (resourceStatusReason :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {resourceStatusReason = a} :: StackResourceDetail)
{-# DEPRECATED sResourceStatusReason "Use generic-lens or generic-optics with 'resourceStatusReason' instead." #-}

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDriftInformation :: Lens.Lens' StackResourceDetail (Lude.Maybe StackResourceDriftInformation)
sDriftInformation = Lens.lens (driftInformation :: StackResourceDetail -> Lude.Maybe StackResourceDriftInformation) (\s a -> s {driftInformation = a} :: StackResourceDetail)
{-# DEPRECATED sDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sModuleInfo :: Lens.Lens' StackResourceDetail (Lude.Maybe ModuleInfo)
sModuleInfo = Lens.lens (moduleInfo :: StackResourceDetail -> Lude.Maybe ModuleInfo) (\s a -> s {moduleInfo = a} :: StackResourceDetail)
{-# DEPRECATED sModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMetadata :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
sMetadata = Lens.lens (metadata :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {metadata = a} :: StackResourceDetail)
{-# DEPRECATED sMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackId :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
sStackId = Lens.lens (stackId :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: StackResourceDetail)
{-# DEPRECATED sStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | User defined description associated with the resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
sDescription = Lens.lens (description :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StackResourceDetail)
{-# DEPRECATED sDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackName :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
sStackName = Lens.lens (stackName :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: StackResourceDetail)
{-# DEPRECATED sStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLogicalResourceId :: Lens.Lens' StackResourceDetail Lude.Text
sLogicalResourceId = Lens.lens (logicalResourceId :: StackResourceDetail -> Lude.Text) (\s a -> s {logicalResourceId = a} :: StackResourceDetail)
{-# DEPRECATED sLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceType :: Lens.Lens' StackResourceDetail Lude.Text
sResourceType = Lens.lens (resourceType :: StackResourceDetail -> Lude.Text) (\s a -> s {resourceType = a} :: StackResourceDetail)
{-# DEPRECATED sResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Time the status was updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLastUpdatedTimestamp :: Lens.Lens' StackResourceDetail Lude.ISO8601
sLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: StackResourceDetail -> Lude.ISO8601) (\s a -> s {lastUpdatedTimestamp = a} :: StackResourceDetail)
{-# DEPRECATED sLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Current status of the resource.
--
-- /Note:/ Consider using 'resourceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceStatus :: Lens.Lens' StackResourceDetail ResourceStatus
sResourceStatus = Lens.lens (resourceStatus :: StackResourceDetail -> ResourceStatus) (\s a -> s {resourceStatus = a} :: StackResourceDetail)
{-# DEPRECATED sResourceStatus "Use generic-lens or generic-optics with 'resourceStatus' instead." #-}

instance Lude.FromXML StackResourceDetail where
  parseXML x =
    StackResourceDetail'
      Lude.<$> (x Lude..@? "PhysicalResourceId")
      Lude.<*> (x Lude..@? "ResourceStatusReason")
      Lude.<*> (x Lude..@? "DriftInformation")
      Lude.<*> (x Lude..@? "ModuleInfo")
      Lude.<*> (x Lude..@? "Metadata")
      Lude.<*> (x Lude..@? "StackId")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> (x Lude..@? "StackName")
      Lude.<*> (x Lude..@ "LogicalResourceId")
      Lude.<*> (x Lude..@ "ResourceType")
      Lude.<*> (x Lude..@ "LastUpdatedTimestamp")
      Lude.<*> (x Lude..@ "ResourceStatus")
