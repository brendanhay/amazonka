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
    srdPhysicalResourceId,
    srdResourceType,
    srdResourceStatusReason,
    srdResourceStatus,
    srdDriftInformation,
    srdModuleInfo,
    srdMetadata,
    srdStackId,
    srdDescription,
    srdLastUpdatedTimestamp,
    srdStackName,
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
  { -- | The logical name of the resource specified in the template.
    logicalResourceId :: Lude.Text,
    -- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
    physicalResourceId :: Lude.Maybe Lude.Text,
    -- | Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
    resourceType :: Lude.Text,
    -- | Success/failure message associated with the resource.
    resourceStatusReason :: Lude.Maybe Lude.Text,
    -- | Current status of the resource.
    resourceStatus :: ResourceStatus,
    -- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
    driftInformation :: Lude.Maybe StackResourceDriftInformation,
    -- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
    moduleInfo :: Lude.Maybe ModuleInfo,
    -- | The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
    metadata :: Lude.Maybe Lude.Text,
    -- | Unique identifier of the stack.
    stackId :: Lude.Maybe Lude.Text,
    -- | User defined description associated with the resource.
    description :: Lude.Maybe Lude.Text,
    -- | Time the status was updated.
    lastUpdatedTimestamp :: Lude.DateTime,
    -- | The name associated with the stack.
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackResourceDetail' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The logical name of the resource specified in the template.
-- * 'physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
-- * 'resourceType' - Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
-- * 'resourceStatusReason' - Success/failure message associated with the resource.
-- * 'resourceStatus' - Current status of the resource.
-- * 'driftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
-- * 'moduleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
-- * 'metadata' - The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
-- * 'stackId' - Unique identifier of the stack.
-- * 'description' - User defined description associated with the resource.
-- * 'lastUpdatedTimestamp' - Time the status was updated.
-- * 'stackName' - The name associated with the stack.
mkStackResourceDetail ::
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resourceStatus'
  ResourceStatus ->
  -- | 'lastUpdatedTimestamp'
  Lude.DateTime ->
  StackResourceDetail
mkStackResourceDetail
  pLogicalResourceId_
  pResourceType_
  pResourceStatus_
  pLastUpdatedTimestamp_ =
    StackResourceDetail'
      { logicalResourceId = pLogicalResourceId_,
        physicalResourceId = Lude.Nothing,
        resourceType = pResourceType_,
        resourceStatusReason = Lude.Nothing,
        resourceStatus = pResourceStatus_,
        driftInformation = Lude.Nothing,
        moduleInfo = Lude.Nothing,
        metadata = Lude.Nothing,
        stackId = Lude.Nothing,
        description = Lude.Nothing,
        lastUpdatedTimestamp = pLastUpdatedTimestamp_,
        stackName = Lude.Nothing
      }

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdLogicalResourceId :: Lens.Lens' StackResourceDetail Lude.Text
srdLogicalResourceId = Lens.lens (logicalResourceId :: StackResourceDetail -> Lude.Text) (\s a -> s {logicalResourceId = a} :: StackResourceDetail)
{-# DEPRECATED srdLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdPhysicalResourceId :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
srdPhysicalResourceId = Lens.lens (physicalResourceId :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {physicalResourceId = a} :: StackResourceDetail)
{-# DEPRECATED srdPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdResourceType :: Lens.Lens' StackResourceDetail Lude.Text
srdResourceType = Lens.lens (resourceType :: StackResourceDetail -> Lude.Text) (\s a -> s {resourceType = a} :: StackResourceDetail)
{-# DEPRECATED srdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Success/failure message associated with the resource.
--
-- /Note:/ Consider using 'resourceStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdResourceStatusReason :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
srdResourceStatusReason = Lens.lens (resourceStatusReason :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {resourceStatusReason = a} :: StackResourceDetail)
{-# DEPRECATED srdResourceStatusReason "Use generic-lens or generic-optics with 'resourceStatusReason' instead." #-}

-- | Current status of the resource.
--
-- /Note:/ Consider using 'resourceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdResourceStatus :: Lens.Lens' StackResourceDetail ResourceStatus
srdResourceStatus = Lens.lens (resourceStatus :: StackResourceDetail -> ResourceStatus) (\s a -> s {resourceStatus = a} :: StackResourceDetail)
{-# DEPRECATED srdResourceStatus "Use generic-lens or generic-optics with 'resourceStatus' instead." #-}

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdDriftInformation :: Lens.Lens' StackResourceDetail (Lude.Maybe StackResourceDriftInformation)
srdDriftInformation = Lens.lens (driftInformation :: StackResourceDetail -> Lude.Maybe StackResourceDriftInformation) (\s a -> s {driftInformation = a} :: StackResourceDetail)
{-# DEPRECATED srdDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdModuleInfo :: Lens.Lens' StackResourceDetail (Lude.Maybe ModuleInfo)
srdModuleInfo = Lens.lens (moduleInfo :: StackResourceDetail -> Lude.Maybe ModuleInfo) (\s a -> s {moduleInfo = a} :: StackResourceDetail)
{-# DEPRECATED srdModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdMetadata :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
srdMetadata = Lens.lens (metadata :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {metadata = a} :: StackResourceDetail)
{-# DEPRECATED srdMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdStackId :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
srdStackId = Lens.lens (stackId :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: StackResourceDetail)
{-# DEPRECATED srdStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | User defined description associated with the resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdDescription :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
srdDescription = Lens.lens (description :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StackResourceDetail)
{-# DEPRECATED srdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Time the status was updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdLastUpdatedTimestamp :: Lens.Lens' StackResourceDetail Lude.DateTime
srdLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: StackResourceDetail -> Lude.DateTime) (\s a -> s {lastUpdatedTimestamp = a} :: StackResourceDetail)
{-# DEPRECATED srdLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdStackName :: Lens.Lens' StackResourceDetail (Lude.Maybe Lude.Text)
srdStackName = Lens.lens (stackName :: StackResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: StackResourceDetail)
{-# DEPRECATED srdStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.FromXML StackResourceDetail where
  parseXML x =
    StackResourceDetail'
      Lude.<$> (x Lude..@ "LogicalResourceId")
      Lude.<*> (x Lude..@? "PhysicalResourceId")
      Lude.<*> (x Lude..@ "ResourceType")
      Lude.<*> (x Lude..@? "ResourceStatusReason")
      Lude.<*> (x Lude..@ "ResourceStatus")
      Lude.<*> (x Lude..@? "DriftInformation")
      Lude.<*> (x Lude..@? "ModuleInfo")
      Lude.<*> (x Lude..@? "Metadata")
      Lude.<*> (x Lude..@? "StackId")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> (x Lude..@ "LastUpdatedTimestamp")
      Lude.<*> (x Lude..@? "StackName")
