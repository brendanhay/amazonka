{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceSummary
  ( StackResourceSummary (..),

    -- * Smart constructor
    mkStackResourceSummary,

    -- * Lenses
    srsLogicalResourceId,
    srsPhysicalResourceId,
    srsResourceType,
    srsResourceStatusReason,
    srsResourceStatus,
    srsDriftInformation,
    srsModuleInfo,
    srsLastUpdatedTimestamp,
  )
where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains high-level information about the specified stack resource.
--
-- /See:/ 'mkStackResourceSummary' smart constructor.
data StackResourceSummary = StackResourceSummary'
  { -- | The logical name of the resource specified in the template.
    logicalResourceId :: Lude.Text,
    -- | The name or unique identifier that corresponds to a physical instance ID of the resource.
    physicalResourceId :: Lude.Maybe Lude.Text,
    -- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
    resourceType :: Lude.Text,
    -- | Success/failure message associated with the resource.
    resourceStatusReason :: Lude.Maybe Lude.Text,
    -- | Current status of the resource.
    resourceStatus :: ResourceStatus,
    -- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
    driftInformation :: Lude.Maybe StackResourceDriftInformationSummary,
    -- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
    moduleInfo :: Lude.Maybe ModuleInfo,
    -- | Time the status was updated.
    lastUpdatedTimestamp :: Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackResourceSummary' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The logical name of the resource specified in the template.
-- * 'physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of the resource.
-- * 'resourceType' - Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
-- * 'resourceStatusReason' - Success/failure message associated with the resource.
-- * 'resourceStatus' - Current status of the resource.
-- * 'driftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
-- * 'moduleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
-- * 'lastUpdatedTimestamp' - Time the status was updated.
mkStackResourceSummary ::
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resourceStatus'
  ResourceStatus ->
  -- | 'lastUpdatedTimestamp'
  Lude.DateTime ->
  StackResourceSummary
mkStackResourceSummary
  pLogicalResourceId_
  pResourceType_
  pResourceStatus_
  pLastUpdatedTimestamp_ =
    StackResourceSummary'
      { logicalResourceId = pLogicalResourceId_,
        physicalResourceId = Lude.Nothing,
        resourceType = pResourceType_,
        resourceStatusReason = Lude.Nothing,
        resourceStatus = pResourceStatus_,
        driftInformation = Lude.Nothing,
        moduleInfo = Lude.Nothing,
        lastUpdatedTimestamp = pLastUpdatedTimestamp_
      }

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsLogicalResourceId :: Lens.Lens' StackResourceSummary Lude.Text
srsLogicalResourceId = Lens.lens (logicalResourceId :: StackResourceSummary -> Lude.Text) (\s a -> s {logicalResourceId = a} :: StackResourceSummary)
{-# DEPRECATED srsLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of the resource.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsPhysicalResourceId :: Lens.Lens' StackResourceSummary (Lude.Maybe Lude.Text)
srsPhysicalResourceId = Lens.lens (physicalResourceId :: StackResourceSummary -> Lude.Maybe Lude.Text) (\s a -> s {physicalResourceId = a} :: StackResourceSummary)
{-# DEPRECATED srsPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResourceType :: Lens.Lens' StackResourceSummary Lude.Text
srsResourceType = Lens.lens (resourceType :: StackResourceSummary -> Lude.Text) (\s a -> s {resourceType = a} :: StackResourceSummary)
{-# DEPRECATED srsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Success/failure message associated with the resource.
--
-- /Note:/ Consider using 'resourceStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResourceStatusReason :: Lens.Lens' StackResourceSummary (Lude.Maybe Lude.Text)
srsResourceStatusReason = Lens.lens (resourceStatusReason :: StackResourceSummary -> Lude.Maybe Lude.Text) (\s a -> s {resourceStatusReason = a} :: StackResourceSummary)
{-# DEPRECATED srsResourceStatusReason "Use generic-lens or generic-optics with 'resourceStatusReason' instead." #-}

-- | Current status of the resource.
--
-- /Note:/ Consider using 'resourceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResourceStatus :: Lens.Lens' StackResourceSummary ResourceStatus
srsResourceStatus = Lens.lens (resourceStatus :: StackResourceSummary -> ResourceStatus) (\s a -> s {resourceStatus = a} :: StackResourceSummary)
{-# DEPRECATED srsResourceStatus "Use generic-lens or generic-optics with 'resourceStatus' instead." #-}

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsDriftInformation :: Lens.Lens' StackResourceSummary (Lude.Maybe StackResourceDriftInformationSummary)
srsDriftInformation = Lens.lens (driftInformation :: StackResourceSummary -> Lude.Maybe StackResourceDriftInformationSummary) (\s a -> s {driftInformation = a} :: StackResourceSummary)
{-# DEPRECATED srsDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsModuleInfo :: Lens.Lens' StackResourceSummary (Lude.Maybe ModuleInfo)
srsModuleInfo = Lens.lens (moduleInfo :: StackResourceSummary -> Lude.Maybe ModuleInfo) (\s a -> s {moduleInfo = a} :: StackResourceSummary)
{-# DEPRECATED srsModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | Time the status was updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsLastUpdatedTimestamp :: Lens.Lens' StackResourceSummary Lude.DateTime
srsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: StackResourceSummary -> Lude.DateTime) (\s a -> s {lastUpdatedTimestamp = a} :: StackResourceSummary)
{-# DEPRECATED srsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

instance Lude.FromXML StackResourceSummary where
  parseXML x =
    StackResourceSummary'
      Lude.<$> (x Lude..@ "LogicalResourceId")
      Lude.<*> (x Lude..@? "PhysicalResourceId")
      Lude.<*> (x Lude..@ "ResourceType")
      Lude.<*> (x Lude..@? "ResourceStatusReason")
      Lude.<*> (x Lude..@ "ResourceStatus")
      Lude.<*> (x Lude..@? "DriftInformation")
      Lude.<*> (x Lude..@? "ModuleInfo")
      Lude.<*> (x Lude..@ "LastUpdatedTimestamp")
