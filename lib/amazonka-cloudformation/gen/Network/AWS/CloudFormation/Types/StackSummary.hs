{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSummary
  ( StackSummary (..),

    -- * Smart constructor
    mkStackSummary,

    -- * Lenses
    ssLastUpdatedTime,
    ssRootId,
    ssStackStatusReason,
    ssTemplateDescription,
    ssDriftInformation,
    ssDeletionTime,
    ssStackId,
    ssParentId,
    ssStackName,
    ssCreationTime,
    ssStackStatus,
  )
where

import Network.AWS.CloudFormation.Types.StackDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The StackSummary Data Type
--
-- /See:/ 'mkStackSummary' smart constructor.
data StackSummary = StackSummary'
  { lastUpdatedTime ::
      Lude.Maybe Lude.DateTime,
    rootId :: Lude.Maybe Lude.Text,
    stackStatusReason :: Lude.Maybe Lude.Text,
    templateDescription :: Lude.Maybe Lude.Text,
    driftInformation :: Lude.Maybe StackDriftInformationSummary,
    deletionTime :: Lude.Maybe Lude.DateTime,
    stackId :: Lude.Maybe Lude.Text,
    parentId :: Lude.Maybe Lude.Text,
    stackName :: Lude.Text,
    creationTime :: Lude.DateTime,
    stackStatus :: StackStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time the stack was created.
-- * 'deletionTime' - The time the stack was deleted.
-- * 'driftInformation' - Summarizes information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
-- * 'lastUpdatedTime' - The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
-- * 'parentId' - For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
-- * 'rootId' - For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
-- * 'stackId' - Unique stack identifier.
-- * 'stackName' - The name associated with the stack.
-- * 'stackStatus' - The current status of the stack.
-- * 'stackStatusReason' - Success/Failure message associated with the stack status.
-- * 'templateDescription' - The template description of the template used to create the stack.
mkStackSummary ::
  -- | 'stackName'
  Lude.Text ->
  -- | 'creationTime'
  Lude.DateTime ->
  -- | 'stackStatus'
  StackStatus ->
  StackSummary
mkStackSummary pStackName_ pCreationTime_ pStackStatus_ =
  StackSummary'
    { lastUpdatedTime = Lude.Nothing,
      rootId = Lude.Nothing,
      stackStatusReason = Lude.Nothing,
      templateDescription = Lude.Nothing,
      driftInformation = Lude.Nothing,
      deletionTime = Lude.Nothing,
      stackId = Lude.Nothing,
      parentId = Lude.Nothing,
      stackName = pStackName_,
      creationTime = pCreationTime_,
      stackStatus = pStackStatus_
    }

-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLastUpdatedTime :: Lens.Lens' StackSummary (Lude.Maybe Lude.DateTime)
ssLastUpdatedTime = Lens.lens (lastUpdatedTime :: StackSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {lastUpdatedTime = a} :: StackSummary)
{-# DEPRECATED ssLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRootId :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
ssRootId = Lens.lens (rootId :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {rootId = a} :: StackSummary)
{-# DEPRECATED ssRootId "Use generic-lens or generic-optics with 'rootId' instead." #-}

-- | Success/Failure message associated with the stack status.
--
-- /Note:/ Consider using 'stackStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackStatusReason :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
ssStackStatusReason = Lens.lens (stackStatusReason :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackStatusReason = a} :: StackSummary)
{-# DEPRECATED ssStackStatusReason "Use generic-lens or generic-optics with 'stackStatusReason' instead." #-}

-- | The template description of the template used to create the stack.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTemplateDescription :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
ssTemplateDescription = Lens.lens (templateDescription :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: StackSummary)
{-# DEPRECATED ssTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | Summarizes information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDriftInformation :: Lens.Lens' StackSummary (Lude.Maybe StackDriftInformationSummary)
ssDriftInformation = Lens.lens (driftInformation :: StackSummary -> Lude.Maybe StackDriftInformationSummary) (\s a -> s {driftInformation = a} :: StackSummary)
{-# DEPRECATED ssDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | The time the stack was deleted.
--
-- /Note:/ Consider using 'deletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDeletionTime :: Lens.Lens' StackSummary (Lude.Maybe Lude.DateTime)
ssDeletionTime = Lens.lens (deletionTime :: StackSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {deletionTime = a} :: StackSummary)
{-# DEPRECATED ssDeletionTime "Use generic-lens or generic-optics with 'deletionTime' instead." #-}

-- | Unique stack identifier.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackId :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
ssStackId = Lens.lens (stackId :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: StackSummary)
{-# DEPRECATED ssStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParentId :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
ssParentId = Lens.lens (parentId :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {parentId = a} :: StackSummary)
{-# DEPRECATED ssParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackName :: Lens.Lens' StackSummary Lude.Text
ssStackName = Lens.lens (stackName :: StackSummary -> Lude.Text) (\s a -> s {stackName = a} :: StackSummary)
{-# DEPRECATED ssStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The time the stack was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCreationTime :: Lens.Lens' StackSummary Lude.DateTime
ssCreationTime = Lens.lens (creationTime :: StackSummary -> Lude.DateTime) (\s a -> s {creationTime = a} :: StackSummary)
{-# DEPRECATED ssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The current status of the stack.
--
-- /Note:/ Consider using 'stackStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackStatus :: Lens.Lens' StackSummary StackStatus
ssStackStatus = Lens.lens (stackStatus :: StackSummary -> StackStatus) (\s a -> s {stackStatus = a} :: StackSummary)
{-# DEPRECATED ssStackStatus "Use generic-lens or generic-optics with 'stackStatus' instead." #-}

instance Lude.FromXML StackSummary where
  parseXML x =
    StackSummary'
      Lude.<$> (x Lude..@? "LastUpdatedTime")
      Lude.<*> (x Lude..@? "RootId")
      Lude.<*> (x Lude..@? "StackStatusReason")
      Lude.<*> (x Lude..@? "TemplateDescription")
      Lude.<*> (x Lude..@? "DriftInformation")
      Lude.<*> (x Lude..@? "DeletionTime")
      Lude.<*> (x Lude..@? "StackId")
      Lude.<*> (x Lude..@? "ParentId")
      Lude.<*> (x Lude..@ "StackName")
      Lude.<*> (x Lude..@ "CreationTime")
      Lude.<*> (x Lude..@ "StackStatus")
