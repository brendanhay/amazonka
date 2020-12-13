{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSetSummary
  ( ChangeSetSummary (..),

    -- * Smart constructor
    mkChangeSetSummary,

    -- * Lenses
    cssCreationTime,
    cssStatus,
    cssParentChangeSetId,
    cssChangeSetName,
    cssExecutionStatus,
    cssChangeSetId,
    cssIncludeNestedStacks,
    cssRootChangeSetId,
    cssStatusReason,
    cssStackId,
    cssDescription,
    cssStackName,
  )
where

import Network.AWS.CloudFormation.Types.ChangeSetStatus
import Network.AWS.CloudFormation.Types.ExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @ChangeSetSummary@ structure describes a change set, its status, and the stack with which it's associated.
--
-- /See:/ 'mkChangeSetSummary' smart constructor.
data ChangeSetSummary = ChangeSetSummary'
  { -- | The start time when the change set was created, in UTC.
    creationTime :: Lude.Maybe Lude.DateTime,
    -- | The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
    status :: Lude.Maybe ChangeSetStatus,
    -- | The parent change set ID.
    parentChangeSetId :: Lude.Maybe Lude.Text,
    -- | The name of the change set.
    changeSetName :: Lude.Maybe Lude.Text,
    -- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
    executionStatus :: Lude.Maybe ExecutionStatus,
    -- | The ID of the change set.
    changeSetId :: Lude.Maybe Lude.Text,
    -- | Specifies the current setting of @IncludeNestedStacks@ for the change set.
    includeNestedStacks :: Lude.Maybe Lude.Bool,
    -- | The root change set ID.
    rootChangeSetId :: Lude.Maybe Lude.Text,
    -- | A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
    statusReason :: Lude.Maybe Lude.Text,
    -- | The ID of the stack with which the change set is associated.
    stackId :: Lude.Maybe Lude.Text,
    -- | Descriptive information about the change set.
    description :: Lude.Maybe Lude.Text,
    -- | The name of the stack with which the change set is associated.
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeSetSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The start time when the change set was created, in UTC.
-- * 'status' - The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
-- * 'parentChangeSetId' - The parent change set ID.
-- * 'changeSetName' - The name of the change set.
-- * 'executionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
-- * 'changeSetId' - The ID of the change set.
-- * 'includeNestedStacks' - Specifies the current setting of @IncludeNestedStacks@ for the change set.
-- * 'rootChangeSetId' - The root change set ID.
-- * 'statusReason' - A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
-- * 'stackId' - The ID of the stack with which the change set is associated.
-- * 'description' - Descriptive information about the change set.
-- * 'stackName' - The name of the stack with which the change set is associated.
mkChangeSetSummary ::
  ChangeSetSummary
mkChangeSetSummary =
  ChangeSetSummary'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      parentChangeSetId = Lude.Nothing,
      changeSetName = Lude.Nothing,
      executionStatus = Lude.Nothing,
      changeSetId = Lude.Nothing,
      includeNestedStacks = Lude.Nothing,
      rootChangeSetId = Lude.Nothing,
      statusReason = Lude.Nothing,
      stackId = Lude.Nothing,
      description = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | The start time when the change set was created, in UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssCreationTime :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.DateTime)
cssCreationTime = Lens.lens (creationTime :: ChangeSetSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {creationTime = a} :: ChangeSetSummary)
{-# DEPRECATED cssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStatus :: Lens.Lens' ChangeSetSummary (Lude.Maybe ChangeSetStatus)
cssStatus = Lens.lens (status :: ChangeSetSummary -> Lude.Maybe ChangeSetStatus) (\s a -> s {status = a} :: ChangeSetSummary)
{-# DEPRECATED cssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The parent change set ID.
--
-- /Note:/ Consider using 'parentChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssParentChangeSetId :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cssParentChangeSetId = Lens.lens (parentChangeSetId :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {parentChangeSetId = a} :: ChangeSetSummary)
{-# DEPRECATED cssParentChangeSetId "Use generic-lens or generic-optics with 'parentChangeSetId' instead." #-}

-- | The name of the change set.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssChangeSetName :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cssChangeSetName = Lens.lens (changeSetName :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {changeSetName = a} :: ChangeSetSummary)
{-# DEPRECATED cssChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- /Note:/ Consider using 'executionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssExecutionStatus :: Lens.Lens' ChangeSetSummary (Lude.Maybe ExecutionStatus)
cssExecutionStatus = Lens.lens (executionStatus :: ChangeSetSummary -> Lude.Maybe ExecutionStatus) (\s a -> s {executionStatus = a} :: ChangeSetSummary)
{-# DEPRECATED cssExecutionStatus "Use generic-lens or generic-optics with 'executionStatus' instead." #-}

-- | The ID of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssChangeSetId :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cssChangeSetId = Lens.lens (changeSetId :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {changeSetId = a} :: ChangeSetSummary)
{-# DEPRECATED cssChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | Specifies the current setting of @IncludeNestedStacks@ for the change set.
--
-- /Note:/ Consider using 'includeNestedStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssIncludeNestedStacks :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Bool)
cssIncludeNestedStacks = Lens.lens (includeNestedStacks :: ChangeSetSummary -> Lude.Maybe Lude.Bool) (\s a -> s {includeNestedStacks = a} :: ChangeSetSummary)
{-# DEPRECATED cssIncludeNestedStacks "Use generic-lens or generic-optics with 'includeNestedStacks' instead." #-}

-- | The root change set ID.
--
-- /Note:/ Consider using 'rootChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssRootChangeSetId :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cssRootChangeSetId = Lens.lens (rootChangeSetId :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {rootChangeSetId = a} :: ChangeSetSummary)
{-# DEPRECATED cssRootChangeSetId "Use generic-lens or generic-optics with 'rootChangeSetId' instead." #-}

-- | A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStatusReason :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cssStatusReason = Lens.lens (statusReason :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: ChangeSetSummary)
{-# DEPRECATED cssStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The ID of the stack with which the change set is associated.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStackId :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cssStackId = Lens.lens (stackId :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: ChangeSetSummary)
{-# DEPRECATED cssStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | Descriptive information about the change set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDescription :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cssDescription = Lens.lens (description :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ChangeSetSummary)
{-# DEPRECATED cssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the stack with which the change set is associated.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStackName :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cssStackName = Lens.lens (stackName :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: ChangeSetSummary)
{-# DEPRECATED cssStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.FromXML ChangeSetSummary where
  parseXML x =
    ChangeSetSummary'
      Lude.<$> (x Lude..@? "CreationTime")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "ParentChangeSetId")
      Lude.<*> (x Lude..@? "ChangeSetName")
      Lude.<*> (x Lude..@? "ExecutionStatus")
      Lude.<*> (x Lude..@? "ChangeSetId")
      Lude.<*> (x Lude..@? "IncludeNestedStacks")
      Lude.<*> (x Lude..@? "RootChangeSetId")
      Lude.<*> (x Lude..@? "StatusReason")
      Lude.<*> (x Lude..@? "StackId")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> (x Lude..@? "StackName")
