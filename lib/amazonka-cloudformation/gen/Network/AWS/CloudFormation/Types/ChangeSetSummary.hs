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
    cCreationTime,
    cStatus,
    cParentChangeSetId,
    cChangeSetName,
    cExecutionStatus,
    cChangeSetId,
    cIncludeNestedStacks,
    cRootChangeSetId,
    cStatusReason,
    cStackId,
    cDescription,
    cStackName,
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
  { creationTime ::
      Lude.Maybe Lude.ISO8601,
    status :: Lude.Maybe ChangeSetStatus,
    parentChangeSetId :: Lude.Maybe Lude.Text,
    changeSetName :: Lude.Maybe Lude.Text,
    executionStatus :: Lude.Maybe ExecutionStatus,
    changeSetId :: Lude.Maybe Lude.Text,
    includeNestedStacks :: Lude.Maybe Lude.Bool,
    rootChangeSetId :: Lude.Maybe Lude.Text,
    statusReason :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeSetSummary' with the minimum fields required to make a request.
--
-- * 'changeSetId' - The ID of the change set.
-- * 'changeSetName' - The name of the change set.
-- * 'creationTime' - The start time when the change set was created, in UTC.
-- * 'description' - Descriptive information about the change set.
-- * 'executionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
-- * 'includeNestedStacks' - Specifies the current setting of @IncludeNestedStacks@ for the change set.
-- * 'parentChangeSetId' - The parent change set ID.
-- * 'rootChangeSetId' - The root change set ID.
-- * 'stackId' - The ID of the stack with which the change set is associated.
-- * 'stackName' - The name of the stack with which the change set is associated.
-- * 'status' - The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
-- * 'statusReason' - A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
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
cCreationTime :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.ISO8601)
cCreationTime = Lens.lens (creationTime :: ChangeSetSummary -> Lude.Maybe Lude.ISO8601) (\s a -> s {creationTime = a} :: ChangeSetSummary)
{-# DEPRECATED cCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' ChangeSetSummary (Lude.Maybe ChangeSetStatus)
cStatus = Lens.lens (status :: ChangeSetSummary -> Lude.Maybe ChangeSetStatus) (\s a -> s {status = a} :: ChangeSetSummary)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The parent change set ID.
--
-- /Note:/ Consider using 'parentChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParentChangeSetId :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cParentChangeSetId = Lens.lens (parentChangeSetId :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {parentChangeSetId = a} :: ChangeSetSummary)
{-# DEPRECATED cParentChangeSetId "Use generic-lens or generic-optics with 'parentChangeSetId' instead." #-}

-- | The name of the change set.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cChangeSetName :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cChangeSetName = Lens.lens (changeSetName :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {changeSetName = a} :: ChangeSetSummary)
{-# DEPRECATED cChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- /Note:/ Consider using 'executionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExecutionStatus :: Lens.Lens' ChangeSetSummary (Lude.Maybe ExecutionStatus)
cExecutionStatus = Lens.lens (executionStatus :: ChangeSetSummary -> Lude.Maybe ExecutionStatus) (\s a -> s {executionStatus = a} :: ChangeSetSummary)
{-# DEPRECATED cExecutionStatus "Use generic-lens or generic-optics with 'executionStatus' instead." #-}

-- | The ID of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cChangeSetId :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cChangeSetId = Lens.lens (changeSetId :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {changeSetId = a} :: ChangeSetSummary)
{-# DEPRECATED cChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | Specifies the current setting of @IncludeNestedStacks@ for the change set.
--
-- /Note:/ Consider using 'includeNestedStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIncludeNestedStacks :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Bool)
cIncludeNestedStacks = Lens.lens (includeNestedStacks :: ChangeSetSummary -> Lude.Maybe Lude.Bool) (\s a -> s {includeNestedStacks = a} :: ChangeSetSummary)
{-# DEPRECATED cIncludeNestedStacks "Use generic-lens or generic-optics with 'includeNestedStacks' instead." #-}

-- | The root change set ID.
--
-- /Note:/ Consider using 'rootChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRootChangeSetId :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cRootChangeSetId = Lens.lens (rootChangeSetId :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {rootChangeSetId = a} :: ChangeSetSummary)
{-# DEPRECATED cRootChangeSetId "Use generic-lens or generic-optics with 'rootChangeSetId' instead." #-}

-- | A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatusReason :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cStatusReason = Lens.lens (statusReason :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: ChangeSetSummary)
{-# DEPRECATED cStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The ID of the stack with which the change set is associated.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStackId :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cStackId = Lens.lens (stackId :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: ChangeSetSummary)
{-# DEPRECATED cStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | Descriptive information about the change set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cDescription = Lens.lens (description :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ChangeSetSummary)
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the stack with which the change set is associated.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStackName :: Lens.Lens' ChangeSetSummary (Lude.Maybe Lude.Text)
cStackName = Lens.lens (stackName :: ChangeSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: ChangeSetSummary)
{-# DEPRECATED cStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

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
