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
    cssChangeSetId,
    cssChangeSetName,
    cssCreationTime,
    cssDescription,
    cssExecutionStatus,
    cssIncludeNestedStacks,
    cssParentChangeSetId,
    cssRootChangeSetId,
    cssStackId,
    cssStackName,
    cssStatus,
    cssStatusReason,
  )
where

import qualified Network.AWS.CloudFormation.Types.ChangeSetId as Types
import qualified Network.AWS.CloudFormation.Types.ChangeSetName as Types
import qualified Network.AWS.CloudFormation.Types.ChangeSetStatus as Types
import qualified Network.AWS.CloudFormation.Types.ChangeSetStatusReason as Types
import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.ExecutionStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackId as Types
import qualified Network.AWS.CloudFormation.Types.StackName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @ChangeSetSummary@ structure describes a change set, its status, and the stack with which it's associated.
--
-- /See:/ 'mkChangeSetSummary' smart constructor.
data ChangeSetSummary = ChangeSetSummary'
  { -- | The ID of the change set.
    changeSetId :: Core.Maybe Types.ChangeSetId,
    -- | The name of the change set.
    changeSetName :: Core.Maybe Types.ChangeSetName,
    -- | The start time when the change set was created, in UTC.
    creationTime :: Core.Maybe Core.UTCTime,
    -- | Descriptive information about the change set.
    description :: Core.Maybe Types.Description,
    -- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
    executionStatus :: Core.Maybe Types.ExecutionStatus,
    -- | Specifies the current setting of @IncludeNestedStacks@ for the change set.
    includeNestedStacks :: Core.Maybe Core.Bool,
    -- | The parent change set ID.
    parentChangeSetId :: Core.Maybe Types.ChangeSetId,
    -- | The root change set ID.
    rootChangeSetId :: Core.Maybe Types.ChangeSetId,
    -- | The ID of the stack with which the change set is associated.
    stackId :: Core.Maybe Types.StackId,
    -- | The name of the stack with which the change set is associated.
    stackName :: Core.Maybe Types.StackName,
    -- | The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
    status :: Core.Maybe Types.ChangeSetStatus,
    -- | A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
    statusReason :: Core.Maybe Types.ChangeSetStatusReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ChangeSetSummary' value with any optional fields omitted.
mkChangeSetSummary ::
  ChangeSetSummary
mkChangeSetSummary =
  ChangeSetSummary'
    { changeSetId = Core.Nothing,
      changeSetName = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      executionStatus = Core.Nothing,
      includeNestedStacks = Core.Nothing,
      parentChangeSetId = Core.Nothing,
      rootChangeSetId = Core.Nothing,
      stackId = Core.Nothing,
      stackName = Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The ID of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssChangeSetId :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.ChangeSetId)
cssChangeSetId = Lens.field @"changeSetId"
{-# DEPRECATED cssChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | The name of the change set.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssChangeSetName :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.ChangeSetName)
cssChangeSetName = Lens.field @"changeSetName"
{-# DEPRECATED cssChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | The start time when the change set was created, in UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssCreationTime :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.UTCTime)
cssCreationTime = Lens.field @"creationTime"
{-# DEPRECATED cssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Descriptive information about the change set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDescription :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.Description)
cssDescription = Lens.field @"description"
{-# DEPRECATED cssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- /Note:/ Consider using 'executionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssExecutionStatus :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.ExecutionStatus)
cssExecutionStatus = Lens.field @"executionStatus"
{-# DEPRECATED cssExecutionStatus "Use generic-lens or generic-optics with 'executionStatus' instead." #-}

-- | Specifies the current setting of @IncludeNestedStacks@ for the change set.
--
-- /Note:/ Consider using 'includeNestedStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssIncludeNestedStacks :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Bool)
cssIncludeNestedStacks = Lens.field @"includeNestedStacks"
{-# DEPRECATED cssIncludeNestedStacks "Use generic-lens or generic-optics with 'includeNestedStacks' instead." #-}

-- | The parent change set ID.
--
-- /Note:/ Consider using 'parentChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssParentChangeSetId :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.ChangeSetId)
cssParentChangeSetId = Lens.field @"parentChangeSetId"
{-# DEPRECATED cssParentChangeSetId "Use generic-lens or generic-optics with 'parentChangeSetId' instead." #-}

-- | The root change set ID.
--
-- /Note:/ Consider using 'rootChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssRootChangeSetId :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.ChangeSetId)
cssRootChangeSetId = Lens.field @"rootChangeSetId"
{-# DEPRECATED cssRootChangeSetId "Use generic-lens or generic-optics with 'rootChangeSetId' instead." #-}

-- | The ID of the stack with which the change set is associated.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStackId :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.StackId)
cssStackId = Lens.field @"stackId"
{-# DEPRECATED cssStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The name of the stack with which the change set is associated.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStackName :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.StackName)
cssStackName = Lens.field @"stackName"
{-# DEPRECATED cssStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStatus :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.ChangeSetStatus)
cssStatus = Lens.field @"status"
{-# DEPRECATED cssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStatusReason :: Lens.Lens' ChangeSetSummary (Core.Maybe Types.ChangeSetStatusReason)
cssStatusReason = Lens.field @"statusReason"
{-# DEPRECATED cssStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Core.FromXML ChangeSetSummary where
  parseXML x =
    ChangeSetSummary'
      Core.<$> (x Core..@? "ChangeSetId")
      Core.<*> (x Core..@? "ChangeSetName")
      Core.<*> (x Core..@? "CreationTime")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ExecutionStatus")
      Core.<*> (x Core..@? "IncludeNestedStacks")
      Core.<*> (x Core..@? "ParentChangeSetId")
      Core.<*> (x Core..@? "RootChangeSetId")
      Core.<*> (x Core..@? "StackId")
      Core.<*> (x Core..@? "StackName")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "StatusReason")
