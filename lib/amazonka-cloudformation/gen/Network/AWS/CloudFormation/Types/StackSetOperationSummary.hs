{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetOperationSummary
  ( StackSetOperationSummary (..)
  -- * Smart constructor
  , mkStackSetOperationSummary
  -- * Lenses
  , ssosAction
  , ssosCreationTimestamp
  , ssosEndTimestamp
  , ssosOperationId
  , ssosStatus
  ) where

import qualified Network.AWS.CloudFormation.Types.OperationId as Types
import qualified Network.AWS.CloudFormation.Types.StackSetOperationAction as Types
import qualified Network.AWS.CloudFormation.Types.StackSetOperationStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structures that contain summary information about the specified operation.
--
-- /See:/ 'mkStackSetOperationSummary' smart constructor.
data StackSetOperationSummary = StackSetOperationSummary'
  { action :: Core.Maybe Types.StackSetOperationAction
    -- ^ The type of operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack instances that are associated with the specified stack set. Update operations affect both the stack set itself as well as /all/ associated stack set instances.
  , creationTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
  , endTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
  , operationId :: Core.Maybe Types.OperationId
    -- ^ The unique ID of the stack set operation.
  , status :: Core.Maybe Types.StackSetOperationStatus
    -- ^ The overall status of the operation.
--
--
--     * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each Region during stack create and update operations. If the number of failed stacks within a Region exceeds the failure tolerance, the status of the operation in the Region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining Regions.
--
--
--     * @QUEUED@ : [@Service-managed@ permissions] For automatic deployments that require a sequence of operations, the operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.
--
--
--     * @RUNNING@ : The operation is currently being performed.
--
--
--     * @STOPPED@ : The user has cancelled the operation.
--
--
--     * @STOPPING@ : The operation is in the process of stopping, at user request. 
--
--
--     * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StackSetOperationSummary' value with any optional fields omitted.
mkStackSetOperationSummary
    :: StackSetOperationSummary
mkStackSetOperationSummary
  = StackSetOperationSummary'{action = Core.Nothing,
                              creationTimestamp = Core.Nothing, endTimestamp = Core.Nothing,
                              operationId = Core.Nothing, status = Core.Nothing}

-- | The type of operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack instances that are associated with the specified stack set. Update operations affect both the stack set itself as well as /all/ associated stack set instances.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosAction :: Lens.Lens' StackSetOperationSummary (Core.Maybe Types.StackSetOperationAction)
ssosAction = Lens.field @"action"
{-# INLINEABLE ssosAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosCreationTimestamp :: Lens.Lens' StackSetOperationSummary (Core.Maybe Core.UTCTime)
ssosCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE ssosCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosEndTimestamp :: Lens.Lens' StackSetOperationSummary (Core.Maybe Core.UTCTime)
ssosEndTimestamp = Lens.field @"endTimestamp"
{-# INLINEABLE ssosEndTimestamp #-}
{-# DEPRECATED endTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead"  #-}

-- | The unique ID of the stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosOperationId :: Lens.Lens' StackSetOperationSummary (Core.Maybe Types.OperationId)
ssosOperationId = Lens.field @"operationId"
{-# INLINEABLE ssosOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The overall status of the operation.
--
--
--     * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each Region during stack create and update operations. If the number of failed stacks within a Region exceeds the failure tolerance, the status of the operation in the Region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining Regions.
--
--
--     * @QUEUED@ : [@Service-managed@ permissions] For automatic deployments that require a sequence of operations, the operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.
--
--
--     * @RUNNING@ : The operation is currently being performed.
--
--
--     * @STOPPED@ : The user has cancelled the operation.
--
--
--     * @STOPPING@ : The operation is in the process of stopping, at user request. 
--
--
--     * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosStatus :: Lens.Lens' StackSetOperationSummary (Core.Maybe Types.StackSetOperationStatus)
ssosStatus = Lens.field @"status"
{-# INLINEABLE ssosStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML StackSetOperationSummary where
        parseXML x
          = StackSetOperationSummary' Core.<$>
              (x Core..@? "Action") Core.<*> x Core..@? "CreationTimestamp"
                Core.<*> x Core..@? "EndTimestamp"
                Core.<*> x Core..@? "OperationId"
                Core.<*> x Core..@? "Status"
