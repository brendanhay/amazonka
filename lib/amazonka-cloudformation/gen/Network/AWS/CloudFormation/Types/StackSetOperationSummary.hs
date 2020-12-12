{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationSummary
  ( StackSetOperationSummary (..),

    -- * Smart constructor
    mkStackSetOperationSummary,

    -- * Lenses
    ssosStatus,
    ssosAction,
    ssosEndTimestamp,
    ssosCreationTimestamp,
    ssosOperationId,
  )
where

import Network.AWS.CloudFormation.Types.StackSetOperationAction
import Network.AWS.CloudFormation.Types.StackSetOperationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The structures that contain summary information about the specified operation.
--
-- /See:/ 'mkStackSetOperationSummary' smart constructor.
data StackSetOperationSummary = StackSetOperationSummary'
  { status ::
      Lude.Maybe StackSetOperationStatus,
    action ::
      Lude.Maybe StackSetOperationAction,
    endTimestamp :: Lude.Maybe Lude.DateTime,
    creationTimestamp ::
      Lude.Maybe Lude.DateTime,
    operationId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSetOperationSummary' with the minimum fields required to make a request.
--
-- * 'action' - The type of operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack instances that are associated with the specified stack set. Update operations affect both the stack set itself as well as /all/ associated stack set instances.
-- * 'creationTimestamp' - The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
-- * 'endTimestamp' - The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
-- * 'operationId' - The unique ID of the stack set operation.
-- * 'status' - The overall status of the operation.
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
mkStackSetOperationSummary ::
  StackSetOperationSummary
mkStackSetOperationSummary =
  StackSetOperationSummary'
    { status = Lude.Nothing,
      action = Lude.Nothing,
      endTimestamp = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      operationId = Lude.Nothing
    }

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
ssosStatus :: Lens.Lens' StackSetOperationSummary (Lude.Maybe StackSetOperationStatus)
ssosStatus = Lens.lens (status :: StackSetOperationSummary -> Lude.Maybe StackSetOperationStatus) (\s a -> s {status = a} :: StackSetOperationSummary)
{-# DEPRECATED ssosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The type of operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack instances that are associated with the specified stack set. Update operations affect both the stack set itself as well as /all/ associated stack set instances.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosAction :: Lens.Lens' StackSetOperationSummary (Lude.Maybe StackSetOperationAction)
ssosAction = Lens.lens (action :: StackSetOperationSummary -> Lude.Maybe StackSetOperationAction) (\s a -> s {action = a} :: StackSetOperationSummary)
{-# DEPRECATED ssosAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosEndTimestamp :: Lens.Lens' StackSetOperationSummary (Lude.Maybe Lude.DateTime)
ssosEndTimestamp = Lens.lens (endTimestamp :: StackSetOperationSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {endTimestamp = a} :: StackSetOperationSummary)
{-# DEPRECATED ssosEndTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead." #-}

-- | The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosCreationTimestamp :: Lens.Lens' StackSetOperationSummary (Lude.Maybe Lude.DateTime)
ssosCreationTimestamp = Lens.lens (creationTimestamp :: StackSetOperationSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {creationTimestamp = a} :: StackSetOperationSummary)
{-# DEPRECATED ssosCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The unique ID of the stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssosOperationId :: Lens.Lens' StackSetOperationSummary (Lude.Maybe Lude.Text)
ssosOperationId = Lens.lens (operationId :: StackSetOperationSummary -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: StackSetOperationSummary)
{-# DEPRECATED ssosOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Lude.FromXML StackSetOperationSummary where
  parseXML x =
    StackSetOperationSummary'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "Action")
      Lude.<*> (x Lude..@? "EndTimestamp")
      Lude.<*> (x Lude..@? "CreationTimestamp")
      Lude.<*> (x Lude..@? "OperationId")
