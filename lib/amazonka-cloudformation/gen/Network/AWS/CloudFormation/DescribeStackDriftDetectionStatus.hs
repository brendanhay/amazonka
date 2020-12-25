{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackDriftDetectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a stack drift detection operation. A stack drift detection operation detects whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. A stack is considered to have drifted if one or more of its resources have drifted. For more information on stack and resource drift, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- Use 'DetectStackDrift' to initiate a stack drift detection operation. @DetectStackDrift@ returns a @StackDriftDetectionId@ you can use to monitor the progress of the operation using @DescribeStackDriftDetectionStatus@ . Once the drift detection operation has completed, use 'DescribeStackResourceDrifts' to return drift information about the stack and its resources.
module Network.AWS.CloudFormation.DescribeStackDriftDetectionStatus
  ( -- * Creating a request
    DescribeStackDriftDetectionStatus (..),
    mkDescribeStackDriftDetectionStatus,

    -- ** Request lenses
    dsddsStackDriftDetectionId,

    -- * Destructuring the response
    DescribeStackDriftDetectionStatusResponse (..),
    mkDescribeStackDriftDetectionStatusResponse,

    -- ** Response lenses
    dsddsrrsStackId,
    dsddsrrsStackDriftDetectionId,
    dsddsrrsDetectionStatus,
    dsddsrrsTimestamp,
    dsddsrrsDetectionStatusReason,
    dsddsrrsDriftedStackResourceCount,
    dsddsrrsStackDriftStatus,
    dsddsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStackDriftDetectionStatus' smart constructor.
newtype DescribeStackDriftDetectionStatus = DescribeStackDriftDetectionStatus'
  { -- | The ID of the drift detection results of this operation.
    --
    -- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
    stackDriftDetectionId :: Types.StackDriftDetectionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackDriftDetectionStatus' value with any optional fields omitted.
mkDescribeStackDriftDetectionStatus ::
  -- | 'stackDriftDetectionId'
  Types.StackDriftDetectionId ->
  DescribeStackDriftDetectionStatus
mkDescribeStackDriftDetectionStatus stackDriftDetectionId =
  DescribeStackDriftDetectionStatus' {stackDriftDetectionId}

-- | The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
--
-- /Note:/ Consider using 'stackDriftDetectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsStackDriftDetectionId :: Lens.Lens' DescribeStackDriftDetectionStatus Types.StackDriftDetectionId
dsddsStackDriftDetectionId = Lens.field @"stackDriftDetectionId"
{-# DEPRECATED dsddsStackDriftDetectionId "Use generic-lens or generic-optics with 'stackDriftDetectionId' instead." #-}

instance Core.AWSRequest DescribeStackDriftDetectionStatus where
  type
    Rs DescribeStackDriftDetectionStatus =
      DescribeStackDriftDetectionStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeStackDriftDetectionStatus")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackDriftDetectionId" stackDriftDetectionId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeStackDriftDetectionStatusResult"
      ( \s h x ->
          DescribeStackDriftDetectionStatusResponse'
            Core.<$> (x Core..@ "StackId")
            Core.<*> (x Core..@ "StackDriftDetectionId")
            Core.<*> (x Core..@ "DetectionStatus")
            Core.<*> (x Core..@ "Timestamp")
            Core.<*> (x Core..@? "DetectionStatusReason")
            Core.<*> (x Core..@? "DriftedStackResourceCount")
            Core.<*> (x Core..@? "StackDriftStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStackDriftDetectionStatusResponse' smart constructor.
data DescribeStackDriftDetectionStatusResponse = DescribeStackDriftDetectionStatusResponse'
  { -- | The ID of the stack.
    stackId :: Types.StackId,
    -- | The ID of the drift detection results of this operation.
    --
    -- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of reports AWS CloudFormation retains for any given stack, and for how long, may vary.
    stackDriftDetectionId :: Types.StackDriftDetectionId,
    -- | The status of the stack drift detection operation.
    --
    --
    --     * @DETECTION_COMPLETE@ : The stack drift detection operation has successfully completed for all resources in the stack that support drift detection. (Resources that do not currently support stack detection remain unchecked.)
    -- If you specified logical resource IDs for AWS CloudFormation to use as a filter for the stack drift detection operation, only the resources with those logical IDs are checked for drift.
    --
    --
    --     * @DETECTION_FAILED@ : The stack drift detection operation has failed for at least one resource in the stack. Results will be available for resources on which AWS CloudFormation successfully completed drift detection.
    --
    --
    --     * @DETECTION_IN_PROGRESS@ : The stack drift detection operation is currently in progress.
    detectionStatus :: Types.StackDriftDetectionStatus,
    -- | Time at which the stack drift detection operation was initiated.
    timestamp :: Core.UTCTime,
    -- | The reason the stack drift detection operation has its current status.
    detectionStatusReason :: Core.Maybe Types.StackDriftDetectionStatusReason,
    -- | Total number of stack resources that have drifted. This is NULL until the drift detection operation reaches a status of @DETECTION_COMPLETE@ . This value will be 0 for stacks whose drift status is @IN_SYNC@ .
    driftedStackResourceCount :: Core.Maybe Core.Int,
    -- | Status of the stack's actual configuration compared to its expected configuration.
    --
    --
    --     * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.
    --
    --
    --     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.
    --
    --
    --     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.
    --
    --
    --     * @UNKNOWN@ : This value is reserved for future use.
    stackDriftStatus :: Core.Maybe Types.StackDriftStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStackDriftDetectionStatusResponse' value with any optional fields omitted.
mkDescribeStackDriftDetectionStatusResponse ::
  -- | 'stackId'
  Types.StackId ->
  -- | 'stackDriftDetectionId'
  Types.StackDriftDetectionId ->
  -- | 'detectionStatus'
  Types.StackDriftDetectionStatus ->
  -- | 'timestamp'
  Core.UTCTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeStackDriftDetectionStatusResponse
mkDescribeStackDriftDetectionStatusResponse
  stackId
  stackDriftDetectionId
  detectionStatus
  timestamp
  responseStatus =
    DescribeStackDriftDetectionStatusResponse'
      { stackId,
        stackDriftDetectionId,
        detectionStatus,
        timestamp,
        detectionStatusReason = Core.Nothing,
        driftedStackResourceCount = Core.Nothing,
        stackDriftStatus = Core.Nothing,
        responseStatus
      }

-- | The ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrrsStackId :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Types.StackId
dsddsrrsStackId = Lens.field @"stackId"
{-# DEPRECATED dsddsrrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of reports AWS CloudFormation retains for any given stack, and for how long, may vary.
--
-- /Note:/ Consider using 'stackDriftDetectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrrsStackDriftDetectionId :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Types.StackDriftDetectionId
dsddsrrsStackDriftDetectionId = Lens.field @"stackDriftDetectionId"
{-# DEPRECATED dsddsrrsStackDriftDetectionId "Use generic-lens or generic-optics with 'stackDriftDetectionId' instead." #-}

-- | The status of the stack drift detection operation.
--
--
--     * @DETECTION_COMPLETE@ : The stack drift detection operation has successfully completed for all resources in the stack that support drift detection. (Resources that do not currently support stack detection remain unchecked.)
-- If you specified logical resource IDs for AWS CloudFormation to use as a filter for the stack drift detection operation, only the resources with those logical IDs are checked for drift.
--
--
--     * @DETECTION_FAILED@ : The stack drift detection operation has failed for at least one resource in the stack. Results will be available for resources on which AWS CloudFormation successfully completed drift detection.
--
--
--     * @DETECTION_IN_PROGRESS@ : The stack drift detection operation is currently in progress.
--
--
--
-- /Note:/ Consider using 'detectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrrsDetectionStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Types.StackDriftDetectionStatus
dsddsrrsDetectionStatus = Lens.field @"detectionStatus"
{-# DEPRECATED dsddsrrsDetectionStatus "Use generic-lens or generic-optics with 'detectionStatus' instead." #-}

-- | Time at which the stack drift detection operation was initiated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrrsTimestamp :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Core.UTCTime
dsddsrrsTimestamp = Lens.field @"timestamp"
{-# DEPRECATED dsddsrrsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The reason the stack drift detection operation has its current status.
--
-- /Note:/ Consider using 'detectionStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrrsDetectionStatusReason :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Core.Maybe Types.StackDriftDetectionStatusReason)
dsddsrrsDetectionStatusReason = Lens.field @"detectionStatusReason"
{-# DEPRECATED dsddsrrsDetectionStatusReason "Use generic-lens or generic-optics with 'detectionStatusReason' instead." #-}

-- | Total number of stack resources that have drifted. This is NULL until the drift detection operation reaches a status of @DETECTION_COMPLETE@ . This value will be 0 for stacks whose drift status is @IN_SYNC@ .
--
-- /Note:/ Consider using 'driftedStackResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrrsDriftedStackResourceCount :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Core.Maybe Core.Int)
dsddsrrsDriftedStackResourceCount = Lens.field @"driftedStackResourceCount"
{-# DEPRECATED dsddsrrsDriftedStackResourceCount "Use generic-lens or generic-optics with 'driftedStackResourceCount' instead." #-}

-- | Status of the stack's actual configuration compared to its expected configuration.
--
--
--     * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.
--
--
--     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.
--
--
--     * @UNKNOWN@ : This value is reserved for future use.
--
--
--
-- /Note:/ Consider using 'stackDriftStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrrsStackDriftStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Core.Maybe Types.StackDriftStatus)
dsddsrrsStackDriftStatus = Lens.field @"stackDriftStatus"
{-# DEPRECATED dsddsrrsStackDriftStatus "Use generic-lens or generic-optics with 'stackDriftStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrrsResponseStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Core.Int
dsddsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsddsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
