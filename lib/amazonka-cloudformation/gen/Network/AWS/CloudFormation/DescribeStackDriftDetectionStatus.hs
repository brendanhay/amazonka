{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dsddsrsStackDriftStatus,
    dsddsrsDriftedStackResourceCount,
    dsddsrsDetectionStatusReason,
    dsddsrsResponseStatus,
    dsddsrsStackId,
    dsddsrsStackDriftDetectionId,
    dsddsrsDetectionStatus,
    dsddsrsTimestamp,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStackDriftDetectionStatus' smart constructor.
newtype DescribeStackDriftDetectionStatus = DescribeStackDriftDetectionStatus'
  { stackDriftDetectionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackDriftDetectionStatus' with the minimum fields required to make a request.
--
-- * 'stackDriftDetectionId' - The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
mkDescribeStackDriftDetectionStatus ::
  -- | 'stackDriftDetectionId'
  Lude.Text ->
  DescribeStackDriftDetectionStatus
mkDescribeStackDriftDetectionStatus pStackDriftDetectionId_ =
  DescribeStackDriftDetectionStatus'
    { stackDriftDetectionId =
        pStackDriftDetectionId_
    }

-- | The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
--
-- /Note:/ Consider using 'stackDriftDetectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsStackDriftDetectionId :: Lens.Lens' DescribeStackDriftDetectionStatus Lude.Text
dsddsStackDriftDetectionId = Lens.lens (stackDriftDetectionId :: DescribeStackDriftDetectionStatus -> Lude.Text) (\s a -> s {stackDriftDetectionId = a} :: DescribeStackDriftDetectionStatus)
{-# DEPRECATED dsddsStackDriftDetectionId "Use generic-lens or generic-optics with 'stackDriftDetectionId' instead." #-}

instance Lude.AWSRequest DescribeStackDriftDetectionStatus where
  type
    Rs DescribeStackDriftDetectionStatus =
      DescribeStackDriftDetectionStatusResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeStackDriftDetectionStatusResult"
      ( \s h x ->
          DescribeStackDriftDetectionStatusResponse'
            Lude.<$> (x Lude..@? "StackDriftStatus")
            Lude.<*> (x Lude..@? "DriftedStackResourceCount")
            Lude.<*> (x Lude..@? "DetectionStatusReason")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "StackId")
            Lude.<*> (x Lude..@ "StackDriftDetectionId")
            Lude.<*> (x Lude..@ "DetectionStatus")
            Lude.<*> (x Lude..@ "Timestamp")
      )

instance Lude.ToHeaders DescribeStackDriftDetectionStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStackDriftDetectionStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackDriftDetectionStatus where
  toQuery DescribeStackDriftDetectionStatus' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeStackDriftDetectionStatus" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "StackDriftDetectionId" Lude.=: stackDriftDetectionId
      ]

-- | /See:/ 'mkDescribeStackDriftDetectionStatusResponse' smart constructor.
data DescribeStackDriftDetectionStatusResponse = DescribeStackDriftDetectionStatusResponse'
  { stackDriftStatus ::
      Lude.Maybe
        StackDriftStatus,
    driftedStackResourceCount ::
      Lude.Maybe
        Lude.Int,
    detectionStatusReason ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    stackId ::
      Lude.Text,
    stackDriftDetectionId ::
      Lude.Text,
    detectionStatus ::
      StackDriftDetectionStatus,
    timestamp ::
      Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackDriftDetectionStatusResponse' with the minimum fields required to make a request.
--
-- * 'detectionStatus' - The status of the stack drift detection operation.
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
-- * 'detectionStatusReason' - The reason the stack drift detection operation has its current status.
-- * 'driftedStackResourceCount' - Total number of stack resources that have drifted. This is NULL until the drift detection operation reaches a status of @DETECTION_COMPLETE@ . This value will be 0 for stacks whose drift status is @IN_SYNC@ .
-- * 'responseStatus' - The response status code.
-- * 'stackDriftDetectionId' - The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of reports AWS CloudFormation retains for any given stack, and for how long, may vary.
-- * 'stackDriftStatus' - Status of the stack's actual configuration compared to its expected configuration.
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
-- * 'stackId' - The ID of the stack.
-- * 'timestamp' - Time at which the stack drift detection operation was initiated.
mkDescribeStackDriftDetectionStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'stackId'
  Lude.Text ->
  -- | 'stackDriftDetectionId'
  Lude.Text ->
  -- | 'detectionStatus'
  StackDriftDetectionStatus ->
  -- | 'timestamp'
  Lude.DateTime ->
  DescribeStackDriftDetectionStatusResponse
mkDescribeStackDriftDetectionStatusResponse
  pResponseStatus_
  pStackId_
  pStackDriftDetectionId_
  pDetectionStatus_
  pTimestamp_ =
    DescribeStackDriftDetectionStatusResponse'
      { stackDriftStatus =
          Lude.Nothing,
        driftedStackResourceCount = Lude.Nothing,
        detectionStatusReason = Lude.Nothing,
        responseStatus = pResponseStatus_,
        stackId = pStackId_,
        stackDriftDetectionId = pStackDriftDetectionId_,
        detectionStatus = pDetectionStatus_,
        timestamp = pTimestamp_
      }

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
dsddsrsStackDriftStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Lude.Maybe StackDriftStatus)
dsddsrsStackDriftStatus = Lens.lens (stackDriftStatus :: DescribeStackDriftDetectionStatusResponse -> Lude.Maybe StackDriftStatus) (\s a -> s {stackDriftStatus = a} :: DescribeStackDriftDetectionStatusResponse)
{-# DEPRECATED dsddsrsStackDriftStatus "Use generic-lens or generic-optics with 'stackDriftStatus' instead." #-}

-- | Total number of stack resources that have drifted. This is NULL until the drift detection operation reaches a status of @DETECTION_COMPLETE@ . This value will be 0 for stacks whose drift status is @IN_SYNC@ .
--
-- /Note:/ Consider using 'driftedStackResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrsDriftedStackResourceCount :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Lude.Maybe Lude.Int)
dsddsrsDriftedStackResourceCount = Lens.lens (driftedStackResourceCount :: DescribeStackDriftDetectionStatusResponse -> Lude.Maybe Lude.Int) (\s a -> s {driftedStackResourceCount = a} :: DescribeStackDriftDetectionStatusResponse)
{-# DEPRECATED dsddsrsDriftedStackResourceCount "Use generic-lens or generic-optics with 'driftedStackResourceCount' instead." #-}

-- | The reason the stack drift detection operation has its current status.
--
-- /Note:/ Consider using 'detectionStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrsDetectionStatusReason :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Lude.Maybe Lude.Text)
dsddsrsDetectionStatusReason = Lens.lens (detectionStatusReason :: DescribeStackDriftDetectionStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {detectionStatusReason = a} :: DescribeStackDriftDetectionStatusResponse)
{-# DEPRECATED dsddsrsDetectionStatusReason "Use generic-lens or generic-optics with 'detectionStatusReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrsResponseStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Lude.Int
dsddsrsResponseStatus = Lens.lens (responseStatus :: DescribeStackDriftDetectionStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackDriftDetectionStatusResponse)
{-# DEPRECATED dsddsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrsStackId :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Lude.Text
dsddsrsStackId = Lens.lens (stackId :: DescribeStackDriftDetectionStatusResponse -> Lude.Text) (\s a -> s {stackId = a} :: DescribeStackDriftDetectionStatusResponse)
{-# DEPRECATED dsddsrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of reports AWS CloudFormation retains for any given stack, and for how long, may vary.
--
-- /Note:/ Consider using 'stackDriftDetectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrsStackDriftDetectionId :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Lude.Text
dsddsrsStackDriftDetectionId = Lens.lens (stackDriftDetectionId :: DescribeStackDriftDetectionStatusResponse -> Lude.Text) (\s a -> s {stackDriftDetectionId = a} :: DescribeStackDriftDetectionStatusResponse)
{-# DEPRECATED dsddsrsStackDriftDetectionId "Use generic-lens or generic-optics with 'stackDriftDetectionId' instead." #-}

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
dsddsrsDetectionStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse StackDriftDetectionStatus
dsddsrsDetectionStatus = Lens.lens (detectionStatus :: DescribeStackDriftDetectionStatusResponse -> StackDriftDetectionStatus) (\s a -> s {detectionStatus = a} :: DescribeStackDriftDetectionStatusResponse)
{-# DEPRECATED dsddsrsDetectionStatus "Use generic-lens or generic-optics with 'detectionStatus' instead." #-}

-- | Time at which the stack drift detection operation was initiated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsddsrsTimestamp :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Lude.DateTime
dsddsrsTimestamp = Lens.lens (timestamp :: DescribeStackDriftDetectionStatusResponse -> Lude.DateTime) (\s a -> s {timestamp = a} :: DescribeStackDriftDetectionStatusResponse)
{-# DEPRECATED dsddsrsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}
