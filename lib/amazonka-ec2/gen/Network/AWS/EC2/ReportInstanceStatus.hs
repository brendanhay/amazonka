{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReportInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits feedback about the status of an instance. The instance must be in the @running@ state. If your experience with the instance differs from the instance status returned by 'DescribeInstanceStatus' , use 'ReportInstanceStatus' to report your experience with the instance. Amazon EC2 collects this information to improve the accuracy of status checks.
--
-- Use of this action does not change the value returned by 'DescribeInstanceStatus' .
module Network.AWS.EC2.ReportInstanceStatus
  ( -- * Creating a request
    ReportInstanceStatus (..),
    mkReportInstanceStatus,

    -- ** Request lenses
    rissStartTime,
    rissEndTime,
    rissDescription,
    rissDryRun,
    rissInstances,
    rissReasonCodes,
    rissStatus,

    -- * Destructuring the response
    ReportInstanceStatusResponse (..),
    mkReportInstanceStatusResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReportInstanceStatus' smart constructor.
data ReportInstanceStatus = ReportInstanceStatus'
  { startTime ::
      Lude.Maybe Lude.ISO8601,
    endTime :: Lude.Maybe Lude.ISO8601,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    instances :: [Lude.Text],
    reasonCodes :: [ReportInstanceReasonCodes],
    status :: ReportStatusType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportInstanceStatus' with the minimum fields required to make a request.
--
-- * 'description' - Descriptive text about the health state of your instance.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'endTime' - The time at which the reported instance health state ended.
-- * 'instances' - The instances.
-- * 'reasonCodes' - The reason codes that describe the health state of your instance.
--
--
--     * @instance-stuck-in-state@ : My instance is stuck in a state.
--
--
--     * @unresponsive@ : My instance is unresponsive.
--
--
--     * @not-accepting-credentials@ : My instance is not accepting my credentials.
--
--
--     * @password-not-available@ : A password is not available for my instance.
--
--
--     * @performance-network@ : My instance is experiencing performance problems that I believe are network related.
--
--
--     * @performance-instance-store@ : My instance is experiencing performance problems that I believe are related to the instance stores.
--
--
--     * @performance-ebs-volume@ : My instance is experiencing performance problems that I believe are related to an EBS volume.
--
--
--     * @performance-other@ : My instance is experiencing performance problems.
--
--
--     * @other@ : [explain using the description parameter]
--
--
-- * 'startTime' - The time at which the reported instance health state began.
-- * 'status' - The status of all instances listed.
mkReportInstanceStatus ::
  -- | 'status'
  ReportStatusType ->
  ReportInstanceStatus
mkReportInstanceStatus pStatus_ =
  ReportInstanceStatus'
    { startTime = Lude.Nothing,
      endTime = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      instances = Lude.mempty,
      reasonCodes = Lude.mempty,
      status = pStatus_
    }

-- | The time at which the reported instance health state began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissStartTime :: Lens.Lens' ReportInstanceStatus (Lude.Maybe Lude.ISO8601)
rissStartTime = Lens.lens (startTime :: ReportInstanceStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {startTime = a} :: ReportInstanceStatus)
{-# DEPRECATED rissStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The time at which the reported instance health state ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissEndTime :: Lens.Lens' ReportInstanceStatus (Lude.Maybe Lude.ISO8601)
rissEndTime = Lens.lens (endTime :: ReportInstanceStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {endTime = a} :: ReportInstanceStatus)
{-# DEPRECATED rissEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Descriptive text about the health state of your instance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissDescription :: Lens.Lens' ReportInstanceStatus (Lude.Maybe Lude.Text)
rissDescription = Lens.lens (description :: ReportInstanceStatus -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ReportInstanceStatus)
{-# DEPRECATED rissDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissDryRun :: Lens.Lens' ReportInstanceStatus (Lude.Maybe Lude.Bool)
rissDryRun = Lens.lens (dryRun :: ReportInstanceStatus -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ReportInstanceStatus)
{-# DEPRECATED rissDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissInstances :: Lens.Lens' ReportInstanceStatus [Lude.Text]
rissInstances = Lens.lens (instances :: ReportInstanceStatus -> [Lude.Text]) (\s a -> s {instances = a} :: ReportInstanceStatus)
{-# DEPRECATED rissInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The reason codes that describe the health state of your instance.
--
--
--     * @instance-stuck-in-state@ : My instance is stuck in a state.
--
--
--     * @unresponsive@ : My instance is unresponsive.
--
--
--     * @not-accepting-credentials@ : My instance is not accepting my credentials.
--
--
--     * @password-not-available@ : A password is not available for my instance.
--
--
--     * @performance-network@ : My instance is experiencing performance problems that I believe are network related.
--
--
--     * @performance-instance-store@ : My instance is experiencing performance problems that I believe are related to the instance stores.
--
--
--     * @performance-ebs-volume@ : My instance is experiencing performance problems that I believe are related to an EBS volume.
--
--
--     * @performance-other@ : My instance is experiencing performance problems.
--
--
--     * @other@ : [explain using the description parameter]
--
--
--
-- /Note:/ Consider using 'reasonCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissReasonCodes :: Lens.Lens' ReportInstanceStatus [ReportInstanceReasonCodes]
rissReasonCodes = Lens.lens (reasonCodes :: ReportInstanceStatus -> [ReportInstanceReasonCodes]) (\s a -> s {reasonCodes = a} :: ReportInstanceStatus)
{-# DEPRECATED rissReasonCodes "Use generic-lens or generic-optics with 'reasonCodes' instead." #-}

-- | The status of all instances listed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissStatus :: Lens.Lens' ReportInstanceStatus ReportStatusType
rissStatus = Lens.lens (status :: ReportInstanceStatus -> ReportStatusType) (\s a -> s {status = a} :: ReportInstanceStatus)
{-# DEPRECATED rissStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.AWSRequest ReportInstanceStatus where
  type Rs ReportInstanceStatus = ReportInstanceStatusResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ReportInstanceStatusResponse'

instance Lude.ToHeaders ReportInstanceStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReportInstanceStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery ReportInstanceStatus where
  toQuery ReportInstanceStatus' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ReportInstanceStatus" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "StartTime" Lude.=: startTime,
        "EndTime" Lude.=: endTime,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "InstanceId" instances,
        Lude.toQueryList "ReasonCode" reasonCodes,
        "Status" Lude.=: status
      ]

-- | /See:/ 'mkReportInstanceStatusResponse' smart constructor.
data ReportInstanceStatusResponse = ReportInstanceStatusResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportInstanceStatusResponse' with the minimum fields required to make a request.
mkReportInstanceStatusResponse ::
  ReportInstanceStatusResponse
mkReportInstanceStatusResponse = ReportInstanceStatusResponse'
