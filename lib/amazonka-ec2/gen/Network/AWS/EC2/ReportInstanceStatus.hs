{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    rissStatus,
    rissReasonCodes,
    rissStartTime,
    rissEndTime,
    rissInstances,
    rissDescription,
    rissDryRun,

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
  { -- | The status of all instances listed.
    status :: ReportStatusType,
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
    reasonCodes :: [ReportInstanceReasonCodes],
    -- | The time at which the reported instance health state began.
    startTime :: Lude.Maybe Lude.DateTime,
    -- | The time at which the reported instance health state ended.
    endTime :: Lude.Maybe Lude.DateTime,
    -- | The instances.
    instances :: [Lude.Text],
    -- | Descriptive text about the health state of your instance.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportInstanceStatus' with the minimum fields required to make a request.
--
-- * 'status' - The status of all instances listed.
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
-- * 'endTime' - The time at which the reported instance health state ended.
-- * 'instances' - The instances.
-- * 'description' - Descriptive text about the health state of your instance.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkReportInstanceStatus ::
  -- | 'status'
  ReportStatusType ->
  ReportInstanceStatus
mkReportInstanceStatus pStatus_ =
  ReportInstanceStatus'
    { status = pStatus_,
      reasonCodes = Lude.mempty,
      startTime = Lude.Nothing,
      endTime = Lude.Nothing,
      instances = Lude.mempty,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The status of all instances listed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissStatus :: Lens.Lens' ReportInstanceStatus ReportStatusType
rissStatus = Lens.lens (status :: ReportInstanceStatus -> ReportStatusType) (\s a -> s {status = a} :: ReportInstanceStatus)
{-# DEPRECATED rissStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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

-- | The time at which the reported instance health state began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissStartTime :: Lens.Lens' ReportInstanceStatus (Lude.Maybe Lude.DateTime)
rissStartTime = Lens.lens (startTime :: ReportInstanceStatus -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: ReportInstanceStatus)
{-# DEPRECATED rissStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The time at which the reported instance health state ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissEndTime :: Lens.Lens' ReportInstanceStatus (Lude.Maybe Lude.DateTime)
rissEndTime = Lens.lens (endTime :: ReportInstanceStatus -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: ReportInstanceStatus)
{-# DEPRECATED rissEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissInstances :: Lens.Lens' ReportInstanceStatus [Lude.Text]
rissInstances = Lens.lens (instances :: ReportInstanceStatus -> [Lude.Text]) (\s a -> s {instances = a} :: ReportInstanceStatus)
{-# DEPRECATED rissInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

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
        "Status" Lude.=: status,
        Lude.toQueryList "ReasonCode" reasonCodes,
        "StartTime" Lude.=: startTime,
        "EndTime" Lude.=: endTime,
        Lude.toQueryList "InstanceId" instances,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkReportInstanceStatusResponse' smart constructor.
data ReportInstanceStatusResponse = ReportInstanceStatusResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportInstanceStatusResponse' with the minimum fields required to make a request.
mkReportInstanceStatusResponse ::
  ReportInstanceStatusResponse
mkReportInstanceStatusResponse = ReportInstanceStatusResponse'
