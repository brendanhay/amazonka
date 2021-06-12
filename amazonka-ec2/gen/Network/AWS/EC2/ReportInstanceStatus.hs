{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReportInstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits feedback about the status of an instance. The instance must be
-- in the @running@ state. If your experience with the instance differs
-- from the instance status returned by DescribeInstanceStatus, use
-- ReportInstanceStatus to report your experience with the instance. Amazon
-- EC2 collects this information to improve the accuracy of status checks.
--
-- Use of this action does not change the value returned by
-- DescribeInstanceStatus.
module Network.AWS.EC2.ReportInstanceStatus
  ( -- * Creating a Request
    ReportInstanceStatus (..),
    newReportInstanceStatus,

    -- * Request Lenses
    reportInstanceStatus_dryRun,
    reportInstanceStatus_startTime,
    reportInstanceStatus_endTime,
    reportInstanceStatus_description,
    reportInstanceStatus_instances,
    reportInstanceStatus_reasonCodes,
    reportInstanceStatus_status,

    -- * Destructuring the Response
    ReportInstanceStatusResponse (..),
    newReportInstanceStatusResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReportInstanceStatus' smart constructor.
data ReportInstanceStatus = ReportInstanceStatus'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The time at which the reported instance health state began.
    startTime :: Core.Maybe Core.ISO8601,
    -- | The time at which the reported instance health state ended.
    endTime :: Core.Maybe Core.ISO8601,
    -- | Descriptive text about the health state of your instance.
    description :: Core.Maybe Core.Text,
    -- | The instances.
    instances :: [Core.Text],
    -- | The reason codes that describe the health state of your instance.
    --
    -- -   @instance-stuck-in-state@: My instance is stuck in a state.
    --
    -- -   @unresponsive@: My instance is unresponsive.
    --
    -- -   @not-accepting-credentials@: My instance is not accepting my
    --     credentials.
    --
    -- -   @password-not-available@: A password is not available for my
    --     instance.
    --
    -- -   @performance-network@: My instance is experiencing performance
    --     problems that I believe are network related.
    --
    -- -   @performance-instance-store@: My instance is experiencing
    --     performance problems that I believe are related to the instance
    --     stores.
    --
    -- -   @performance-ebs-volume@: My instance is experiencing performance
    --     problems that I believe are related to an EBS volume.
    --
    -- -   @performance-other@: My instance is experiencing performance
    --     problems.
    --
    -- -   @other@: [explain using the description parameter]
    reasonCodes :: [ReportInstanceReasonCodes],
    -- | The status of all instances listed.
    status :: ReportStatusType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReportInstanceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'reportInstanceStatus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'startTime', 'reportInstanceStatus_startTime' - The time at which the reported instance health state began.
--
-- 'endTime', 'reportInstanceStatus_endTime' - The time at which the reported instance health state ended.
--
-- 'description', 'reportInstanceStatus_description' - Descriptive text about the health state of your instance.
--
-- 'instances', 'reportInstanceStatus_instances' - The instances.
--
-- 'reasonCodes', 'reportInstanceStatus_reasonCodes' - The reason codes that describe the health state of your instance.
--
-- -   @instance-stuck-in-state@: My instance is stuck in a state.
--
-- -   @unresponsive@: My instance is unresponsive.
--
-- -   @not-accepting-credentials@: My instance is not accepting my
--     credentials.
--
-- -   @password-not-available@: A password is not available for my
--     instance.
--
-- -   @performance-network@: My instance is experiencing performance
--     problems that I believe are network related.
--
-- -   @performance-instance-store@: My instance is experiencing
--     performance problems that I believe are related to the instance
--     stores.
--
-- -   @performance-ebs-volume@: My instance is experiencing performance
--     problems that I believe are related to an EBS volume.
--
-- -   @performance-other@: My instance is experiencing performance
--     problems.
--
-- -   @other@: [explain using the description parameter]
--
-- 'status', 'reportInstanceStatus_status' - The status of all instances listed.
newReportInstanceStatus ::
  -- | 'status'
  ReportStatusType ->
  ReportInstanceStatus
newReportInstanceStatus pStatus_ =
  ReportInstanceStatus'
    { dryRun = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      description = Core.Nothing,
      instances = Core.mempty,
      reasonCodes = Core.mempty,
      status = pStatus_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
reportInstanceStatus_dryRun :: Lens.Lens' ReportInstanceStatus (Core.Maybe Core.Bool)
reportInstanceStatus_dryRun = Lens.lens (\ReportInstanceStatus' {dryRun} -> dryRun) (\s@ReportInstanceStatus' {} a -> s {dryRun = a} :: ReportInstanceStatus)

-- | The time at which the reported instance health state began.
reportInstanceStatus_startTime :: Lens.Lens' ReportInstanceStatus (Core.Maybe Core.UTCTime)
reportInstanceStatus_startTime = Lens.lens (\ReportInstanceStatus' {startTime} -> startTime) (\s@ReportInstanceStatus' {} a -> s {startTime = a} :: ReportInstanceStatus) Core.. Lens.mapping Core._Time

-- | The time at which the reported instance health state ended.
reportInstanceStatus_endTime :: Lens.Lens' ReportInstanceStatus (Core.Maybe Core.UTCTime)
reportInstanceStatus_endTime = Lens.lens (\ReportInstanceStatus' {endTime} -> endTime) (\s@ReportInstanceStatus' {} a -> s {endTime = a} :: ReportInstanceStatus) Core.. Lens.mapping Core._Time

-- | Descriptive text about the health state of your instance.
reportInstanceStatus_description :: Lens.Lens' ReportInstanceStatus (Core.Maybe Core.Text)
reportInstanceStatus_description = Lens.lens (\ReportInstanceStatus' {description} -> description) (\s@ReportInstanceStatus' {} a -> s {description = a} :: ReportInstanceStatus)

-- | The instances.
reportInstanceStatus_instances :: Lens.Lens' ReportInstanceStatus [Core.Text]
reportInstanceStatus_instances = Lens.lens (\ReportInstanceStatus' {instances} -> instances) (\s@ReportInstanceStatus' {} a -> s {instances = a} :: ReportInstanceStatus) Core.. Lens._Coerce

-- | The reason codes that describe the health state of your instance.
--
-- -   @instance-stuck-in-state@: My instance is stuck in a state.
--
-- -   @unresponsive@: My instance is unresponsive.
--
-- -   @not-accepting-credentials@: My instance is not accepting my
--     credentials.
--
-- -   @password-not-available@: A password is not available for my
--     instance.
--
-- -   @performance-network@: My instance is experiencing performance
--     problems that I believe are network related.
--
-- -   @performance-instance-store@: My instance is experiencing
--     performance problems that I believe are related to the instance
--     stores.
--
-- -   @performance-ebs-volume@: My instance is experiencing performance
--     problems that I believe are related to an EBS volume.
--
-- -   @performance-other@: My instance is experiencing performance
--     problems.
--
-- -   @other@: [explain using the description parameter]
reportInstanceStatus_reasonCodes :: Lens.Lens' ReportInstanceStatus [ReportInstanceReasonCodes]
reportInstanceStatus_reasonCodes = Lens.lens (\ReportInstanceStatus' {reasonCodes} -> reasonCodes) (\s@ReportInstanceStatus' {} a -> s {reasonCodes = a} :: ReportInstanceStatus) Core.. Lens._Coerce

-- | The status of all instances listed.
reportInstanceStatus_status :: Lens.Lens' ReportInstanceStatus ReportStatusType
reportInstanceStatus_status = Lens.lens (\ReportInstanceStatus' {status} -> status) (\s@ReportInstanceStatus' {} a -> s {status = a} :: ReportInstanceStatus)

instance Core.AWSRequest ReportInstanceStatus where
  type
    AWSResponse ReportInstanceStatus =
      ReportInstanceStatusResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ReportInstanceStatusResponse'

instance Core.Hashable ReportInstanceStatus

instance Core.NFData ReportInstanceStatus

instance Core.ToHeaders ReportInstanceStatus where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ReportInstanceStatus where
  toPath = Core.const "/"

instance Core.ToQuery ReportInstanceStatus where
  toQuery ReportInstanceStatus' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ReportInstanceStatus" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime,
        "Description" Core.=: description,
        Core.toQueryList "InstanceId" instances,
        Core.toQueryList "ReasonCode" reasonCodes,
        "Status" Core.=: status
      ]

-- | /See:/ 'newReportInstanceStatusResponse' smart constructor.
data ReportInstanceStatusResponse = ReportInstanceStatusResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReportInstanceStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReportInstanceStatusResponse ::
  ReportInstanceStatusResponse
newReportInstanceStatusResponse =
  ReportInstanceStatusResponse'

instance Core.NFData ReportInstanceStatusResponse
