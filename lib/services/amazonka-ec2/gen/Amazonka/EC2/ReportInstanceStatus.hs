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
-- Module      : Amazonka.EC2.ReportInstanceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.ReportInstanceStatus
  ( -- * Creating a Request
    ReportInstanceStatus (..),
    newReportInstanceStatus,

    -- * Request Lenses
    reportInstanceStatus_description,
    reportInstanceStatus_dryRun,
    reportInstanceStatus_endTime,
    reportInstanceStatus_startTime,
    reportInstanceStatus_instances,
    reportInstanceStatus_reasonCodes,
    reportInstanceStatus_status,

    -- * Destructuring the Response
    ReportInstanceStatusResponse (..),
    newReportInstanceStatusResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReportInstanceStatus' smart constructor.
data ReportInstanceStatus = ReportInstanceStatus'
  { -- | Descriptive text about the health state of your instance.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The time at which the reported instance health state ended.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The time at which the reported instance health state began.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The instances.
    instances :: [Prelude.Text],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportInstanceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'reportInstanceStatus_description' - Descriptive text about the health state of your instance.
--
-- 'dryRun', 'reportInstanceStatus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'endTime', 'reportInstanceStatus_endTime' - The time at which the reported instance health state ended.
--
-- 'startTime', 'reportInstanceStatus_startTime' - The time at which the reported instance health state began.
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
    { description =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      endTime = Prelude.Nothing,
      startTime = Prelude.Nothing,
      instances = Prelude.mempty,
      reasonCodes = Prelude.mempty,
      status = pStatus_
    }

-- | Descriptive text about the health state of your instance.
reportInstanceStatus_description :: Lens.Lens' ReportInstanceStatus (Prelude.Maybe Prelude.Text)
reportInstanceStatus_description = Lens.lens (\ReportInstanceStatus' {description} -> description) (\s@ReportInstanceStatus' {} a -> s {description = a} :: ReportInstanceStatus)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
reportInstanceStatus_dryRun :: Lens.Lens' ReportInstanceStatus (Prelude.Maybe Prelude.Bool)
reportInstanceStatus_dryRun = Lens.lens (\ReportInstanceStatus' {dryRun} -> dryRun) (\s@ReportInstanceStatus' {} a -> s {dryRun = a} :: ReportInstanceStatus)

-- | The time at which the reported instance health state ended.
reportInstanceStatus_endTime :: Lens.Lens' ReportInstanceStatus (Prelude.Maybe Prelude.UTCTime)
reportInstanceStatus_endTime = Lens.lens (\ReportInstanceStatus' {endTime} -> endTime) (\s@ReportInstanceStatus' {} a -> s {endTime = a} :: ReportInstanceStatus) Prelude.. Lens.mapping Data._Time

-- | The time at which the reported instance health state began.
reportInstanceStatus_startTime :: Lens.Lens' ReportInstanceStatus (Prelude.Maybe Prelude.UTCTime)
reportInstanceStatus_startTime = Lens.lens (\ReportInstanceStatus' {startTime} -> startTime) (\s@ReportInstanceStatus' {} a -> s {startTime = a} :: ReportInstanceStatus) Prelude.. Lens.mapping Data._Time

-- | The instances.
reportInstanceStatus_instances :: Lens.Lens' ReportInstanceStatus [Prelude.Text]
reportInstanceStatus_instances = Lens.lens (\ReportInstanceStatus' {instances} -> instances) (\s@ReportInstanceStatus' {} a -> s {instances = a} :: ReportInstanceStatus) Prelude.. Lens.coerced

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
reportInstanceStatus_reasonCodes = Lens.lens (\ReportInstanceStatus' {reasonCodes} -> reasonCodes) (\s@ReportInstanceStatus' {} a -> s {reasonCodes = a} :: ReportInstanceStatus) Prelude.. Lens.coerced

-- | The status of all instances listed.
reportInstanceStatus_status :: Lens.Lens' ReportInstanceStatus ReportStatusType
reportInstanceStatus_status = Lens.lens (\ReportInstanceStatus' {status} -> status) (\s@ReportInstanceStatus' {} a -> s {status = a} :: ReportInstanceStatus)

instance Core.AWSRequest ReportInstanceStatus where
  type
    AWSResponse ReportInstanceStatus =
      ReportInstanceStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ReportInstanceStatusResponse'

instance Prelude.Hashable ReportInstanceStatus where
  hashWithSalt _salt ReportInstanceStatus' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` reasonCodes
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReportInstanceStatus where
  rnf ReportInstanceStatus' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf endTime `Prelude.seq`
          Prelude.rnf startTime `Prelude.seq`
            Prelude.rnf instances `Prelude.seq`
              Prelude.rnf reasonCodes `Prelude.seq`
                Prelude.rnf status

instance Data.ToHeaders ReportInstanceStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReportInstanceStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery ReportInstanceStatus where
  toQuery ReportInstanceStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ReportInstanceStatus" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "EndTime" Data.=: endTime,
        "StartTime" Data.=: startTime,
        Data.toQueryList "InstanceId" instances,
        Data.toQueryList "ReasonCode" reasonCodes,
        "Status" Data.=: status
      ]

-- | /See:/ 'newReportInstanceStatusResponse' smart constructor.
data ReportInstanceStatusResponse = ReportInstanceStatusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportInstanceStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReportInstanceStatusResponse ::
  ReportInstanceStatusResponse
newReportInstanceStatusResponse =
  ReportInstanceStatusResponse'

instance Prelude.NFData ReportInstanceStatusResponse where
  rnf _ = ()
