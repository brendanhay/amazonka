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
-- Module      : Amazonka.EC2.CreateInstanceEventWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an event window in which scheduled events for the associated
-- Amazon EC2 instances can run.
--
-- You can define either a set of time ranges or a cron expression when
-- creating the event window, but not both. All event window times are in
-- UTC.
--
-- You can create up to 200 event windows per Amazon Web Services Region.
--
-- When you create the event window, targets (instance IDs, Dedicated Host
-- IDs, or tags) are not yet associated with it. To ensure that the event
-- window can be used, you must associate one or more targets with it by
-- using the AssociateInstanceEventWindow API.
--
-- Event windows are applicable only for scheduled events that stop,
-- reboot, or terminate instances.
--
-- Event windows are /not/ applicable for:
--
-- -   Expedited scheduled events and network maintenance events.
--
-- -   Unscheduled maintenance such as AutoRecovery and unplanned reboots.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/event-windows.html Define event windows for scheduled events>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.CreateInstanceEventWindow
  ( -- * Creating a Request
    CreateInstanceEventWindow (..),
    newCreateInstanceEventWindow,

    -- * Request Lenses
    createInstanceEventWindow_cronExpression,
    createInstanceEventWindow_dryRun,
    createInstanceEventWindow_name,
    createInstanceEventWindow_tagSpecifications,
    createInstanceEventWindow_timeRanges,

    -- * Destructuring the Response
    CreateInstanceEventWindowResponse (..),
    newCreateInstanceEventWindowResponse,

    -- * Response Lenses
    createInstanceEventWindowResponse_instanceEventWindow,
    createInstanceEventWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInstanceEventWindow' smart constructor.
data CreateInstanceEventWindow = CreateInstanceEventWindow'
  { -- | The cron expression for the event window, for example,
    -- @* 0-4,20-23 * * 1,5@. If you specify a cron expression, you can\'t
    -- specify a time range.
    --
    -- Constraints:
    --
    -- -   Only hour and day of the week values are supported.
    --
    -- -   For day of the week values, you can specify either integers @0@
    --     through @6@, or alternative single values @SUN@ through @SAT@.
    --
    -- -   The minute, month, and year must be specified by @*@.
    --
    -- -   The hour value must be one or a multiple range, for example, @0-4@
    --     or @0-4,20-23@.
    --
    -- -   Each hour range must be >= 2 hours, for example, @0-2@ or @20-23@.
    --
    -- -   The event window must be >= 4 hours. The combined total time ranges
    --     in the event window must be >= 4 hours.
    --
    -- For more information about cron expressions, see
    -- <https://en.wikipedia.org/wiki/Cron cron> on the /Wikipedia website/.
    cronExpression :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the event window.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the event window.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The time range for the event window. If you specify a time range, you
    -- can\'t specify a cron expression.
    timeRanges :: Prelude.Maybe [InstanceEventWindowTimeRangeRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceEventWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cronExpression', 'createInstanceEventWindow_cronExpression' - The cron expression for the event window, for example,
-- @* 0-4,20-23 * * 1,5@. If you specify a cron expression, you can\'t
-- specify a time range.
--
-- Constraints:
--
-- -   Only hour and day of the week values are supported.
--
-- -   For day of the week values, you can specify either integers @0@
--     through @6@, or alternative single values @SUN@ through @SAT@.
--
-- -   The minute, month, and year must be specified by @*@.
--
-- -   The hour value must be one or a multiple range, for example, @0-4@
--     or @0-4,20-23@.
--
-- -   Each hour range must be >= 2 hours, for example, @0-2@ or @20-23@.
--
-- -   The event window must be >= 4 hours. The combined total time ranges
--     in the event window must be >= 4 hours.
--
-- For more information about cron expressions, see
-- <https://en.wikipedia.org/wiki/Cron cron> on the /Wikipedia website/.
--
-- 'dryRun', 'createInstanceEventWindow_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'name', 'createInstanceEventWindow_name' - The name of the event window.
--
-- 'tagSpecifications', 'createInstanceEventWindow_tagSpecifications' - The tags to apply to the event window.
--
-- 'timeRanges', 'createInstanceEventWindow_timeRanges' - The time range for the event window. If you specify a time range, you
-- can\'t specify a cron expression.
newCreateInstanceEventWindow ::
  CreateInstanceEventWindow
newCreateInstanceEventWindow =
  CreateInstanceEventWindow'
    { cronExpression =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      name = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      timeRanges = Prelude.Nothing
    }

-- | The cron expression for the event window, for example,
-- @* 0-4,20-23 * * 1,5@. If you specify a cron expression, you can\'t
-- specify a time range.
--
-- Constraints:
--
-- -   Only hour and day of the week values are supported.
--
-- -   For day of the week values, you can specify either integers @0@
--     through @6@, or alternative single values @SUN@ through @SAT@.
--
-- -   The minute, month, and year must be specified by @*@.
--
-- -   The hour value must be one or a multiple range, for example, @0-4@
--     or @0-4,20-23@.
--
-- -   Each hour range must be >= 2 hours, for example, @0-2@ or @20-23@.
--
-- -   The event window must be >= 4 hours. The combined total time ranges
--     in the event window must be >= 4 hours.
--
-- For more information about cron expressions, see
-- <https://en.wikipedia.org/wiki/Cron cron> on the /Wikipedia website/.
createInstanceEventWindow_cronExpression :: Lens.Lens' CreateInstanceEventWindow (Prelude.Maybe Prelude.Text)
createInstanceEventWindow_cronExpression = Lens.lens (\CreateInstanceEventWindow' {cronExpression} -> cronExpression) (\s@CreateInstanceEventWindow' {} a -> s {cronExpression = a} :: CreateInstanceEventWindow)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createInstanceEventWindow_dryRun :: Lens.Lens' CreateInstanceEventWindow (Prelude.Maybe Prelude.Bool)
createInstanceEventWindow_dryRun = Lens.lens (\CreateInstanceEventWindow' {dryRun} -> dryRun) (\s@CreateInstanceEventWindow' {} a -> s {dryRun = a} :: CreateInstanceEventWindow)

-- | The name of the event window.
createInstanceEventWindow_name :: Lens.Lens' CreateInstanceEventWindow (Prelude.Maybe Prelude.Text)
createInstanceEventWindow_name = Lens.lens (\CreateInstanceEventWindow' {name} -> name) (\s@CreateInstanceEventWindow' {} a -> s {name = a} :: CreateInstanceEventWindow)

-- | The tags to apply to the event window.
createInstanceEventWindow_tagSpecifications :: Lens.Lens' CreateInstanceEventWindow (Prelude.Maybe [TagSpecification])
createInstanceEventWindow_tagSpecifications = Lens.lens (\CreateInstanceEventWindow' {tagSpecifications} -> tagSpecifications) (\s@CreateInstanceEventWindow' {} a -> s {tagSpecifications = a} :: CreateInstanceEventWindow) Prelude.. Lens.mapping Lens.coerced

-- | The time range for the event window. If you specify a time range, you
-- can\'t specify a cron expression.
createInstanceEventWindow_timeRanges :: Lens.Lens' CreateInstanceEventWindow (Prelude.Maybe [InstanceEventWindowTimeRangeRequest])
createInstanceEventWindow_timeRanges = Lens.lens (\CreateInstanceEventWindow' {timeRanges} -> timeRanges) (\s@CreateInstanceEventWindow' {} a -> s {timeRanges = a} :: CreateInstanceEventWindow) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateInstanceEventWindow where
  type
    AWSResponse CreateInstanceEventWindow =
      CreateInstanceEventWindowResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInstanceEventWindowResponse'
            Prelude.<$> (x Data..@? "instanceEventWindow")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInstanceEventWindow where
  hashWithSalt _salt CreateInstanceEventWindow' {..} =
    _salt
      `Prelude.hashWithSalt` cronExpression
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` timeRanges

instance Prelude.NFData CreateInstanceEventWindow where
  rnf CreateInstanceEventWindow' {..} =
    Prelude.rnf cronExpression `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf tagSpecifications `Prelude.seq`
            Prelude.rnf timeRanges

instance Data.ToHeaders CreateInstanceEventWindow where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateInstanceEventWindow where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateInstanceEventWindow where
  toQuery CreateInstanceEventWindow' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateInstanceEventWindow" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "CronExpression" Data.=: cronExpression,
        "DryRun" Data.=: dryRun,
        "Name" Data.=: name,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        Data.toQuery
          ( Data.toQueryList "TimeRange"
              Prelude.<$> timeRanges
          )
      ]

-- | /See:/ 'newCreateInstanceEventWindowResponse' smart constructor.
data CreateInstanceEventWindowResponse = CreateInstanceEventWindowResponse'
  { -- | Information about the event window.
    instanceEventWindow :: Prelude.Maybe InstanceEventWindow,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceEventWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceEventWindow', 'createInstanceEventWindowResponse_instanceEventWindow' - Information about the event window.
--
-- 'httpStatus', 'createInstanceEventWindowResponse_httpStatus' - The response's http status code.
newCreateInstanceEventWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInstanceEventWindowResponse
newCreateInstanceEventWindowResponse pHttpStatus_ =
  CreateInstanceEventWindowResponse'
    { instanceEventWindow =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the event window.
createInstanceEventWindowResponse_instanceEventWindow :: Lens.Lens' CreateInstanceEventWindowResponse (Prelude.Maybe InstanceEventWindow)
createInstanceEventWindowResponse_instanceEventWindow = Lens.lens (\CreateInstanceEventWindowResponse' {instanceEventWindow} -> instanceEventWindow) (\s@CreateInstanceEventWindowResponse' {} a -> s {instanceEventWindow = a} :: CreateInstanceEventWindowResponse)

-- | The response's http status code.
createInstanceEventWindowResponse_httpStatus :: Lens.Lens' CreateInstanceEventWindowResponse Prelude.Int
createInstanceEventWindowResponse_httpStatus = Lens.lens (\CreateInstanceEventWindowResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceEventWindowResponse' {} a -> s {httpStatus = a} :: CreateInstanceEventWindowResponse)

instance
  Prelude.NFData
    CreateInstanceEventWindowResponse
  where
  rnf CreateInstanceEventWindowResponse' {..} =
    Prelude.rnf instanceEventWindow `Prelude.seq`
      Prelude.rnf httpStatus
