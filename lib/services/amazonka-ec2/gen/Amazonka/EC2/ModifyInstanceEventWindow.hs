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
-- Module      : Amazonka.EC2.ModifyInstanceEventWindow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified event window.
--
-- You can define either a set of time ranges or a cron expression when
-- modifying the event window, but not both.
--
-- To modify the targets associated with the event window, use the
-- AssociateInstanceEventWindow and DisassociateInstanceEventWindow API.
--
-- If Amazon Web Services has already scheduled an event, modifying an
-- event window won\'t change the time of the scheduled event.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/event-windows.html Define event windows for scheduled events>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.ModifyInstanceEventWindow
  ( -- * Creating a Request
    ModifyInstanceEventWindow (..),
    newModifyInstanceEventWindow,

    -- * Request Lenses
    modifyInstanceEventWindow_name,
    modifyInstanceEventWindow_dryRun,
    modifyInstanceEventWindow_timeRanges,
    modifyInstanceEventWindow_cronExpression,
    modifyInstanceEventWindow_instanceEventWindowId,

    -- * Destructuring the Response
    ModifyInstanceEventWindowResponse (..),
    newModifyInstanceEventWindowResponse,

    -- * Response Lenses
    modifyInstanceEventWindowResponse_instanceEventWindow,
    modifyInstanceEventWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyInstanceEventWindow' smart constructor.
data ModifyInstanceEventWindow = ModifyInstanceEventWindow'
  { -- | The name of the event window.
    name :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The time ranges of the event window.
    timeRanges :: Prelude.Maybe [InstanceEventWindowTimeRangeRequest],
    -- | The cron expression of the event window, for example,
    -- @* 0-4,20-23 * * 1,5@.
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
    -- | The ID of the event window.
    instanceEventWindowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceEventWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'modifyInstanceEventWindow_name' - The name of the event window.
--
-- 'dryRun', 'modifyInstanceEventWindow_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'timeRanges', 'modifyInstanceEventWindow_timeRanges' - The time ranges of the event window.
--
-- 'cronExpression', 'modifyInstanceEventWindow_cronExpression' - The cron expression of the event window, for example,
-- @* 0-4,20-23 * * 1,5@.
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
-- 'instanceEventWindowId', 'modifyInstanceEventWindow_instanceEventWindowId' - The ID of the event window.
newModifyInstanceEventWindow ::
  -- | 'instanceEventWindowId'
  Prelude.Text ->
  ModifyInstanceEventWindow
newModifyInstanceEventWindow pInstanceEventWindowId_ =
  ModifyInstanceEventWindow'
    { name = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      timeRanges = Prelude.Nothing,
      cronExpression = Prelude.Nothing,
      instanceEventWindowId = pInstanceEventWindowId_
    }

-- | The name of the event window.
modifyInstanceEventWindow_name :: Lens.Lens' ModifyInstanceEventWindow (Prelude.Maybe Prelude.Text)
modifyInstanceEventWindow_name = Lens.lens (\ModifyInstanceEventWindow' {name} -> name) (\s@ModifyInstanceEventWindow' {} a -> s {name = a} :: ModifyInstanceEventWindow)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceEventWindow_dryRun :: Lens.Lens' ModifyInstanceEventWindow (Prelude.Maybe Prelude.Bool)
modifyInstanceEventWindow_dryRun = Lens.lens (\ModifyInstanceEventWindow' {dryRun} -> dryRun) (\s@ModifyInstanceEventWindow' {} a -> s {dryRun = a} :: ModifyInstanceEventWindow)

-- | The time ranges of the event window.
modifyInstanceEventWindow_timeRanges :: Lens.Lens' ModifyInstanceEventWindow (Prelude.Maybe [InstanceEventWindowTimeRangeRequest])
modifyInstanceEventWindow_timeRanges = Lens.lens (\ModifyInstanceEventWindow' {timeRanges} -> timeRanges) (\s@ModifyInstanceEventWindow' {} a -> s {timeRanges = a} :: ModifyInstanceEventWindow) Prelude.. Lens.mapping Lens.coerced

-- | The cron expression of the event window, for example,
-- @* 0-4,20-23 * * 1,5@.
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
modifyInstanceEventWindow_cronExpression :: Lens.Lens' ModifyInstanceEventWindow (Prelude.Maybe Prelude.Text)
modifyInstanceEventWindow_cronExpression = Lens.lens (\ModifyInstanceEventWindow' {cronExpression} -> cronExpression) (\s@ModifyInstanceEventWindow' {} a -> s {cronExpression = a} :: ModifyInstanceEventWindow)

-- | The ID of the event window.
modifyInstanceEventWindow_instanceEventWindowId :: Lens.Lens' ModifyInstanceEventWindow Prelude.Text
modifyInstanceEventWindow_instanceEventWindowId = Lens.lens (\ModifyInstanceEventWindow' {instanceEventWindowId} -> instanceEventWindowId) (\s@ModifyInstanceEventWindow' {} a -> s {instanceEventWindowId = a} :: ModifyInstanceEventWindow)

instance Core.AWSRequest ModifyInstanceEventWindow where
  type
    AWSResponse ModifyInstanceEventWindow =
      ModifyInstanceEventWindowResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceEventWindowResponse'
            Prelude.<$> (x Core..@? "instanceEventWindow")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyInstanceEventWindow where
  hashWithSalt _salt ModifyInstanceEventWindow' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` timeRanges
      `Prelude.hashWithSalt` cronExpression
      `Prelude.hashWithSalt` instanceEventWindowId

instance Prelude.NFData ModifyInstanceEventWindow where
  rnf ModifyInstanceEventWindow' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf timeRanges
      `Prelude.seq` Prelude.rnf cronExpression
      `Prelude.seq` Prelude.rnf instanceEventWindowId

instance Core.ToHeaders ModifyInstanceEventWindow where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyInstanceEventWindow where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyInstanceEventWindow where
  toQuery ModifyInstanceEventWindow' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyInstanceEventWindow" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "Name" Core.=: name,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TimeRange"
              Prelude.<$> timeRanges
          ),
        "CronExpression" Core.=: cronExpression,
        "InstanceEventWindowId"
          Core.=: instanceEventWindowId
      ]

-- | /See:/ 'newModifyInstanceEventWindowResponse' smart constructor.
data ModifyInstanceEventWindowResponse = ModifyInstanceEventWindowResponse'
  { -- | Information about the event window.
    instanceEventWindow :: Prelude.Maybe InstanceEventWindow,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceEventWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceEventWindow', 'modifyInstanceEventWindowResponse_instanceEventWindow' - Information about the event window.
--
-- 'httpStatus', 'modifyInstanceEventWindowResponse_httpStatus' - The response's http status code.
newModifyInstanceEventWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyInstanceEventWindowResponse
newModifyInstanceEventWindowResponse pHttpStatus_ =
  ModifyInstanceEventWindowResponse'
    { instanceEventWindow =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the event window.
modifyInstanceEventWindowResponse_instanceEventWindow :: Lens.Lens' ModifyInstanceEventWindowResponse (Prelude.Maybe InstanceEventWindow)
modifyInstanceEventWindowResponse_instanceEventWindow = Lens.lens (\ModifyInstanceEventWindowResponse' {instanceEventWindow} -> instanceEventWindow) (\s@ModifyInstanceEventWindowResponse' {} a -> s {instanceEventWindow = a} :: ModifyInstanceEventWindowResponse)

-- | The response's http status code.
modifyInstanceEventWindowResponse_httpStatus :: Lens.Lens' ModifyInstanceEventWindowResponse Prelude.Int
modifyInstanceEventWindowResponse_httpStatus = Lens.lens (\ModifyInstanceEventWindowResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceEventWindowResponse' {} a -> s {httpStatus = a} :: ModifyInstanceEventWindowResponse)

instance
  Prelude.NFData
    ModifyInstanceEventWindowResponse
  where
  rnf ModifyInstanceEventWindowResponse' {..} =
    Prelude.rnf instanceEventWindow
      `Prelude.seq` Prelude.rnf httpStatus
