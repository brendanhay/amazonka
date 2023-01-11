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
-- Module      : Amazonka.IoTEventsData.BatchSnoozeAlarm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes one or more alarms to the snooze mode. The alarms change to the
-- @SNOOZE_DISABLED@ state after you set them to the snooze mode.
module Amazonka.IoTEventsData.BatchSnoozeAlarm
  ( -- * Creating a Request
    BatchSnoozeAlarm (..),
    newBatchSnoozeAlarm,

    -- * Request Lenses
    batchSnoozeAlarm_snoozeActionRequests,

    -- * Destructuring the Response
    BatchSnoozeAlarmResponse (..),
    newBatchSnoozeAlarmResponse,

    -- * Response Lenses
    batchSnoozeAlarmResponse_errorEntries,
    batchSnoozeAlarmResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchSnoozeAlarm' smart constructor.
data BatchSnoozeAlarm = BatchSnoozeAlarm'
  { -- | The list of snooze action requests. You can specify up to 10 requests
    -- per operation.
    snoozeActionRequests :: Prelude.NonEmpty SnoozeAlarmActionRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchSnoozeAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snoozeActionRequests', 'batchSnoozeAlarm_snoozeActionRequests' - The list of snooze action requests. You can specify up to 10 requests
-- per operation.
newBatchSnoozeAlarm ::
  -- | 'snoozeActionRequests'
  Prelude.NonEmpty SnoozeAlarmActionRequest ->
  BatchSnoozeAlarm
newBatchSnoozeAlarm pSnoozeActionRequests_ =
  BatchSnoozeAlarm'
    { snoozeActionRequests =
        Lens.coerced Lens.# pSnoozeActionRequests_
    }

-- | The list of snooze action requests. You can specify up to 10 requests
-- per operation.
batchSnoozeAlarm_snoozeActionRequests :: Lens.Lens' BatchSnoozeAlarm (Prelude.NonEmpty SnoozeAlarmActionRequest)
batchSnoozeAlarm_snoozeActionRequests = Lens.lens (\BatchSnoozeAlarm' {snoozeActionRequests} -> snoozeActionRequests) (\s@BatchSnoozeAlarm' {} a -> s {snoozeActionRequests = a} :: BatchSnoozeAlarm) Prelude.. Lens.coerced

instance Core.AWSRequest BatchSnoozeAlarm where
  type
    AWSResponse BatchSnoozeAlarm =
      BatchSnoozeAlarmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchSnoozeAlarmResponse'
            Prelude.<$> (x Data..?> "errorEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchSnoozeAlarm where
  hashWithSalt _salt BatchSnoozeAlarm' {..} =
    _salt `Prelude.hashWithSalt` snoozeActionRequests

instance Prelude.NFData BatchSnoozeAlarm where
  rnf BatchSnoozeAlarm' {..} =
    Prelude.rnf snoozeActionRequests

instance Data.ToHeaders BatchSnoozeAlarm where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchSnoozeAlarm where
  toJSON BatchSnoozeAlarm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "snoozeActionRequests"
                  Data..= snoozeActionRequests
              )
          ]
      )

instance Data.ToPath BatchSnoozeAlarm where
  toPath = Prelude.const "/alarms/snooze"

instance Data.ToQuery BatchSnoozeAlarm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchSnoozeAlarmResponse' smart constructor.
data BatchSnoozeAlarmResponse = BatchSnoozeAlarmResponse'
  { -- | A list of errors associated with the request, or @null@ if there are no
    -- errors. Each error entry contains an entry ID that helps you identify
    -- the entry that failed.
    errorEntries :: Prelude.Maybe [BatchAlarmActionErrorEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchSnoozeAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorEntries', 'batchSnoozeAlarmResponse_errorEntries' - A list of errors associated with the request, or @null@ if there are no
-- errors. Each error entry contains an entry ID that helps you identify
-- the entry that failed.
--
-- 'httpStatus', 'batchSnoozeAlarmResponse_httpStatus' - The response's http status code.
newBatchSnoozeAlarmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchSnoozeAlarmResponse
newBatchSnoozeAlarmResponse pHttpStatus_ =
  BatchSnoozeAlarmResponse'
    { errorEntries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors associated with the request, or @null@ if there are no
-- errors. Each error entry contains an entry ID that helps you identify
-- the entry that failed.
batchSnoozeAlarmResponse_errorEntries :: Lens.Lens' BatchSnoozeAlarmResponse (Prelude.Maybe [BatchAlarmActionErrorEntry])
batchSnoozeAlarmResponse_errorEntries = Lens.lens (\BatchSnoozeAlarmResponse' {errorEntries} -> errorEntries) (\s@BatchSnoozeAlarmResponse' {} a -> s {errorEntries = a} :: BatchSnoozeAlarmResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchSnoozeAlarmResponse_httpStatus :: Lens.Lens' BatchSnoozeAlarmResponse Prelude.Int
batchSnoozeAlarmResponse_httpStatus = Lens.lens (\BatchSnoozeAlarmResponse' {httpStatus} -> httpStatus) (\s@BatchSnoozeAlarmResponse' {} a -> s {httpStatus = a} :: BatchSnoozeAlarmResponse)

instance Prelude.NFData BatchSnoozeAlarmResponse where
  rnf BatchSnoozeAlarmResponse' {..} =
    Prelude.rnf errorEntries
      `Prelude.seq` Prelude.rnf httpStatus
