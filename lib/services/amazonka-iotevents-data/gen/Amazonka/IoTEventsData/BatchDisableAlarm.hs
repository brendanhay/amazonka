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
-- Module      : Amazonka.IoTEventsData.BatchDisableAlarm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables one or more alarms. The alarms change to the @DISABLED@ state
-- after you disable them.
module Amazonka.IoTEventsData.BatchDisableAlarm
  ( -- * Creating a Request
    BatchDisableAlarm (..),
    newBatchDisableAlarm,

    -- * Request Lenses
    batchDisableAlarm_disableActionRequests,

    -- * Destructuring the Response
    BatchDisableAlarmResponse (..),
    newBatchDisableAlarmResponse,

    -- * Response Lenses
    batchDisableAlarmResponse_errorEntries,
    batchDisableAlarmResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDisableAlarm' smart constructor.
data BatchDisableAlarm = BatchDisableAlarm'
  { -- | The list of disable action requests. You can specify up to 10 requests
    -- per operation.
    disableActionRequests :: Prelude.NonEmpty DisableAlarmActionRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisableAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableActionRequests', 'batchDisableAlarm_disableActionRequests' - The list of disable action requests. You can specify up to 10 requests
-- per operation.
newBatchDisableAlarm ::
  -- | 'disableActionRequests'
  Prelude.NonEmpty DisableAlarmActionRequest ->
  BatchDisableAlarm
newBatchDisableAlarm pDisableActionRequests_ =
  BatchDisableAlarm'
    { disableActionRequests =
        Lens.coerced Lens.# pDisableActionRequests_
    }

-- | The list of disable action requests. You can specify up to 10 requests
-- per operation.
batchDisableAlarm_disableActionRequests :: Lens.Lens' BatchDisableAlarm (Prelude.NonEmpty DisableAlarmActionRequest)
batchDisableAlarm_disableActionRequests = Lens.lens (\BatchDisableAlarm' {disableActionRequests} -> disableActionRequests) (\s@BatchDisableAlarm' {} a -> s {disableActionRequests = a} :: BatchDisableAlarm) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDisableAlarm where
  type
    AWSResponse BatchDisableAlarm =
      BatchDisableAlarmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisableAlarmResponse'
            Prelude.<$> (x Data..?> "errorEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDisableAlarm where
  hashWithSalt _salt BatchDisableAlarm' {..} =
    _salt `Prelude.hashWithSalt` disableActionRequests

instance Prelude.NFData BatchDisableAlarm where
  rnf BatchDisableAlarm' {..} =
    Prelude.rnf disableActionRequests

instance Data.ToHeaders BatchDisableAlarm where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchDisableAlarm where
  toJSON BatchDisableAlarm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "disableActionRequests"
                  Data..= disableActionRequests
              )
          ]
      )

instance Data.ToPath BatchDisableAlarm where
  toPath = Prelude.const "/alarms/disable"

instance Data.ToQuery BatchDisableAlarm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDisableAlarmResponse' smart constructor.
data BatchDisableAlarmResponse = BatchDisableAlarmResponse'
  { -- | A list of errors associated with the request, or @null@ if there are no
    -- errors. Each error entry contains an entry ID that helps you identify
    -- the entry that failed.
    errorEntries :: Prelude.Maybe [BatchAlarmActionErrorEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisableAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorEntries', 'batchDisableAlarmResponse_errorEntries' - A list of errors associated with the request, or @null@ if there are no
-- errors. Each error entry contains an entry ID that helps you identify
-- the entry that failed.
--
-- 'httpStatus', 'batchDisableAlarmResponse_httpStatus' - The response's http status code.
newBatchDisableAlarmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDisableAlarmResponse
newBatchDisableAlarmResponse pHttpStatus_ =
  BatchDisableAlarmResponse'
    { errorEntries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors associated with the request, or @null@ if there are no
-- errors. Each error entry contains an entry ID that helps you identify
-- the entry that failed.
batchDisableAlarmResponse_errorEntries :: Lens.Lens' BatchDisableAlarmResponse (Prelude.Maybe [BatchAlarmActionErrorEntry])
batchDisableAlarmResponse_errorEntries = Lens.lens (\BatchDisableAlarmResponse' {errorEntries} -> errorEntries) (\s@BatchDisableAlarmResponse' {} a -> s {errorEntries = a} :: BatchDisableAlarmResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDisableAlarmResponse_httpStatus :: Lens.Lens' BatchDisableAlarmResponse Prelude.Int
batchDisableAlarmResponse_httpStatus = Lens.lens (\BatchDisableAlarmResponse' {httpStatus} -> httpStatus) (\s@BatchDisableAlarmResponse' {} a -> s {httpStatus = a} :: BatchDisableAlarmResponse)

instance Prelude.NFData BatchDisableAlarmResponse where
  rnf BatchDisableAlarmResponse' {..} =
    Prelude.rnf errorEntries
      `Prelude.seq` Prelude.rnf httpStatus
