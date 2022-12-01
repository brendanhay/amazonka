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
-- Module      : Amazonka.IoTEventsData.BatchAcknowledgeAlarm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Acknowledges one or more alarms. The alarms change to the @ACKNOWLEDGED@
-- state after you acknowledge them.
module Amazonka.IoTEventsData.BatchAcknowledgeAlarm
  ( -- * Creating a Request
    BatchAcknowledgeAlarm (..),
    newBatchAcknowledgeAlarm,

    -- * Request Lenses
    batchAcknowledgeAlarm_acknowledgeActionRequests,

    -- * Destructuring the Response
    BatchAcknowledgeAlarmResponse (..),
    newBatchAcknowledgeAlarmResponse,

    -- * Response Lenses
    batchAcknowledgeAlarmResponse_errorEntries,
    batchAcknowledgeAlarmResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchAcknowledgeAlarm' smart constructor.
data BatchAcknowledgeAlarm = BatchAcknowledgeAlarm'
  { -- | The list of acknowledge action requests. You can specify up to 10
    -- requests per operation.
    acknowledgeActionRequests :: Prelude.NonEmpty AcknowledgeAlarmActionRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAcknowledgeAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acknowledgeActionRequests', 'batchAcknowledgeAlarm_acknowledgeActionRequests' - The list of acknowledge action requests. You can specify up to 10
-- requests per operation.
newBatchAcknowledgeAlarm ::
  -- | 'acknowledgeActionRequests'
  Prelude.NonEmpty AcknowledgeAlarmActionRequest ->
  BatchAcknowledgeAlarm
newBatchAcknowledgeAlarm pAcknowledgeActionRequests_ =
  BatchAcknowledgeAlarm'
    { acknowledgeActionRequests =
        Lens.coerced Lens.# pAcknowledgeActionRequests_
    }

-- | The list of acknowledge action requests. You can specify up to 10
-- requests per operation.
batchAcknowledgeAlarm_acknowledgeActionRequests :: Lens.Lens' BatchAcknowledgeAlarm (Prelude.NonEmpty AcknowledgeAlarmActionRequest)
batchAcknowledgeAlarm_acknowledgeActionRequests = Lens.lens (\BatchAcknowledgeAlarm' {acknowledgeActionRequests} -> acknowledgeActionRequests) (\s@BatchAcknowledgeAlarm' {} a -> s {acknowledgeActionRequests = a} :: BatchAcknowledgeAlarm) Prelude.. Lens.coerced

instance Core.AWSRequest BatchAcknowledgeAlarm where
  type
    AWSResponse BatchAcknowledgeAlarm =
      BatchAcknowledgeAlarmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAcknowledgeAlarmResponse'
            Prelude.<$> (x Core..?> "errorEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchAcknowledgeAlarm where
  hashWithSalt _salt BatchAcknowledgeAlarm' {..} =
    _salt
      `Prelude.hashWithSalt` acknowledgeActionRequests

instance Prelude.NFData BatchAcknowledgeAlarm where
  rnf BatchAcknowledgeAlarm' {..} =
    Prelude.rnf acknowledgeActionRequests

instance Core.ToHeaders BatchAcknowledgeAlarm where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON BatchAcknowledgeAlarm where
  toJSON BatchAcknowledgeAlarm' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "acknowledgeActionRequests"
                  Core..= acknowledgeActionRequests
              )
          ]
      )

instance Core.ToPath BatchAcknowledgeAlarm where
  toPath = Prelude.const "/alarms/acknowledge"

instance Core.ToQuery BatchAcknowledgeAlarm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAcknowledgeAlarmResponse' smart constructor.
data BatchAcknowledgeAlarmResponse = BatchAcknowledgeAlarmResponse'
  { -- | A list of errors associated with the request, or @null@ if there are no
    -- errors. Each error entry contains an entry ID that helps you identify
    -- the entry that failed.
    errorEntries :: Prelude.Maybe [BatchAlarmActionErrorEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAcknowledgeAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorEntries', 'batchAcknowledgeAlarmResponse_errorEntries' - A list of errors associated with the request, or @null@ if there are no
-- errors. Each error entry contains an entry ID that helps you identify
-- the entry that failed.
--
-- 'httpStatus', 'batchAcknowledgeAlarmResponse_httpStatus' - The response's http status code.
newBatchAcknowledgeAlarmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchAcknowledgeAlarmResponse
newBatchAcknowledgeAlarmResponse pHttpStatus_ =
  BatchAcknowledgeAlarmResponse'
    { errorEntries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors associated with the request, or @null@ if there are no
-- errors. Each error entry contains an entry ID that helps you identify
-- the entry that failed.
batchAcknowledgeAlarmResponse_errorEntries :: Lens.Lens' BatchAcknowledgeAlarmResponse (Prelude.Maybe [BatchAlarmActionErrorEntry])
batchAcknowledgeAlarmResponse_errorEntries = Lens.lens (\BatchAcknowledgeAlarmResponse' {errorEntries} -> errorEntries) (\s@BatchAcknowledgeAlarmResponse' {} a -> s {errorEntries = a} :: BatchAcknowledgeAlarmResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchAcknowledgeAlarmResponse_httpStatus :: Lens.Lens' BatchAcknowledgeAlarmResponse Prelude.Int
batchAcknowledgeAlarmResponse_httpStatus = Lens.lens (\BatchAcknowledgeAlarmResponse' {httpStatus} -> httpStatus) (\s@BatchAcknowledgeAlarmResponse' {} a -> s {httpStatus = a} :: BatchAcknowledgeAlarmResponse)

instance Prelude.NFData BatchAcknowledgeAlarmResponse where
  rnf BatchAcknowledgeAlarmResponse' {..} =
    Prelude.rnf errorEntries
      `Prelude.seq` Prelude.rnf httpStatus
