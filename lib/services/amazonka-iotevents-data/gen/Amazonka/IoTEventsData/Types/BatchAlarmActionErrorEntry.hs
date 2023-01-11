{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTEventsData.Types.BatchAlarmActionErrorEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.BatchAlarmActionErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains error messages associated with one of the following requests:
--
-- -   <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchAcknowledgeAlarm.html BatchAcknowledgeAlarm>
--
-- -   <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchDisableAlarm.html BatchDisableAlarm>
--
-- -   <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchEnableAlarm.html BatchEnableAlarm>
--
-- -   <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchResetAlarm.html BatchResetAlarm>
--
-- -   <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchSnoozeAlarm.html BatchSnoozeAlarm>
--
-- /See:/ 'newBatchAlarmActionErrorEntry' smart constructor.
data BatchAlarmActionErrorEntry = BatchAlarmActionErrorEntry'
  { -- | The error code.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | A message that describes the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The request ID. Each ID must be unique within each batch.
    requestId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAlarmActionErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchAlarmActionErrorEntry_errorCode' - The error code.
--
-- 'errorMessage', 'batchAlarmActionErrorEntry_errorMessage' - A message that describes the error.
--
-- 'requestId', 'batchAlarmActionErrorEntry_requestId' - The request ID. Each ID must be unique within each batch.
newBatchAlarmActionErrorEntry ::
  BatchAlarmActionErrorEntry
newBatchAlarmActionErrorEntry =
  BatchAlarmActionErrorEntry'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      requestId = Prelude.Nothing
    }

-- | The error code.
batchAlarmActionErrorEntry_errorCode :: Lens.Lens' BatchAlarmActionErrorEntry (Prelude.Maybe ErrorCode)
batchAlarmActionErrorEntry_errorCode = Lens.lens (\BatchAlarmActionErrorEntry' {errorCode} -> errorCode) (\s@BatchAlarmActionErrorEntry' {} a -> s {errorCode = a} :: BatchAlarmActionErrorEntry)

-- | A message that describes the error.
batchAlarmActionErrorEntry_errorMessage :: Lens.Lens' BatchAlarmActionErrorEntry (Prelude.Maybe Prelude.Text)
batchAlarmActionErrorEntry_errorMessage = Lens.lens (\BatchAlarmActionErrorEntry' {errorMessage} -> errorMessage) (\s@BatchAlarmActionErrorEntry' {} a -> s {errorMessage = a} :: BatchAlarmActionErrorEntry)

-- | The request ID. Each ID must be unique within each batch.
batchAlarmActionErrorEntry_requestId :: Lens.Lens' BatchAlarmActionErrorEntry (Prelude.Maybe Prelude.Text)
batchAlarmActionErrorEntry_requestId = Lens.lens (\BatchAlarmActionErrorEntry' {requestId} -> requestId) (\s@BatchAlarmActionErrorEntry' {} a -> s {requestId = a} :: BatchAlarmActionErrorEntry)

instance Data.FromJSON BatchAlarmActionErrorEntry where
  parseJSON =
    Data.withObject
      "BatchAlarmActionErrorEntry"
      ( \x ->
          BatchAlarmActionErrorEntry'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "requestId")
      )

instance Prelude.Hashable BatchAlarmActionErrorEntry where
  hashWithSalt _salt BatchAlarmActionErrorEntry' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` requestId

instance Prelude.NFData BatchAlarmActionErrorEntry where
  rnf BatchAlarmActionErrorEntry' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf requestId
