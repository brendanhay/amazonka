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
-- Module      : Amazonka.IoTEventsData.Types.BatchDeleteDetectorErrorEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.BatchDeleteDetectorErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains error messages associated with the deletion request.
--
-- /See:/ 'newBatchDeleteDetectorErrorEntry' smart constructor.
data BatchDeleteDetectorErrorEntry = BatchDeleteDetectorErrorEntry'
  { -- | The error code.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | A message that describes the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the message that caused the error. (See the value of the
    -- @\"messageId\"@ in the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchDeleteDetector.html#iotevents-iotevents-data_BatchDeleteDetector-request-detectors detectors>
    -- object of the @DeleteDetectorRequest@.)
    messageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDetectorErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchDeleteDetectorErrorEntry_errorCode' - The error code.
--
-- 'errorMessage', 'batchDeleteDetectorErrorEntry_errorMessage' - A message that describes the error.
--
-- 'messageId', 'batchDeleteDetectorErrorEntry_messageId' - The ID of the message that caused the error. (See the value of the
-- @\"messageId\"@ in the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchDeleteDetector.html#iotevents-iotevents-data_BatchDeleteDetector-request-detectors detectors>
-- object of the @DeleteDetectorRequest@.)
newBatchDeleteDetectorErrorEntry ::
  BatchDeleteDetectorErrorEntry
newBatchDeleteDetectorErrorEntry =
  BatchDeleteDetectorErrorEntry'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      messageId = Prelude.Nothing
    }

-- | The error code.
batchDeleteDetectorErrorEntry_errorCode :: Lens.Lens' BatchDeleteDetectorErrorEntry (Prelude.Maybe ErrorCode)
batchDeleteDetectorErrorEntry_errorCode = Lens.lens (\BatchDeleteDetectorErrorEntry' {errorCode} -> errorCode) (\s@BatchDeleteDetectorErrorEntry' {} a -> s {errorCode = a} :: BatchDeleteDetectorErrorEntry)

-- | A message that describes the error.
batchDeleteDetectorErrorEntry_errorMessage :: Lens.Lens' BatchDeleteDetectorErrorEntry (Prelude.Maybe Prelude.Text)
batchDeleteDetectorErrorEntry_errorMessage = Lens.lens (\BatchDeleteDetectorErrorEntry' {errorMessage} -> errorMessage) (\s@BatchDeleteDetectorErrorEntry' {} a -> s {errorMessage = a} :: BatchDeleteDetectorErrorEntry)

-- | The ID of the message that caused the error. (See the value of the
-- @\"messageId\"@ in the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchDeleteDetector.html#iotevents-iotevents-data_BatchDeleteDetector-request-detectors detectors>
-- object of the @DeleteDetectorRequest@.)
batchDeleteDetectorErrorEntry_messageId :: Lens.Lens' BatchDeleteDetectorErrorEntry (Prelude.Maybe Prelude.Text)
batchDeleteDetectorErrorEntry_messageId = Lens.lens (\BatchDeleteDetectorErrorEntry' {messageId} -> messageId) (\s@BatchDeleteDetectorErrorEntry' {} a -> s {messageId = a} :: BatchDeleteDetectorErrorEntry)

instance Data.FromJSON BatchDeleteDetectorErrorEntry where
  parseJSON =
    Data.withObject
      "BatchDeleteDetectorErrorEntry"
      ( \x ->
          BatchDeleteDetectorErrorEntry'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "messageId")
      )

instance
  Prelude.Hashable
    BatchDeleteDetectorErrorEntry
  where
  hashWithSalt _salt BatchDeleteDetectorErrorEntry' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` messageId

instance Prelude.NFData BatchDeleteDetectorErrorEntry where
  rnf BatchDeleteDetectorErrorEntry' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf messageId
