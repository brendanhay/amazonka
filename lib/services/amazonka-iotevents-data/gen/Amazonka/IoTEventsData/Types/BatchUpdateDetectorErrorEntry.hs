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
-- Module      : Amazonka.IoTEventsData.Types.BatchUpdateDetectorErrorEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.BatchUpdateDetectorErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Information about the error that occurred when attempting to update a
-- detector.
--
-- /See:/ 'newBatchUpdateDetectorErrorEntry' smart constructor.
data BatchUpdateDetectorErrorEntry = BatchUpdateDetectorErrorEntry'
  { -- | A message that describes the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The @\"messageId\"@ of the update request that caused the error. (The
    -- value of the @\"messageId\"@ in the update request @\"Detector\"@
    -- object.)
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe ErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateDetectorErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchUpdateDetectorErrorEntry_errorMessage' - A message that describes the error.
--
-- 'messageId', 'batchUpdateDetectorErrorEntry_messageId' - The @\"messageId\"@ of the update request that caused the error. (The
-- value of the @\"messageId\"@ in the update request @\"Detector\"@
-- object.)
--
-- 'errorCode', 'batchUpdateDetectorErrorEntry_errorCode' - The error code.
newBatchUpdateDetectorErrorEntry ::
  BatchUpdateDetectorErrorEntry
newBatchUpdateDetectorErrorEntry =
  BatchUpdateDetectorErrorEntry'
    { errorMessage =
        Prelude.Nothing,
      messageId = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | A message that describes the error.
batchUpdateDetectorErrorEntry_errorMessage :: Lens.Lens' BatchUpdateDetectorErrorEntry (Prelude.Maybe Prelude.Text)
batchUpdateDetectorErrorEntry_errorMessage = Lens.lens (\BatchUpdateDetectorErrorEntry' {errorMessage} -> errorMessage) (\s@BatchUpdateDetectorErrorEntry' {} a -> s {errorMessage = a} :: BatchUpdateDetectorErrorEntry)

-- | The @\"messageId\"@ of the update request that caused the error. (The
-- value of the @\"messageId\"@ in the update request @\"Detector\"@
-- object.)
batchUpdateDetectorErrorEntry_messageId :: Lens.Lens' BatchUpdateDetectorErrorEntry (Prelude.Maybe Prelude.Text)
batchUpdateDetectorErrorEntry_messageId = Lens.lens (\BatchUpdateDetectorErrorEntry' {messageId} -> messageId) (\s@BatchUpdateDetectorErrorEntry' {} a -> s {messageId = a} :: BatchUpdateDetectorErrorEntry)

-- | The error code.
batchUpdateDetectorErrorEntry_errorCode :: Lens.Lens' BatchUpdateDetectorErrorEntry (Prelude.Maybe ErrorCode)
batchUpdateDetectorErrorEntry_errorCode = Lens.lens (\BatchUpdateDetectorErrorEntry' {errorCode} -> errorCode) (\s@BatchUpdateDetectorErrorEntry' {} a -> s {errorCode = a} :: BatchUpdateDetectorErrorEntry)

instance Core.FromJSON BatchUpdateDetectorErrorEntry where
  parseJSON =
    Core.withObject
      "BatchUpdateDetectorErrorEntry"
      ( \x ->
          BatchUpdateDetectorErrorEntry'
            Prelude.<$> (x Core..:? "errorMessage")
            Prelude.<*> (x Core..:? "messageId")
            Prelude.<*> (x Core..:? "errorCode")
      )

instance
  Prelude.Hashable
    BatchUpdateDetectorErrorEntry
  where
  hashWithSalt _salt BatchUpdateDetectorErrorEntry' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData BatchUpdateDetectorErrorEntry where
  rnf BatchUpdateDetectorErrorEntry' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf errorCode
