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
-- Module      : Amazonka.Firehose.Types.PutRecordBatchResponseEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.PutRecordBatchResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the result for an individual record from a PutRecordBatch
-- request. If the record is successfully added to your delivery stream, it
-- receives a record ID. If the record fails to be added to your delivery
-- stream, the result includes an error code and an error message.
--
-- /See:/ 'newPutRecordBatchResponseEntry' smart constructor.
data PutRecordBatchResponseEntry = PutRecordBatchResponseEntry'
  { -- | The ID of the record.
    recordId :: Prelude.Maybe Prelude.Text,
    -- | The error code for an individual record result.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message for an individual record result.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecordBatchResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordId', 'putRecordBatchResponseEntry_recordId' - The ID of the record.
--
-- 'errorCode', 'putRecordBatchResponseEntry_errorCode' - The error code for an individual record result.
--
-- 'errorMessage', 'putRecordBatchResponseEntry_errorMessage' - The error message for an individual record result.
newPutRecordBatchResponseEntry ::
  PutRecordBatchResponseEntry
newPutRecordBatchResponseEntry =
  PutRecordBatchResponseEntry'
    { recordId =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The ID of the record.
putRecordBatchResponseEntry_recordId :: Lens.Lens' PutRecordBatchResponseEntry (Prelude.Maybe Prelude.Text)
putRecordBatchResponseEntry_recordId = Lens.lens (\PutRecordBatchResponseEntry' {recordId} -> recordId) (\s@PutRecordBatchResponseEntry' {} a -> s {recordId = a} :: PutRecordBatchResponseEntry)

-- | The error code for an individual record result.
putRecordBatchResponseEntry_errorCode :: Lens.Lens' PutRecordBatchResponseEntry (Prelude.Maybe Prelude.Text)
putRecordBatchResponseEntry_errorCode = Lens.lens (\PutRecordBatchResponseEntry' {errorCode} -> errorCode) (\s@PutRecordBatchResponseEntry' {} a -> s {errorCode = a} :: PutRecordBatchResponseEntry)

-- | The error message for an individual record result.
putRecordBatchResponseEntry_errorMessage :: Lens.Lens' PutRecordBatchResponseEntry (Prelude.Maybe Prelude.Text)
putRecordBatchResponseEntry_errorMessage = Lens.lens (\PutRecordBatchResponseEntry' {errorMessage} -> errorMessage) (\s@PutRecordBatchResponseEntry' {} a -> s {errorMessage = a} :: PutRecordBatchResponseEntry)

instance Core.FromJSON PutRecordBatchResponseEntry where
  parseJSON =
    Core.withObject
      "PutRecordBatchResponseEntry"
      ( \x ->
          PutRecordBatchResponseEntry'
            Prelude.<$> (x Core..:? "RecordId")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable PutRecordBatchResponseEntry where
  hashWithSalt _salt PutRecordBatchResponseEntry' {..} =
    _salt `Prelude.hashWithSalt` recordId
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData PutRecordBatchResponseEntry where
  rnf PutRecordBatchResponseEntry' {..} =
    Prelude.rnf recordId
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
