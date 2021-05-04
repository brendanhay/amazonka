{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.PutRecordBatchResponseEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the result for an individual record from a PutRecordBatch
-- request. If the record is successfully added to your delivery stream, it
-- receives a record ID. If the record fails to be added to your delivery
-- stream, the result includes an error code and an error message.
--
-- /See:/ 'newPutRecordBatchResponseEntry' smart constructor.
data PutRecordBatchResponseEntry = PutRecordBatchResponseEntry'
  { -- | The ID of the record.
    recordId :: Prelude.Maybe Prelude.Text,
    -- | The error message for an individual record result.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code for an individual record result.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'errorMessage', 'putRecordBatchResponseEntry_errorMessage' - The error message for an individual record result.
--
-- 'errorCode', 'putRecordBatchResponseEntry_errorCode' - The error code for an individual record result.
newPutRecordBatchResponseEntry ::
  PutRecordBatchResponseEntry
newPutRecordBatchResponseEntry =
  PutRecordBatchResponseEntry'
    { recordId =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The ID of the record.
putRecordBatchResponseEntry_recordId :: Lens.Lens' PutRecordBatchResponseEntry (Prelude.Maybe Prelude.Text)
putRecordBatchResponseEntry_recordId = Lens.lens (\PutRecordBatchResponseEntry' {recordId} -> recordId) (\s@PutRecordBatchResponseEntry' {} a -> s {recordId = a} :: PutRecordBatchResponseEntry)

-- | The error message for an individual record result.
putRecordBatchResponseEntry_errorMessage :: Lens.Lens' PutRecordBatchResponseEntry (Prelude.Maybe Prelude.Text)
putRecordBatchResponseEntry_errorMessage = Lens.lens (\PutRecordBatchResponseEntry' {errorMessage} -> errorMessage) (\s@PutRecordBatchResponseEntry' {} a -> s {errorMessage = a} :: PutRecordBatchResponseEntry)

-- | The error code for an individual record result.
putRecordBatchResponseEntry_errorCode :: Lens.Lens' PutRecordBatchResponseEntry (Prelude.Maybe Prelude.Text)
putRecordBatchResponseEntry_errorCode = Lens.lens (\PutRecordBatchResponseEntry' {errorCode} -> errorCode) (\s@PutRecordBatchResponseEntry' {} a -> s {errorCode = a} :: PutRecordBatchResponseEntry)

instance Prelude.FromJSON PutRecordBatchResponseEntry where
  parseJSON =
    Prelude.withObject
      "PutRecordBatchResponseEntry"
      ( \x ->
          PutRecordBatchResponseEntry'
            Prelude.<$> (x Prelude..:? "RecordId")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable PutRecordBatchResponseEntry

instance Prelude.NFData PutRecordBatchResponseEntry
