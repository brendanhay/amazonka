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
-- Module      : Amazonka.Kinesis.Types.PutRecordsResultEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.PutRecordsResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the result of an individual record from a @PutRecords@
-- request. A record that is successfully added to a stream includes
-- @SequenceNumber@ and @ShardId@ in the result. A record that fails to be
-- added to the stream includes @ErrorCode@ and @ErrorMessage@ in the
-- result.
--
-- /See:/ 'newPutRecordsResultEntry' smart constructor.
data PutRecordsResultEntry = PutRecordsResultEntry'
  { -- | The error code for an individual record result. @ErrorCodes@ can be
    -- either @ProvisionedThroughputExceededException@ or @InternalFailure@.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message for an individual record result. An @ErrorCode@ value
    -- of @ProvisionedThroughputExceededException@ has an error message that
    -- includes the account ID, stream name, and shard ID. An @ErrorCode@ value
    -- of @InternalFailure@ has the error message
    -- @\"Internal Service Failure\"@.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The sequence number for an individual record result.
    sequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | The shard ID for an individual record result.
    shardId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecordsResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'putRecordsResultEntry_errorCode' - The error code for an individual record result. @ErrorCodes@ can be
-- either @ProvisionedThroughputExceededException@ or @InternalFailure@.
--
-- 'errorMessage', 'putRecordsResultEntry_errorMessage' - The error message for an individual record result. An @ErrorCode@ value
-- of @ProvisionedThroughputExceededException@ has an error message that
-- includes the account ID, stream name, and shard ID. An @ErrorCode@ value
-- of @InternalFailure@ has the error message
-- @\"Internal Service Failure\"@.
--
-- 'sequenceNumber', 'putRecordsResultEntry_sequenceNumber' - The sequence number for an individual record result.
--
-- 'shardId', 'putRecordsResultEntry_shardId' - The shard ID for an individual record result.
newPutRecordsResultEntry ::
  PutRecordsResultEntry
newPutRecordsResultEntry =
  PutRecordsResultEntry'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      sequenceNumber = Prelude.Nothing,
      shardId = Prelude.Nothing
    }

-- | The error code for an individual record result. @ErrorCodes@ can be
-- either @ProvisionedThroughputExceededException@ or @InternalFailure@.
putRecordsResultEntry_errorCode :: Lens.Lens' PutRecordsResultEntry (Prelude.Maybe Prelude.Text)
putRecordsResultEntry_errorCode = Lens.lens (\PutRecordsResultEntry' {errorCode} -> errorCode) (\s@PutRecordsResultEntry' {} a -> s {errorCode = a} :: PutRecordsResultEntry)

-- | The error message for an individual record result. An @ErrorCode@ value
-- of @ProvisionedThroughputExceededException@ has an error message that
-- includes the account ID, stream name, and shard ID. An @ErrorCode@ value
-- of @InternalFailure@ has the error message
-- @\"Internal Service Failure\"@.
putRecordsResultEntry_errorMessage :: Lens.Lens' PutRecordsResultEntry (Prelude.Maybe Prelude.Text)
putRecordsResultEntry_errorMessage = Lens.lens (\PutRecordsResultEntry' {errorMessage} -> errorMessage) (\s@PutRecordsResultEntry' {} a -> s {errorMessage = a} :: PutRecordsResultEntry)

-- | The sequence number for an individual record result.
putRecordsResultEntry_sequenceNumber :: Lens.Lens' PutRecordsResultEntry (Prelude.Maybe Prelude.Text)
putRecordsResultEntry_sequenceNumber = Lens.lens (\PutRecordsResultEntry' {sequenceNumber} -> sequenceNumber) (\s@PutRecordsResultEntry' {} a -> s {sequenceNumber = a} :: PutRecordsResultEntry)

-- | The shard ID for an individual record result.
putRecordsResultEntry_shardId :: Lens.Lens' PutRecordsResultEntry (Prelude.Maybe Prelude.Text)
putRecordsResultEntry_shardId = Lens.lens (\PutRecordsResultEntry' {shardId} -> shardId) (\s@PutRecordsResultEntry' {} a -> s {shardId = a} :: PutRecordsResultEntry)

instance Data.FromJSON PutRecordsResultEntry where
  parseJSON =
    Data.withObject
      "PutRecordsResultEntry"
      ( \x ->
          PutRecordsResultEntry'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "SequenceNumber")
            Prelude.<*> (x Data..:? "ShardId")
      )

instance Prelude.Hashable PutRecordsResultEntry where
  hashWithSalt _salt PutRecordsResultEntry' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` sequenceNumber
      `Prelude.hashWithSalt` shardId

instance Prelude.NFData PutRecordsResultEntry where
  rnf PutRecordsResultEntry' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf sequenceNumber
      `Prelude.seq` Prelude.rnf shardId
