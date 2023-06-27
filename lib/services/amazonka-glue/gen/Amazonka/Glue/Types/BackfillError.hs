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
-- Module      : Amazonka.Glue.Types.BackfillError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BackfillError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.BackfillErrorCode
import Amazonka.Glue.Types.PartitionValueList
import qualified Amazonka.Prelude as Prelude

-- | A list of errors that can occur when registering partition indexes for
-- an existing table.
--
-- These errors give the details about why an index registration failed and
-- provide a limited number of partitions in the response, so that you can
-- fix the partitions at fault and try registering the index again. The
-- most common set of errors that can occur are categorized as follows:
--
-- -   EncryptedPartitionError: The partitions are encrypted.
--
-- -   InvalidPartitionTypeDataError: The partition value doesn\'t match
--     the data type for that partition column.
--
-- -   MissingPartitionValueError: The partitions are encrypted.
--
-- -   UnsupportedPartitionCharacterError: Characters inside the partition
--     value are not supported. For example: U+0000 , U+0001, U+0002.
--
-- -   InternalError: Any error which does not belong to other error codes.
--
-- /See:/ 'newBackfillError' smart constructor.
data BackfillError = BackfillError'
  { -- | The error code for an error that occurred when registering partition
    -- indexes for an existing table.
    code :: Prelude.Maybe BackfillErrorCode,
    -- | A list of a limited number of partitions in the response.
    partitions :: Prelude.Maybe [PartitionValueList]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackfillError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'backfillError_code' - The error code for an error that occurred when registering partition
-- indexes for an existing table.
--
-- 'partitions', 'backfillError_partitions' - A list of a limited number of partitions in the response.
newBackfillError ::
  BackfillError
newBackfillError =
  BackfillError'
    { code = Prelude.Nothing,
      partitions = Prelude.Nothing
    }

-- | The error code for an error that occurred when registering partition
-- indexes for an existing table.
backfillError_code :: Lens.Lens' BackfillError (Prelude.Maybe BackfillErrorCode)
backfillError_code = Lens.lens (\BackfillError' {code} -> code) (\s@BackfillError' {} a -> s {code = a} :: BackfillError)

-- | A list of a limited number of partitions in the response.
backfillError_partitions :: Lens.Lens' BackfillError (Prelude.Maybe [PartitionValueList])
backfillError_partitions = Lens.lens (\BackfillError' {partitions} -> partitions) (\s@BackfillError' {} a -> s {partitions = a} :: BackfillError) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BackfillError where
  parseJSON =
    Data.withObject
      "BackfillError"
      ( \x ->
          BackfillError'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Partitions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable BackfillError where
  hashWithSalt _salt BackfillError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` partitions

instance Prelude.NFData BackfillError where
  rnf BackfillError' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf partitions
