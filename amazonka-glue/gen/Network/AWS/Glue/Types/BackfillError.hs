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
-- Module      : Network.AWS.Glue.Types.BackfillError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BackfillError where

import Network.AWS.Glue.Types.BackfillErrorCode
import Network.AWS.Glue.Types.PartitionValueList
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | A list of a limited number of partitions in the response.
    partitions :: Prelude.Maybe [PartitionValueList],
    -- | The error code for an error that occurred when registering partition
    -- indexes for an existing table.
    code :: Prelude.Maybe BackfillErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BackfillError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitions', 'backfillError_partitions' - A list of a limited number of partitions in the response.
--
-- 'code', 'backfillError_code' - The error code for an error that occurred when registering partition
-- indexes for an existing table.
newBackfillError ::
  BackfillError
newBackfillError =
  BackfillError'
    { partitions = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A list of a limited number of partitions in the response.
backfillError_partitions :: Lens.Lens' BackfillError (Prelude.Maybe [PartitionValueList])
backfillError_partitions = Lens.lens (\BackfillError' {partitions} -> partitions) (\s@BackfillError' {} a -> s {partitions = a} :: BackfillError) Prelude.. Lens.mapping Prelude._Coerce

-- | The error code for an error that occurred when registering partition
-- indexes for an existing table.
backfillError_code :: Lens.Lens' BackfillError (Prelude.Maybe BackfillErrorCode)
backfillError_code = Lens.lens (\BackfillError' {code} -> code) (\s@BackfillError' {} a -> s {code = a} :: BackfillError)

instance Prelude.FromJSON BackfillError where
  parseJSON =
    Prelude.withObject
      "BackfillError"
      ( \x ->
          BackfillError'
            Prelude.<$> ( x Prelude..:? "Partitions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Code")
      )

instance Prelude.Hashable BackfillError

instance Prelude.NFData BackfillError
