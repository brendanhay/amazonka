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
-- Module      : Amazonka.Glue.Types.PartitionIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PartitionIndex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure for a partition index.
--
-- /See:/ 'newPartitionIndex' smart constructor.
data PartitionIndex = PartitionIndex'
  { -- | The keys for the partition index.
    keys :: Prelude.NonEmpty Prelude.Text,
    -- | The name of the partition index.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartitionIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keys', 'partitionIndex_keys' - The keys for the partition index.
--
-- 'indexName', 'partitionIndex_indexName' - The name of the partition index.
newPartitionIndex ::
  -- | 'keys'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  PartitionIndex
newPartitionIndex pKeys_ pIndexName_ =
  PartitionIndex'
    { keys = Lens.coerced Lens.# pKeys_,
      indexName = pIndexName_
    }

-- | The keys for the partition index.
partitionIndex_keys :: Lens.Lens' PartitionIndex (Prelude.NonEmpty Prelude.Text)
partitionIndex_keys = Lens.lens (\PartitionIndex' {keys} -> keys) (\s@PartitionIndex' {} a -> s {keys = a} :: PartitionIndex) Prelude.. Lens.coerced

-- | The name of the partition index.
partitionIndex_indexName :: Lens.Lens' PartitionIndex Prelude.Text
partitionIndex_indexName = Lens.lens (\PartitionIndex' {indexName} -> indexName) (\s@PartitionIndex' {} a -> s {indexName = a} :: PartitionIndex)

instance Prelude.Hashable PartitionIndex where
  hashWithSalt _salt PartitionIndex' {..} =
    _salt `Prelude.hashWithSalt` keys
      `Prelude.hashWithSalt` indexName

instance Prelude.NFData PartitionIndex where
  rnf PartitionIndex' {..} =
    Prelude.rnf keys
      `Prelude.seq` Prelude.rnf indexName

instance Data.ToJSON PartitionIndex where
  toJSON PartitionIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Keys" Data..= keys),
            Prelude.Just ("IndexName" Data..= indexName)
          ]
      )
