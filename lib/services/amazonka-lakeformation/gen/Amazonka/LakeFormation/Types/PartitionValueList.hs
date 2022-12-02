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
-- Module      : Amazonka.LakeFormation.Types.PartitionValueList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.PartitionValueList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of values defining partitions.
--
-- /See:/ 'newPartitionValueList' smart constructor.
data PartitionValueList = PartitionValueList'
  { -- | The list of partition values.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartitionValueList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'partitionValueList_values' - The list of partition values.
newPartitionValueList ::
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  PartitionValueList
newPartitionValueList pValues_ =
  PartitionValueList'
    { values =
        Lens.coerced Lens.# pValues_
    }

-- | The list of partition values.
partitionValueList_values :: Lens.Lens' PartitionValueList (Prelude.NonEmpty Prelude.Text)
partitionValueList_values = Lens.lens (\PartitionValueList' {values} -> values) (\s@PartitionValueList' {} a -> s {values = a} :: PartitionValueList) Prelude.. Lens.coerced

instance Prelude.Hashable PartitionValueList where
  hashWithSalt _salt PartitionValueList' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData PartitionValueList where
  rnf PartitionValueList' {..} = Prelude.rnf values

instance Data.ToJSON PartitionValueList where
  toJSON PartitionValueList' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Values" Data..= values)]
      )
