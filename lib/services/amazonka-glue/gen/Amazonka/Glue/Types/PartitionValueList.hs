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
-- Module      : Amazonka.Glue.Types.PartitionValueList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PartitionValueList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of values defining partitions.
--
-- /See:/ 'newPartitionValueList' smart constructor.
data PartitionValueList = PartitionValueList'
  { -- | The list of values.
    values :: [Prelude.Text]
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
-- 'values', 'partitionValueList_values' - The list of values.
newPartitionValueList ::
  PartitionValueList
newPartitionValueList =
  PartitionValueList' {values = Prelude.mempty}

-- | The list of values.
partitionValueList_values :: Lens.Lens' PartitionValueList [Prelude.Text]
partitionValueList_values = Lens.lens (\PartitionValueList' {values} -> values) (\s@PartitionValueList' {} a -> s {values = a} :: PartitionValueList) Prelude.. Lens.coerced

instance Core.FromJSON PartitionValueList where
  parseJSON =
    Core.withObject
      "PartitionValueList"
      ( \x ->
          PartitionValueList'
            Prelude.<$> (x Core..:? "Values" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable PartitionValueList where
  hashWithSalt _salt PartitionValueList' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData PartitionValueList where
  rnf PartitionValueList' {..} = Prelude.rnf values

instance Core.ToJSON PartitionValueList where
  toJSON PartitionValueList' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Values" Core..= values)]
      )
