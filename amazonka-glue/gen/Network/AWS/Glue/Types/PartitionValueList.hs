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
-- Module      : Network.AWS.Glue.Types.PartitionValueList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionValueList where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains a list of values defining partitions.
--
-- /See:/ 'newPartitionValueList' smart constructor.
data PartitionValueList = PartitionValueList'
  { -- | The list of values.
    values :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  PartitionValueList' {values = Core.mempty}

-- | The list of values.
partitionValueList_values :: Lens.Lens' PartitionValueList [Core.Text]
partitionValueList_values = Lens.lens (\PartitionValueList' {values} -> values) (\s@PartitionValueList' {} a -> s {values = a} :: PartitionValueList) Core.. Lens._Coerce

instance Core.FromJSON PartitionValueList where
  parseJSON =
    Core.withObject
      "PartitionValueList"
      ( \x ->
          PartitionValueList'
            Core.<$> (x Core..:? "Values" Core..!= Core.mempty)
      )

instance Core.Hashable PartitionValueList

instance Core.NFData PartitionValueList

instance Core.ToJSON PartitionValueList where
  toJSON PartitionValueList' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Values" Core..= values)]
      )
