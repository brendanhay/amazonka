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
-- Module      : Network.AWS.Glue.Types.PartitionValueList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionValueList where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains a list of values defining partitions.
--
-- /See:/ 'newPartitionValueList' smart constructor.
data PartitionValueList = PartitionValueList'
  { -- | The list of values.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
partitionValueList_values = Lens.lens (\PartitionValueList' {values} -> values) (\s@PartitionValueList' {} a -> s {values = a} :: PartitionValueList) Prelude.. Prelude._Coerce

instance Prelude.FromJSON PartitionValueList where
  parseJSON =
    Prelude.withObject
      "PartitionValueList"
      ( \x ->
          PartitionValueList'
            Prelude.<$> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable PartitionValueList

instance Prelude.NFData PartitionValueList

instance Prelude.ToJSON PartitionValueList where
  toJSON PartitionValueList' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Values" Prelude..= values)]
      )
