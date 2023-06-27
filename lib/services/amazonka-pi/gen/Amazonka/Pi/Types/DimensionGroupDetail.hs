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
-- Module      : Amazonka.Pi.Types.DimensionGroupDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.DimensionGroupDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pi.Types.DimensionDetail
import qualified Amazonka.Prelude as Prelude

-- | Information about dimensions within a dimension group.
--
-- /See:/ 'newDimensionGroupDetail' smart constructor.
data DimensionGroupDetail = DimensionGroupDetail'
  { -- | The dimensions within a dimension group.
    dimensions :: Prelude.Maybe [DimensionDetail],
    -- | The name of the dimension group.
    group' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionGroupDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'dimensionGroupDetail_dimensions' - The dimensions within a dimension group.
--
-- 'group'', 'dimensionGroupDetail_group' - The name of the dimension group.
newDimensionGroupDetail ::
  DimensionGroupDetail
newDimensionGroupDetail =
  DimensionGroupDetail'
    { dimensions = Prelude.Nothing,
      group' = Prelude.Nothing
    }

-- | The dimensions within a dimension group.
dimensionGroupDetail_dimensions :: Lens.Lens' DimensionGroupDetail (Prelude.Maybe [DimensionDetail])
dimensionGroupDetail_dimensions = Lens.lens (\DimensionGroupDetail' {dimensions} -> dimensions) (\s@DimensionGroupDetail' {} a -> s {dimensions = a} :: DimensionGroupDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dimension group.
dimensionGroupDetail_group :: Lens.Lens' DimensionGroupDetail (Prelude.Maybe Prelude.Text)
dimensionGroupDetail_group = Lens.lens (\DimensionGroupDetail' {group'} -> group') (\s@DimensionGroupDetail' {} a -> s {group' = a} :: DimensionGroupDetail)

instance Data.FromJSON DimensionGroupDetail where
  parseJSON =
    Data.withObject
      "DimensionGroupDetail"
      ( \x ->
          DimensionGroupDetail'
            Prelude.<$> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Group")
      )

instance Prelude.Hashable DimensionGroupDetail where
  hashWithSalt _salt DimensionGroupDetail' {..} =
    _salt
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` group'

instance Prelude.NFData DimensionGroupDetail where
  rnf DimensionGroupDetail' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf group'
