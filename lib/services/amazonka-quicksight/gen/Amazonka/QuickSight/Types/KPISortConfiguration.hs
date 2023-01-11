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
-- Module      : Amazonka.QuickSight.Types.KPISortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPISortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions

-- | The sort configuration of a KPI visual.
--
-- /See:/ 'newKPISortConfiguration' smart constructor.
data KPISortConfiguration = KPISortConfiguration'
  { -- | The sort configuration of the trend group fields.
    trendGroupSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPISortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trendGroupSort', 'kPISortConfiguration_trendGroupSort' - The sort configuration of the trend group fields.
newKPISortConfiguration ::
  KPISortConfiguration
newKPISortConfiguration =
  KPISortConfiguration'
    { trendGroupSort =
        Prelude.Nothing
    }

-- | The sort configuration of the trend group fields.
kPISortConfiguration_trendGroupSort :: Lens.Lens' KPISortConfiguration (Prelude.Maybe [FieldSortOptions])
kPISortConfiguration_trendGroupSort = Lens.lens (\KPISortConfiguration' {trendGroupSort} -> trendGroupSort) (\s@KPISortConfiguration' {} a -> s {trendGroupSort = a} :: KPISortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON KPISortConfiguration where
  parseJSON =
    Data.withObject
      "KPISortConfiguration"
      ( \x ->
          KPISortConfiguration'
            Prelude.<$> ( x Data..:? "TrendGroupSort"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable KPISortConfiguration where
  hashWithSalt _salt KPISortConfiguration' {..} =
    _salt `Prelude.hashWithSalt` trendGroupSort

instance Prelude.NFData KPISortConfiguration where
  rnf KPISortConfiguration' {..} =
    Prelude.rnf trendGroupSort

instance Data.ToJSON KPISortConfiguration where
  toJSON KPISortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TrendGroupSort" Data..=)
              Prelude.<$> trendGroupSort
          ]
      )
