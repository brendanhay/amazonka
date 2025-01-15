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
-- Module      : Amazonka.QuickSight.Types.KPIConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPIConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.KPIFieldWells
import Amazonka.QuickSight.Types.KPIOptions
import Amazonka.QuickSight.Types.KPISortConfiguration

-- | The configuration of a KPI visual.
--
-- /See:/ 'newKPIConfiguration' smart constructor.
data KPIConfiguration = KPIConfiguration'
  { -- | The field well configuration of a KPI visual.
    fieldWells :: Prelude.Maybe KPIFieldWells,
    -- | The options that determine the presentation of a KPI visual.
    kPIOptions :: Prelude.Maybe KPIOptions,
    -- | The sort configuration of a KPI visual.
    sortConfiguration :: Prelude.Maybe KPISortConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPIConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldWells', 'kPIConfiguration_fieldWells' - The field well configuration of a KPI visual.
--
-- 'kPIOptions', 'kPIConfiguration_kPIOptions' - The options that determine the presentation of a KPI visual.
--
-- 'sortConfiguration', 'kPIConfiguration_sortConfiguration' - The sort configuration of a KPI visual.
newKPIConfiguration ::
  KPIConfiguration
newKPIConfiguration =
  KPIConfiguration'
    { fieldWells = Prelude.Nothing,
      kPIOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing
    }

-- | The field well configuration of a KPI visual.
kPIConfiguration_fieldWells :: Lens.Lens' KPIConfiguration (Prelude.Maybe KPIFieldWells)
kPIConfiguration_fieldWells = Lens.lens (\KPIConfiguration' {fieldWells} -> fieldWells) (\s@KPIConfiguration' {} a -> s {fieldWells = a} :: KPIConfiguration)

-- | The options that determine the presentation of a KPI visual.
kPIConfiguration_kPIOptions :: Lens.Lens' KPIConfiguration (Prelude.Maybe KPIOptions)
kPIConfiguration_kPIOptions = Lens.lens (\KPIConfiguration' {kPIOptions} -> kPIOptions) (\s@KPIConfiguration' {} a -> s {kPIOptions = a} :: KPIConfiguration)

-- | The sort configuration of a KPI visual.
kPIConfiguration_sortConfiguration :: Lens.Lens' KPIConfiguration (Prelude.Maybe KPISortConfiguration)
kPIConfiguration_sortConfiguration = Lens.lens (\KPIConfiguration' {sortConfiguration} -> sortConfiguration) (\s@KPIConfiguration' {} a -> s {sortConfiguration = a} :: KPIConfiguration)

instance Data.FromJSON KPIConfiguration where
  parseJSON =
    Data.withObject
      "KPIConfiguration"
      ( \x ->
          KPIConfiguration'
            Prelude.<$> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "KPIOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
      )

instance Prelude.Hashable KPIConfiguration where
  hashWithSalt _salt KPIConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` kPIOptions
      `Prelude.hashWithSalt` sortConfiguration

instance Prelude.NFData KPIConfiguration where
  rnf KPIConfiguration' {..} =
    Prelude.rnf fieldWells `Prelude.seq`
      Prelude.rnf kPIOptions `Prelude.seq`
        Prelude.rnf sortConfiguration

instance Data.ToJSON KPIConfiguration where
  toJSON KPIConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("KPIOptions" Data..=) Prelude.<$> kPIOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration
          ]
      )
