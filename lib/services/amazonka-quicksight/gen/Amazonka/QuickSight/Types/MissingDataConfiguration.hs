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
-- Module      : Amazonka.QuickSight.Types.MissingDataConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MissingDataConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.MissingDataTreatmentOption

-- | The configuration options that determine how missing data is treated
-- during the rendering of a line chart.
--
-- /See:/ 'newMissingDataConfiguration' smart constructor.
data MissingDataConfiguration = MissingDataConfiguration'
  { -- | The treatment option that determines how missing data should be
    -- rendered. Choose from the following options:
    --
    -- -   @INTERPOLATE@: Interpolate missing values between the prior and the
    --     next known value.
    --
    -- -   @SHOW_AS_ZERO@: Show missing values as the value @0@.
    --
    -- -   @SHOW_AS_BLANK@: Display a blank space when rendering missing data.
    treatmentOption :: Prelude.Maybe MissingDataTreatmentOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MissingDataConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'treatmentOption', 'missingDataConfiguration_treatmentOption' - The treatment option that determines how missing data should be
-- rendered. Choose from the following options:
--
-- -   @INTERPOLATE@: Interpolate missing values between the prior and the
--     next known value.
--
-- -   @SHOW_AS_ZERO@: Show missing values as the value @0@.
--
-- -   @SHOW_AS_BLANK@: Display a blank space when rendering missing data.
newMissingDataConfiguration ::
  MissingDataConfiguration
newMissingDataConfiguration =
  MissingDataConfiguration'
    { treatmentOption =
        Prelude.Nothing
    }

-- | The treatment option that determines how missing data should be
-- rendered. Choose from the following options:
--
-- -   @INTERPOLATE@: Interpolate missing values between the prior and the
--     next known value.
--
-- -   @SHOW_AS_ZERO@: Show missing values as the value @0@.
--
-- -   @SHOW_AS_BLANK@: Display a blank space when rendering missing data.
missingDataConfiguration_treatmentOption :: Lens.Lens' MissingDataConfiguration (Prelude.Maybe MissingDataTreatmentOption)
missingDataConfiguration_treatmentOption = Lens.lens (\MissingDataConfiguration' {treatmentOption} -> treatmentOption) (\s@MissingDataConfiguration' {} a -> s {treatmentOption = a} :: MissingDataConfiguration)

instance Data.FromJSON MissingDataConfiguration where
  parseJSON =
    Data.withObject
      "MissingDataConfiguration"
      ( \x ->
          MissingDataConfiguration'
            Prelude.<$> (x Data..:? "TreatmentOption")
      )

instance Prelude.Hashable MissingDataConfiguration where
  hashWithSalt _salt MissingDataConfiguration' {..} =
    _salt `Prelude.hashWithSalt` treatmentOption

instance Prelude.NFData MissingDataConfiguration where
  rnf MissingDataConfiguration' {..} =
    Prelude.rnf treatmentOption

instance Data.ToJSON MissingDataConfiguration where
  toJSON MissingDataConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TreatmentOption" Data..=)
              Prelude.<$> treatmentOption
          ]
      )
