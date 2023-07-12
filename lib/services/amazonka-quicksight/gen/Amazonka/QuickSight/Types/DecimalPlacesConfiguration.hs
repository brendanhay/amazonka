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
-- Module      : Amazonka.QuickSight.Types.DecimalPlacesConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DecimalPlacesConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The option that determines the decimal places configuration.
--
-- /See:/ 'newDecimalPlacesConfiguration' smart constructor.
data DecimalPlacesConfiguration = DecimalPlacesConfiguration'
  { -- | The values of the decimal places.
    decimalPlaces :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecimalPlacesConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalPlaces', 'decimalPlacesConfiguration_decimalPlaces' - The values of the decimal places.
newDecimalPlacesConfiguration ::
  -- | 'decimalPlaces'
  Prelude.Natural ->
  DecimalPlacesConfiguration
newDecimalPlacesConfiguration pDecimalPlaces_ =
  DecimalPlacesConfiguration'
    { decimalPlaces =
        pDecimalPlaces_
    }

-- | The values of the decimal places.
decimalPlacesConfiguration_decimalPlaces :: Lens.Lens' DecimalPlacesConfiguration Prelude.Natural
decimalPlacesConfiguration_decimalPlaces = Lens.lens (\DecimalPlacesConfiguration' {decimalPlaces} -> decimalPlaces) (\s@DecimalPlacesConfiguration' {} a -> s {decimalPlaces = a} :: DecimalPlacesConfiguration)

instance Data.FromJSON DecimalPlacesConfiguration where
  parseJSON =
    Data.withObject
      "DecimalPlacesConfiguration"
      ( \x ->
          DecimalPlacesConfiguration'
            Prelude.<$> (x Data..: "DecimalPlaces")
      )

instance Prelude.Hashable DecimalPlacesConfiguration where
  hashWithSalt _salt DecimalPlacesConfiguration' {..} =
    _salt `Prelude.hashWithSalt` decimalPlaces

instance Prelude.NFData DecimalPlacesConfiguration where
  rnf DecimalPlacesConfiguration' {..} =
    Prelude.rnf decimalPlaces

instance Data.ToJSON DecimalPlacesConfiguration where
  toJSON DecimalPlacesConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DecimalPlaces" Data..= decimalPlaces)
          ]
      )
