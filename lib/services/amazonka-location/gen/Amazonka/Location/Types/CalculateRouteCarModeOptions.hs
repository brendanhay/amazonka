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
-- Module      : Amazonka.Location.Types.CalculateRouteCarModeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.CalculateRouteCarModeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about additional route preferences for requests that
-- specify @TravelMode@ as @Car@.
--
-- /See:/ 'newCalculateRouteCarModeOptions' smart constructor.
data CalculateRouteCarModeOptions = CalculateRouteCarModeOptions'
  { -- | Avoids ferries when calculating routes.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    avoidFerries :: Prelude.Maybe Prelude.Bool,
    -- | Avoids tolls when calculating routes.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    avoidTolls :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculateRouteCarModeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'avoidFerries', 'calculateRouteCarModeOptions_avoidFerries' - Avoids ferries when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
--
-- 'avoidTolls', 'calculateRouteCarModeOptions_avoidTolls' - Avoids tolls when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
newCalculateRouteCarModeOptions ::
  CalculateRouteCarModeOptions
newCalculateRouteCarModeOptions =
  CalculateRouteCarModeOptions'
    { avoidFerries =
        Prelude.Nothing,
      avoidTolls = Prelude.Nothing
    }

-- | Avoids ferries when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRouteCarModeOptions_avoidFerries :: Lens.Lens' CalculateRouteCarModeOptions (Prelude.Maybe Prelude.Bool)
calculateRouteCarModeOptions_avoidFerries = Lens.lens (\CalculateRouteCarModeOptions' {avoidFerries} -> avoidFerries) (\s@CalculateRouteCarModeOptions' {} a -> s {avoidFerries = a} :: CalculateRouteCarModeOptions)

-- | Avoids tolls when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRouteCarModeOptions_avoidTolls :: Lens.Lens' CalculateRouteCarModeOptions (Prelude.Maybe Prelude.Bool)
calculateRouteCarModeOptions_avoidTolls = Lens.lens (\CalculateRouteCarModeOptions' {avoidTolls} -> avoidTolls) (\s@CalculateRouteCarModeOptions' {} a -> s {avoidTolls = a} :: CalculateRouteCarModeOptions)

instance
  Prelude.Hashable
    CalculateRouteCarModeOptions
  where
  hashWithSalt _salt CalculateRouteCarModeOptions' {..} =
    _salt
      `Prelude.hashWithSalt` avoidFerries
      `Prelude.hashWithSalt` avoidTolls

instance Prelude.NFData CalculateRouteCarModeOptions where
  rnf CalculateRouteCarModeOptions' {..} =
    Prelude.rnf avoidFerries
      `Prelude.seq` Prelude.rnf avoidTolls

instance Data.ToJSON CalculateRouteCarModeOptions where
  toJSON CalculateRouteCarModeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvoidFerries" Data..=) Prelude.<$> avoidFerries,
            ("AvoidTolls" Data..=) Prelude.<$> avoidTolls
          ]
      )
