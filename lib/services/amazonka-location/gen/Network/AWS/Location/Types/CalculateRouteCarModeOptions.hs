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
-- Module      : Network.AWS.Location.Types.CalculateRouteCarModeOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Location.Types.CalculateRouteCarModeOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about additional route preferences for requests that
-- specify @TravelMode@ as @Car@.
--
-- /See:/ 'newCalculateRouteCarModeOptions' smart constructor.
data CalculateRouteCarModeOptions = CalculateRouteCarModeOptions'
  { -- | Avoids tolls when calculating routes.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    avoidTolls :: Prelude.Maybe Prelude.Bool,
    -- | Avoids ferries when calculating routes.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    avoidFerries :: Prelude.Maybe Prelude.Bool
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
-- 'avoidTolls', 'calculateRouteCarModeOptions_avoidTolls' - Avoids tolls when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
--
-- 'avoidFerries', 'calculateRouteCarModeOptions_avoidFerries' - Avoids ferries when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
newCalculateRouteCarModeOptions ::
  CalculateRouteCarModeOptions
newCalculateRouteCarModeOptions =
  CalculateRouteCarModeOptions'
    { avoidTolls =
        Prelude.Nothing,
      avoidFerries = Prelude.Nothing
    }

-- | Avoids tolls when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRouteCarModeOptions_avoidTolls :: Lens.Lens' CalculateRouteCarModeOptions (Prelude.Maybe Prelude.Bool)
calculateRouteCarModeOptions_avoidTolls = Lens.lens (\CalculateRouteCarModeOptions' {avoidTolls} -> avoidTolls) (\s@CalculateRouteCarModeOptions' {} a -> s {avoidTolls = a} :: CalculateRouteCarModeOptions)

-- | Avoids ferries when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRouteCarModeOptions_avoidFerries :: Lens.Lens' CalculateRouteCarModeOptions (Prelude.Maybe Prelude.Bool)
calculateRouteCarModeOptions_avoidFerries = Lens.lens (\CalculateRouteCarModeOptions' {avoidFerries} -> avoidFerries) (\s@CalculateRouteCarModeOptions' {} a -> s {avoidFerries = a} :: CalculateRouteCarModeOptions)

instance
  Prelude.Hashable
    CalculateRouteCarModeOptions

instance Prelude.NFData CalculateRouteCarModeOptions

instance Core.ToJSON CalculateRouteCarModeOptions where
  toJSON CalculateRouteCarModeOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AvoidTolls" Core..=) Prelude.<$> avoidTolls,
            ("AvoidFerries" Core..=) Prelude.<$> avoidFerries
          ]
      )
