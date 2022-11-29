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
-- Module      : Amazonka.EMR.Types.ScalingAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ScalingAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.MarketType
import Amazonka.EMR.Types.SimpleScalingPolicyConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The type of adjustment the automatic scaling activity makes when
-- triggered, and the periodicity of the adjustment.
--
-- /See:/ 'newScalingAction' smart constructor.
data ScalingAction = ScalingAction'
  { -- | Not available for instance groups. Instance groups use the market type
    -- specified for the group.
    market :: Prelude.Maybe MarketType,
    -- | The type of adjustment the automatic scaling activity makes when
    -- triggered, and the periodicity of the adjustment.
    simpleScalingPolicyConfiguration :: SimpleScalingPolicyConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'market', 'scalingAction_market' - Not available for instance groups. Instance groups use the market type
-- specified for the group.
--
-- 'simpleScalingPolicyConfiguration', 'scalingAction_simpleScalingPolicyConfiguration' - The type of adjustment the automatic scaling activity makes when
-- triggered, and the periodicity of the adjustment.
newScalingAction ::
  -- | 'simpleScalingPolicyConfiguration'
  SimpleScalingPolicyConfiguration ->
  ScalingAction
newScalingAction pSimpleScalingPolicyConfiguration_ =
  ScalingAction'
    { market = Prelude.Nothing,
      simpleScalingPolicyConfiguration =
        pSimpleScalingPolicyConfiguration_
    }

-- | Not available for instance groups. Instance groups use the market type
-- specified for the group.
scalingAction_market :: Lens.Lens' ScalingAction (Prelude.Maybe MarketType)
scalingAction_market = Lens.lens (\ScalingAction' {market} -> market) (\s@ScalingAction' {} a -> s {market = a} :: ScalingAction)

-- | The type of adjustment the automatic scaling activity makes when
-- triggered, and the periodicity of the adjustment.
scalingAction_simpleScalingPolicyConfiguration :: Lens.Lens' ScalingAction SimpleScalingPolicyConfiguration
scalingAction_simpleScalingPolicyConfiguration = Lens.lens (\ScalingAction' {simpleScalingPolicyConfiguration} -> simpleScalingPolicyConfiguration) (\s@ScalingAction' {} a -> s {simpleScalingPolicyConfiguration = a} :: ScalingAction)

instance Core.FromJSON ScalingAction where
  parseJSON =
    Core.withObject
      "ScalingAction"
      ( \x ->
          ScalingAction'
            Prelude.<$> (x Core..:? "Market")
            Prelude.<*> (x Core..: "SimpleScalingPolicyConfiguration")
      )

instance Prelude.Hashable ScalingAction where
  hashWithSalt _salt ScalingAction' {..} =
    _salt `Prelude.hashWithSalt` market
      `Prelude.hashWithSalt` simpleScalingPolicyConfiguration

instance Prelude.NFData ScalingAction where
  rnf ScalingAction' {..} =
    Prelude.rnf market
      `Prelude.seq` Prelude.rnf simpleScalingPolicyConfiguration

instance Core.ToJSON ScalingAction where
  toJSON ScalingAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Market" Core..=) Prelude.<$> market,
            Prelude.Just
              ( "SimpleScalingPolicyConfiguration"
                  Core..= simpleScalingPolicyConfiguration
              )
          ]
      )
