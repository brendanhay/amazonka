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
-- Module      : Network.AWS.Personalize.Types.HPOConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Personalize.Types.HPOConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types.HPOObjective
import Network.AWS.Personalize.Types.HPOResourceConfig
import Network.AWS.Personalize.Types.HyperParameterRanges
import qualified Network.AWS.Prelude as Prelude

-- | Describes the properties for hyperparameter optimization (HPO).
--
-- /See:/ 'newHPOConfig' smart constructor.
data HPOConfig = HPOConfig'
  { -- | The hyperparameters and their allowable ranges.
    algorithmHyperParameterRanges :: Prelude.Maybe HyperParameterRanges,
    -- | Describes the resource configuration for HPO.
    hpoResourceConfig :: Prelude.Maybe HPOResourceConfig,
    -- | The metric to optimize during HPO.
    --
    -- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
    -- this time.
    hpoObjective :: Prelude.Maybe HPOObjective
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HPOConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmHyperParameterRanges', 'hPOConfig_algorithmHyperParameterRanges' - The hyperparameters and their allowable ranges.
--
-- 'hpoResourceConfig', 'hPOConfig_hpoResourceConfig' - Describes the resource configuration for HPO.
--
-- 'hpoObjective', 'hPOConfig_hpoObjective' - The metric to optimize during HPO.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
newHPOConfig ::
  HPOConfig
newHPOConfig =
  HPOConfig'
    { algorithmHyperParameterRanges =
        Prelude.Nothing,
      hpoResourceConfig = Prelude.Nothing,
      hpoObjective = Prelude.Nothing
    }

-- | The hyperparameters and their allowable ranges.
hPOConfig_algorithmHyperParameterRanges :: Lens.Lens' HPOConfig (Prelude.Maybe HyperParameterRanges)
hPOConfig_algorithmHyperParameterRanges = Lens.lens (\HPOConfig' {algorithmHyperParameterRanges} -> algorithmHyperParameterRanges) (\s@HPOConfig' {} a -> s {algorithmHyperParameterRanges = a} :: HPOConfig)

-- | Describes the resource configuration for HPO.
hPOConfig_hpoResourceConfig :: Lens.Lens' HPOConfig (Prelude.Maybe HPOResourceConfig)
hPOConfig_hpoResourceConfig = Lens.lens (\HPOConfig' {hpoResourceConfig} -> hpoResourceConfig) (\s@HPOConfig' {} a -> s {hpoResourceConfig = a} :: HPOConfig)

-- | The metric to optimize during HPO.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
hPOConfig_hpoObjective :: Lens.Lens' HPOConfig (Prelude.Maybe HPOObjective)
hPOConfig_hpoObjective = Lens.lens (\HPOConfig' {hpoObjective} -> hpoObjective) (\s@HPOConfig' {} a -> s {hpoObjective = a} :: HPOConfig)

instance Core.FromJSON HPOConfig where
  parseJSON =
    Core.withObject
      "HPOConfig"
      ( \x ->
          HPOConfig'
            Prelude.<$> (x Core..:? "algorithmHyperParameterRanges")
            Prelude.<*> (x Core..:? "hpoResourceConfig")
            Prelude.<*> (x Core..:? "hpoObjective")
      )

instance Prelude.Hashable HPOConfig

instance Prelude.NFData HPOConfig

instance Core.ToJSON HPOConfig where
  toJSON HPOConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("algorithmHyperParameterRanges" Core..=)
              Prelude.<$> algorithmHyperParameterRanges,
            ("hpoResourceConfig" Core..=)
              Prelude.<$> hpoResourceConfig,
            ("hpoObjective" Core..=) Prelude.<$> hpoObjective
          ]
      )
