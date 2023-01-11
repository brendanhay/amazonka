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
-- Module      : Amazonka.Personalize.Types.HPOConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.HPOConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.HPOObjective
import Amazonka.Personalize.Types.HPOResourceConfig
import Amazonka.Personalize.Types.HyperParameterRanges
import qualified Amazonka.Prelude as Prelude

-- | Describes the properties for hyperparameter optimization (HPO).
--
-- /See:/ 'newHPOConfig' smart constructor.
data HPOConfig = HPOConfig'
  { -- | The hyperparameters and their allowable ranges.
    algorithmHyperParameterRanges :: Prelude.Maybe HyperParameterRanges,
    -- | The metric to optimize during HPO.
    --
    -- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
    -- this time.
    hpoObjective :: Prelude.Maybe HPOObjective,
    -- | Describes the resource configuration for HPO.
    hpoResourceConfig :: Prelude.Maybe HPOResourceConfig
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
-- 'hpoObjective', 'hPOConfig_hpoObjective' - The metric to optimize during HPO.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
--
-- 'hpoResourceConfig', 'hPOConfig_hpoResourceConfig' - Describes the resource configuration for HPO.
newHPOConfig ::
  HPOConfig
newHPOConfig =
  HPOConfig'
    { algorithmHyperParameterRanges =
        Prelude.Nothing,
      hpoObjective = Prelude.Nothing,
      hpoResourceConfig = Prelude.Nothing
    }

-- | The hyperparameters and their allowable ranges.
hPOConfig_algorithmHyperParameterRanges :: Lens.Lens' HPOConfig (Prelude.Maybe HyperParameterRanges)
hPOConfig_algorithmHyperParameterRanges = Lens.lens (\HPOConfig' {algorithmHyperParameterRanges} -> algorithmHyperParameterRanges) (\s@HPOConfig' {} a -> s {algorithmHyperParameterRanges = a} :: HPOConfig)

-- | The metric to optimize during HPO.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
hPOConfig_hpoObjective :: Lens.Lens' HPOConfig (Prelude.Maybe HPOObjective)
hPOConfig_hpoObjective = Lens.lens (\HPOConfig' {hpoObjective} -> hpoObjective) (\s@HPOConfig' {} a -> s {hpoObjective = a} :: HPOConfig)

-- | Describes the resource configuration for HPO.
hPOConfig_hpoResourceConfig :: Lens.Lens' HPOConfig (Prelude.Maybe HPOResourceConfig)
hPOConfig_hpoResourceConfig = Lens.lens (\HPOConfig' {hpoResourceConfig} -> hpoResourceConfig) (\s@HPOConfig' {} a -> s {hpoResourceConfig = a} :: HPOConfig)

instance Data.FromJSON HPOConfig where
  parseJSON =
    Data.withObject
      "HPOConfig"
      ( \x ->
          HPOConfig'
            Prelude.<$> (x Data..:? "algorithmHyperParameterRanges")
            Prelude.<*> (x Data..:? "hpoObjective")
            Prelude.<*> (x Data..:? "hpoResourceConfig")
      )

instance Prelude.Hashable HPOConfig where
  hashWithSalt _salt HPOConfig' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmHyperParameterRanges
      `Prelude.hashWithSalt` hpoObjective
      `Prelude.hashWithSalt` hpoResourceConfig

instance Prelude.NFData HPOConfig where
  rnf HPOConfig' {..} =
    Prelude.rnf algorithmHyperParameterRanges
      `Prelude.seq` Prelude.rnf hpoObjective
      `Prelude.seq` Prelude.rnf hpoResourceConfig

instance Data.ToJSON HPOConfig where
  toJSON HPOConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("algorithmHyperParameterRanges" Data..=)
              Prelude.<$> algorithmHyperParameterRanges,
            ("hpoObjective" Data..=) Prelude.<$> hpoObjective,
            ("hpoResourceConfig" Data..=)
              Prelude.<$> hpoResourceConfig
          ]
      )
