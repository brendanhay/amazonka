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
-- Module      : Amazonka.CloudFront.Types.ContinuousDeploymentSingleWeightConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ContinuousDeploymentSingleWeightConfig where

import Amazonka.CloudFront.Types.SessionStickinessConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the percentage of traffic to send to a staging distribution.
--
-- /See:/ 'newContinuousDeploymentSingleWeightConfig' smart constructor.
data ContinuousDeploymentSingleWeightConfig = ContinuousDeploymentSingleWeightConfig'
  { sessionStickinessConfig :: Prelude.Maybe SessionStickinessConfig,
    -- | The percentage of traffic to send to a staging distribution, expressed
    -- as a decimal number between 0 and .15.
    weight :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousDeploymentSingleWeightConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionStickinessConfig', 'continuousDeploymentSingleWeightConfig_sessionStickinessConfig' - Undocumented member.
--
-- 'weight', 'continuousDeploymentSingleWeightConfig_weight' - The percentage of traffic to send to a staging distribution, expressed
-- as a decimal number between 0 and .15.
newContinuousDeploymentSingleWeightConfig ::
  -- | 'weight'
  Prelude.Double ->
  ContinuousDeploymentSingleWeightConfig
newContinuousDeploymentSingleWeightConfig pWeight_ =
  ContinuousDeploymentSingleWeightConfig'
    { sessionStickinessConfig =
        Prelude.Nothing,
      weight = pWeight_
    }

-- | Undocumented member.
continuousDeploymentSingleWeightConfig_sessionStickinessConfig :: Lens.Lens' ContinuousDeploymentSingleWeightConfig (Prelude.Maybe SessionStickinessConfig)
continuousDeploymentSingleWeightConfig_sessionStickinessConfig = Lens.lens (\ContinuousDeploymentSingleWeightConfig' {sessionStickinessConfig} -> sessionStickinessConfig) (\s@ContinuousDeploymentSingleWeightConfig' {} a -> s {sessionStickinessConfig = a} :: ContinuousDeploymentSingleWeightConfig)

-- | The percentage of traffic to send to a staging distribution, expressed
-- as a decimal number between 0 and .15.
continuousDeploymentSingleWeightConfig_weight :: Lens.Lens' ContinuousDeploymentSingleWeightConfig Prelude.Double
continuousDeploymentSingleWeightConfig_weight = Lens.lens (\ContinuousDeploymentSingleWeightConfig' {weight} -> weight) (\s@ContinuousDeploymentSingleWeightConfig' {} a -> s {weight = a} :: ContinuousDeploymentSingleWeightConfig)

instance
  Data.FromXML
    ContinuousDeploymentSingleWeightConfig
  where
  parseXML x =
    ContinuousDeploymentSingleWeightConfig'
      Prelude.<$> (x Data..@? "SessionStickinessConfig")
      Prelude.<*> (x Data..@ "Weight")

instance
  Prelude.Hashable
    ContinuousDeploymentSingleWeightConfig
  where
  hashWithSalt
    _salt
    ContinuousDeploymentSingleWeightConfig' {..} =
      _salt
        `Prelude.hashWithSalt` sessionStickinessConfig
        `Prelude.hashWithSalt` weight

instance
  Prelude.NFData
    ContinuousDeploymentSingleWeightConfig
  where
  rnf ContinuousDeploymentSingleWeightConfig' {..} =
    Prelude.rnf sessionStickinessConfig
      `Prelude.seq` Prelude.rnf weight

instance
  Data.ToXML
    ContinuousDeploymentSingleWeightConfig
  where
  toXML ContinuousDeploymentSingleWeightConfig' {..} =
    Prelude.mconcat
      [ "SessionStickinessConfig"
          Data.@= sessionStickinessConfig,
        "Weight" Data.@= weight
      ]
