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
-- Module      : Amazonka.VoiceId.Types.FraudDetectionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudDetectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration used for performing fraud detection over a speaker
-- during a session.
--
-- /See:/ 'newFraudDetectionConfiguration' smart constructor.
data FraudDetectionConfiguration = FraudDetectionConfiguration'
  { -- | Threshold value for determining whether the speaker is a fraudster. If
    -- the detected risk score calculated by Voice ID is higher than the
    -- threshold, the speaker is considered a fraudster.
    riskThreshold :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FraudDetectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'riskThreshold', 'fraudDetectionConfiguration_riskThreshold' - Threshold value for determining whether the speaker is a fraudster. If
-- the detected risk score calculated by Voice ID is higher than the
-- threshold, the speaker is considered a fraudster.
newFraudDetectionConfiguration ::
  -- | 'riskThreshold'
  Prelude.Natural ->
  FraudDetectionConfiguration
newFraudDetectionConfiguration pRiskThreshold_ =
  FraudDetectionConfiguration'
    { riskThreshold =
        pRiskThreshold_
    }

-- | Threshold value for determining whether the speaker is a fraudster. If
-- the detected risk score calculated by Voice ID is higher than the
-- threshold, the speaker is considered a fraudster.
fraudDetectionConfiguration_riskThreshold :: Lens.Lens' FraudDetectionConfiguration Prelude.Natural
fraudDetectionConfiguration_riskThreshold = Lens.lens (\FraudDetectionConfiguration' {riskThreshold} -> riskThreshold) (\s@FraudDetectionConfiguration' {} a -> s {riskThreshold = a} :: FraudDetectionConfiguration)

instance Core.FromJSON FraudDetectionConfiguration where
  parseJSON =
    Core.withObject
      "FraudDetectionConfiguration"
      ( \x ->
          FraudDetectionConfiguration'
            Prelude.<$> (x Core..: "RiskThreshold")
      )

instance Prelude.Hashable FraudDetectionConfiguration where
  hashWithSalt _salt FraudDetectionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` riskThreshold

instance Prelude.NFData FraudDetectionConfiguration where
  rnf FraudDetectionConfiguration' {..} =
    Prelude.rnf riskThreshold
