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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudDetectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration used for performing fraud detection over a speaker
-- during a session.
--
-- /See:/ 'newFraudDetectionConfiguration' smart constructor.
data FraudDetectionConfiguration = FraudDetectionConfiguration'
  { -- | Threshold value for determining whether the speaker is a fraudster. If
    -- the detected risk score calculated by Voice ID is higher than the
    -- threshold, the speaker is considered a fraudster.
    riskThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the watchlist against which fraud detection is
    -- performed.
    watchlistId :: Prelude.Maybe Prelude.Text
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
--
-- 'watchlistId', 'fraudDetectionConfiguration_watchlistId' - The identifier of the watchlist against which fraud detection is
-- performed.
newFraudDetectionConfiguration ::
  FraudDetectionConfiguration
newFraudDetectionConfiguration =
  FraudDetectionConfiguration'
    { riskThreshold =
        Prelude.Nothing,
      watchlistId = Prelude.Nothing
    }

-- | Threshold value for determining whether the speaker is a fraudster. If
-- the detected risk score calculated by Voice ID is higher than the
-- threshold, the speaker is considered a fraudster.
fraudDetectionConfiguration_riskThreshold :: Lens.Lens' FraudDetectionConfiguration (Prelude.Maybe Prelude.Natural)
fraudDetectionConfiguration_riskThreshold = Lens.lens (\FraudDetectionConfiguration' {riskThreshold} -> riskThreshold) (\s@FraudDetectionConfiguration' {} a -> s {riskThreshold = a} :: FraudDetectionConfiguration)

-- | The identifier of the watchlist against which fraud detection is
-- performed.
fraudDetectionConfiguration_watchlistId :: Lens.Lens' FraudDetectionConfiguration (Prelude.Maybe Prelude.Text)
fraudDetectionConfiguration_watchlistId = Lens.lens (\FraudDetectionConfiguration' {watchlistId} -> watchlistId) (\s@FraudDetectionConfiguration' {} a -> s {watchlistId = a} :: FraudDetectionConfiguration)

instance Data.FromJSON FraudDetectionConfiguration where
  parseJSON =
    Data.withObject
      "FraudDetectionConfiguration"
      ( \x ->
          FraudDetectionConfiguration'
            Prelude.<$> (x Data..:? "RiskThreshold")
            Prelude.<*> (x Data..:? "WatchlistId")
      )

instance Prelude.Hashable FraudDetectionConfiguration where
  hashWithSalt _salt FraudDetectionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` riskThreshold
      `Prelude.hashWithSalt` watchlistId

instance Prelude.NFData FraudDetectionConfiguration where
  rnf FraudDetectionConfiguration' {..} =
    Prelude.rnf riskThreshold
      `Prelude.seq` Prelude.rnf watchlistId
