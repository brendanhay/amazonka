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
-- Module      : Amazonka.VoiceId.Types.EnrollmentJobFraudDetectionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.EnrollmentJobFraudDetectionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.FraudDetectionAction

-- | The fraud detection configuration to be used during the batch speaker
-- enrollment job.
--
-- /See:/ 'newEnrollmentJobFraudDetectionConfig' smart constructor.
data EnrollmentJobFraudDetectionConfig = EnrollmentJobFraudDetectionConfig'
  { -- | The action to take when the given speaker is flagged by the fraud
    -- detection system. The default value is @FAIL@, which fails the speaker
    -- enrollment. Changing this value to @IGNORE@ results in the speaker being
    -- enrolled even if they are flagged by the fraud detection system.
    fraudDetectionAction :: Prelude.Maybe FraudDetectionAction,
    -- | Threshold value for determining whether the speaker is a high risk to be
    -- fraudulent. If the detected risk score calculated by Voice ID is greater
    -- than or equal to the threshold, the speaker is considered a fraudster.
    riskThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of watchlists against which fraud detection is performed.
    watchlistIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnrollmentJobFraudDetectionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fraudDetectionAction', 'enrollmentJobFraudDetectionConfig_fraudDetectionAction' - The action to take when the given speaker is flagged by the fraud
-- detection system. The default value is @FAIL@, which fails the speaker
-- enrollment. Changing this value to @IGNORE@ results in the speaker being
-- enrolled even if they are flagged by the fraud detection system.
--
-- 'riskThreshold', 'enrollmentJobFraudDetectionConfig_riskThreshold' - Threshold value for determining whether the speaker is a high risk to be
-- fraudulent. If the detected risk score calculated by Voice ID is greater
-- than or equal to the threshold, the speaker is considered a fraudster.
--
-- 'watchlistIds', 'enrollmentJobFraudDetectionConfig_watchlistIds' - The identifier of watchlists against which fraud detection is performed.
newEnrollmentJobFraudDetectionConfig ::
  EnrollmentJobFraudDetectionConfig
newEnrollmentJobFraudDetectionConfig =
  EnrollmentJobFraudDetectionConfig'
    { fraudDetectionAction =
        Prelude.Nothing,
      riskThreshold = Prelude.Nothing,
      watchlistIds = Prelude.Nothing
    }

-- | The action to take when the given speaker is flagged by the fraud
-- detection system. The default value is @FAIL@, which fails the speaker
-- enrollment. Changing this value to @IGNORE@ results in the speaker being
-- enrolled even if they are flagged by the fraud detection system.
enrollmentJobFraudDetectionConfig_fraudDetectionAction :: Lens.Lens' EnrollmentJobFraudDetectionConfig (Prelude.Maybe FraudDetectionAction)
enrollmentJobFraudDetectionConfig_fraudDetectionAction = Lens.lens (\EnrollmentJobFraudDetectionConfig' {fraudDetectionAction} -> fraudDetectionAction) (\s@EnrollmentJobFraudDetectionConfig' {} a -> s {fraudDetectionAction = a} :: EnrollmentJobFraudDetectionConfig)

-- | Threshold value for determining whether the speaker is a high risk to be
-- fraudulent. If the detected risk score calculated by Voice ID is greater
-- than or equal to the threshold, the speaker is considered a fraudster.
enrollmentJobFraudDetectionConfig_riskThreshold :: Lens.Lens' EnrollmentJobFraudDetectionConfig (Prelude.Maybe Prelude.Natural)
enrollmentJobFraudDetectionConfig_riskThreshold = Lens.lens (\EnrollmentJobFraudDetectionConfig' {riskThreshold} -> riskThreshold) (\s@EnrollmentJobFraudDetectionConfig' {} a -> s {riskThreshold = a} :: EnrollmentJobFraudDetectionConfig)

-- | The identifier of watchlists against which fraud detection is performed.
enrollmentJobFraudDetectionConfig_watchlistIds :: Lens.Lens' EnrollmentJobFraudDetectionConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
enrollmentJobFraudDetectionConfig_watchlistIds = Lens.lens (\EnrollmentJobFraudDetectionConfig' {watchlistIds} -> watchlistIds) (\s@EnrollmentJobFraudDetectionConfig' {} a -> s {watchlistIds = a} :: EnrollmentJobFraudDetectionConfig) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    EnrollmentJobFraudDetectionConfig
  where
  parseJSON =
    Data.withObject
      "EnrollmentJobFraudDetectionConfig"
      ( \x ->
          EnrollmentJobFraudDetectionConfig'
            Prelude.<$> (x Data..:? "FraudDetectionAction")
            Prelude.<*> (x Data..:? "RiskThreshold")
            Prelude.<*> (x Data..:? "WatchlistIds")
      )

instance
  Prelude.Hashable
    EnrollmentJobFraudDetectionConfig
  where
  hashWithSalt
    _salt
    EnrollmentJobFraudDetectionConfig' {..} =
      _salt
        `Prelude.hashWithSalt` fraudDetectionAction
        `Prelude.hashWithSalt` riskThreshold
        `Prelude.hashWithSalt` watchlistIds

instance
  Prelude.NFData
    EnrollmentJobFraudDetectionConfig
  where
  rnf EnrollmentJobFraudDetectionConfig' {..} =
    Prelude.rnf fraudDetectionAction
      `Prelude.seq` Prelude.rnf riskThreshold
      `Prelude.seq` Prelude.rnf watchlistIds

instance
  Data.ToJSON
    EnrollmentJobFraudDetectionConfig
  where
  toJSON EnrollmentJobFraudDetectionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FraudDetectionAction" Data..=)
              Prelude.<$> fraudDetectionAction,
            ("RiskThreshold" Data..=) Prelude.<$> riskThreshold,
            ("WatchlistIds" Data..=) Prelude.<$> watchlistIds
          ]
      )
