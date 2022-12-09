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
-- Module      : Amazonka.VoiceId.Types.EnrollmentConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.EnrollmentConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.EnrollmentJobFraudDetectionConfig
import Amazonka.VoiceId.Types.ExistingEnrollmentAction

-- | Contains configurations defining enrollment behavior for the batch job.
--
-- /See:/ 'newEnrollmentConfig' smart constructor.
data EnrollmentConfig = EnrollmentConfig'
  { -- | The action to take when the specified speaker is already enrolled in the
    -- specified domain. The default value is @SKIP@, which skips the
    -- enrollment for the existing speaker. Setting the value to @OVERWRITE@
    -- replaces the existing voice prints and enrollment audio stored for that
    -- speaker with new data generated from the latest audio.
    existingEnrollmentAction :: Prelude.Maybe ExistingEnrollmentAction,
    -- | The fraud detection configuration to use for the speaker enrollment job.
    fraudDetectionConfig :: Prelude.Maybe EnrollmentJobFraudDetectionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnrollmentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'existingEnrollmentAction', 'enrollmentConfig_existingEnrollmentAction' - The action to take when the specified speaker is already enrolled in the
-- specified domain. The default value is @SKIP@, which skips the
-- enrollment for the existing speaker. Setting the value to @OVERWRITE@
-- replaces the existing voice prints and enrollment audio stored for that
-- speaker with new data generated from the latest audio.
--
-- 'fraudDetectionConfig', 'enrollmentConfig_fraudDetectionConfig' - The fraud detection configuration to use for the speaker enrollment job.
newEnrollmentConfig ::
  EnrollmentConfig
newEnrollmentConfig =
  EnrollmentConfig'
    { existingEnrollmentAction =
        Prelude.Nothing,
      fraudDetectionConfig = Prelude.Nothing
    }

-- | The action to take when the specified speaker is already enrolled in the
-- specified domain. The default value is @SKIP@, which skips the
-- enrollment for the existing speaker. Setting the value to @OVERWRITE@
-- replaces the existing voice prints and enrollment audio stored for that
-- speaker with new data generated from the latest audio.
enrollmentConfig_existingEnrollmentAction :: Lens.Lens' EnrollmentConfig (Prelude.Maybe ExistingEnrollmentAction)
enrollmentConfig_existingEnrollmentAction = Lens.lens (\EnrollmentConfig' {existingEnrollmentAction} -> existingEnrollmentAction) (\s@EnrollmentConfig' {} a -> s {existingEnrollmentAction = a} :: EnrollmentConfig)

-- | The fraud detection configuration to use for the speaker enrollment job.
enrollmentConfig_fraudDetectionConfig :: Lens.Lens' EnrollmentConfig (Prelude.Maybe EnrollmentJobFraudDetectionConfig)
enrollmentConfig_fraudDetectionConfig = Lens.lens (\EnrollmentConfig' {fraudDetectionConfig} -> fraudDetectionConfig) (\s@EnrollmentConfig' {} a -> s {fraudDetectionConfig = a} :: EnrollmentConfig)

instance Data.FromJSON EnrollmentConfig where
  parseJSON =
    Data.withObject
      "EnrollmentConfig"
      ( \x ->
          EnrollmentConfig'
            Prelude.<$> (x Data..:? "ExistingEnrollmentAction")
            Prelude.<*> (x Data..:? "FraudDetectionConfig")
      )

instance Prelude.Hashable EnrollmentConfig where
  hashWithSalt _salt EnrollmentConfig' {..} =
    _salt
      `Prelude.hashWithSalt` existingEnrollmentAction
      `Prelude.hashWithSalt` fraudDetectionConfig

instance Prelude.NFData EnrollmentConfig where
  rnf EnrollmentConfig' {..} =
    Prelude.rnf existingEnrollmentAction
      `Prelude.seq` Prelude.rnf fraudDetectionConfig

instance Data.ToJSON EnrollmentConfig where
  toJSON EnrollmentConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExistingEnrollmentAction" Data..=)
              Prelude.<$> existingEnrollmentAction,
            ("FraudDetectionConfig" Data..=)
              Prelude.<$> fraudDetectionConfig
          ]
      )
