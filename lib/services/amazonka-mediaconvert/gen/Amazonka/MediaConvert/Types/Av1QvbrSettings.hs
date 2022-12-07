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
-- Module      : Amazonka.MediaConvert.Types.Av1QvbrSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Av1QvbrSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for quality-defined variable bitrate encoding with the AV1
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
--
-- /See:/ 'newAv1QvbrSettings' smart constructor.
data Av1QvbrSettings = Av1QvbrSettings'
  { -- | Use this setting only when you set Rate control mode (RateControlMode)
    -- to QVBR. Specify the target quality level for this output. MediaConvert
    -- determines the right number of bits to use for each part of the video to
    -- maintain the video quality that you specify. When you keep the default
    -- value, AUTO, MediaConvert picks a quality level for you, based on
    -- characteristics of your input video. If you prefer to specify a quality
    -- level, specify a number from 1 through 10. Use higher numbers for
    -- greater quality. Level 10 results in nearly lossless compression. The
    -- quality level for most broadcast-quality transcodes is between 6 and 9.
    -- Optionally, to specify a value between whole numbers, also provide a
    -- value for the setting qvbrQualityLevelFineTune. For example, if you want
    -- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
    -- qvbrQualityLevelFineTune to .33.
    qvbrQualityLevel :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Specify a value here to set the QVBR quality to a level that
    -- is between whole numbers. For example, if you want your QVBR quality
    -- level to be 7.33, set qvbrQualityLevel to 7 and set
    -- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
    -- level to the nearest third of a whole number. For example, if you set
    -- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
    -- actual QVBR quality level is 7.33.
    qvbrQualityLevelFineTune :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Av1QvbrSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qvbrQualityLevel', 'av1QvbrSettings_qvbrQualityLevel' - Use this setting only when you set Rate control mode (RateControlMode)
-- to QVBR. Specify the target quality level for this output. MediaConvert
-- determines the right number of bits to use for each part of the video to
-- maintain the video quality that you specify. When you keep the default
-- value, AUTO, MediaConvert picks a quality level for you, based on
-- characteristics of your input video. If you prefer to specify a quality
-- level, specify a number from 1 through 10. Use higher numbers for
-- greater quality. Level 10 results in nearly lossless compression. The
-- quality level for most broadcast-quality transcodes is between 6 and 9.
-- Optionally, to specify a value between whole numbers, also provide a
-- value for the setting qvbrQualityLevelFineTune. For example, if you want
-- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33.
--
-- 'qvbrQualityLevelFineTune', 'av1QvbrSettings_qvbrQualityLevelFineTune' - Optional. Specify a value here to set the QVBR quality to a level that
-- is between whole numbers. For example, if you want your QVBR quality
-- level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
-- level to the nearest third of a whole number. For example, if you set
-- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
-- actual QVBR quality level is 7.33.
newAv1QvbrSettings ::
  Av1QvbrSettings
newAv1QvbrSettings =
  Av1QvbrSettings'
    { qvbrQualityLevel =
        Prelude.Nothing,
      qvbrQualityLevelFineTune = Prelude.Nothing
    }

-- | Use this setting only when you set Rate control mode (RateControlMode)
-- to QVBR. Specify the target quality level for this output. MediaConvert
-- determines the right number of bits to use for each part of the video to
-- maintain the video quality that you specify. When you keep the default
-- value, AUTO, MediaConvert picks a quality level for you, based on
-- characteristics of your input video. If you prefer to specify a quality
-- level, specify a number from 1 through 10. Use higher numbers for
-- greater quality. Level 10 results in nearly lossless compression. The
-- quality level for most broadcast-quality transcodes is between 6 and 9.
-- Optionally, to specify a value between whole numbers, also provide a
-- value for the setting qvbrQualityLevelFineTune. For example, if you want
-- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33.
av1QvbrSettings_qvbrQualityLevel :: Lens.Lens' Av1QvbrSettings (Prelude.Maybe Prelude.Natural)
av1QvbrSettings_qvbrQualityLevel = Lens.lens (\Av1QvbrSettings' {qvbrQualityLevel} -> qvbrQualityLevel) (\s@Av1QvbrSettings' {} a -> s {qvbrQualityLevel = a} :: Av1QvbrSettings)

-- | Optional. Specify a value here to set the QVBR quality to a level that
-- is between whole numbers. For example, if you want your QVBR quality
-- level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
-- level to the nearest third of a whole number. For example, if you set
-- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
-- actual QVBR quality level is 7.33.
av1QvbrSettings_qvbrQualityLevelFineTune :: Lens.Lens' Av1QvbrSettings (Prelude.Maybe Prelude.Double)
av1QvbrSettings_qvbrQualityLevelFineTune = Lens.lens (\Av1QvbrSettings' {qvbrQualityLevelFineTune} -> qvbrQualityLevelFineTune) (\s@Av1QvbrSettings' {} a -> s {qvbrQualityLevelFineTune = a} :: Av1QvbrSettings)

instance Data.FromJSON Av1QvbrSettings where
  parseJSON =
    Data.withObject
      "Av1QvbrSettings"
      ( \x ->
          Av1QvbrSettings'
            Prelude.<$> (x Data..:? "qvbrQualityLevel")
            Prelude.<*> (x Data..:? "qvbrQualityLevelFineTune")
      )

instance Prelude.Hashable Av1QvbrSettings where
  hashWithSalt _salt Av1QvbrSettings' {..} =
    _salt `Prelude.hashWithSalt` qvbrQualityLevel
      `Prelude.hashWithSalt` qvbrQualityLevelFineTune

instance Prelude.NFData Av1QvbrSettings where
  rnf Av1QvbrSettings' {..} =
    Prelude.rnf qvbrQualityLevel
      `Prelude.seq` Prelude.rnf qvbrQualityLevelFineTune

instance Data.ToJSON Av1QvbrSettings where
  toJSON Av1QvbrSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("qvbrQualityLevel" Data..=)
              Prelude.<$> qvbrQualityLevel,
            ("qvbrQualityLevelFineTune" Data..=)
              Prelude.<$> qvbrQualityLevelFineTune
          ]
      )
