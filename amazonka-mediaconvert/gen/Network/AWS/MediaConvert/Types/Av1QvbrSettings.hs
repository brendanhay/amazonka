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
-- Module      : Network.AWS.MediaConvert.Types.Av1QvbrSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1QvbrSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for quality-defined variable bitrate encoding with the AV1
-- codec. Required when you set Rate control mode to QVBR. Not valid when
-- you set Rate control mode to a value other than QVBR, or when you don\'t
-- define Rate control mode.
--
-- /See:/ 'newAv1QvbrSettings' smart constructor.
data Av1QvbrSettings = Av1QvbrSettings'
  { -- | Optional. Specify a value here to set the QVBR quality to a level that
    -- is between whole numbers. For example, if you want your QVBR quality
    -- level to be 7.33, set qvbrQualityLevel to 7 and set
    -- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
    -- level to the nearest third of a whole number. For example, if you set
    -- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
    -- actual QVBR quality level is 7.33.
    qvbrQualityLevelFineTune :: Prelude.Maybe Prelude.Double,
    -- | Required when you use QVBR rate control mode. That is, when you specify
    -- qvbrSettings within av1Settings. Specify the general target quality
    -- level for this output, from 1 to 10. Use higher numbers for greater
    -- quality. Level 10 results in nearly lossless compression. The quality
    -- level for most broadcast-quality transcodes is between 6 and 9.
    -- Optionally, to specify a value between whole numbers, also provide a
    -- value for the setting qvbrQualityLevelFineTune. For example, if you want
    -- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
    -- qvbrQualityLevelFineTune to .33.
    qvbrQualityLevel :: Prelude.Maybe Prelude.Natural
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
-- 'qvbrQualityLevelFineTune', 'av1QvbrSettings_qvbrQualityLevelFineTune' - Optional. Specify a value here to set the QVBR quality to a level that
-- is between whole numbers. For example, if you want your QVBR quality
-- level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
-- level to the nearest third of a whole number. For example, if you set
-- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
-- actual QVBR quality level is 7.33.
--
-- 'qvbrQualityLevel', 'av1QvbrSettings_qvbrQualityLevel' - Required when you use QVBR rate control mode. That is, when you specify
-- qvbrSettings within av1Settings. Specify the general target quality
-- level for this output, from 1 to 10. Use higher numbers for greater
-- quality. Level 10 results in nearly lossless compression. The quality
-- level for most broadcast-quality transcodes is between 6 and 9.
-- Optionally, to specify a value between whole numbers, also provide a
-- value for the setting qvbrQualityLevelFineTune. For example, if you want
-- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33.
newAv1QvbrSettings ::
  Av1QvbrSettings
newAv1QvbrSettings =
  Av1QvbrSettings'
    { qvbrQualityLevelFineTune =
        Prelude.Nothing,
      qvbrQualityLevel = Prelude.Nothing
    }

-- | Optional. Specify a value here to set the QVBR quality to a level that
-- is between whole numbers. For example, if you want your QVBR quality
-- level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
-- level to the nearest third of a whole number. For example, if you set
-- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
-- actual QVBR quality level is 7.33.
av1QvbrSettings_qvbrQualityLevelFineTune :: Lens.Lens' Av1QvbrSettings (Prelude.Maybe Prelude.Double)
av1QvbrSettings_qvbrQualityLevelFineTune = Lens.lens (\Av1QvbrSettings' {qvbrQualityLevelFineTune} -> qvbrQualityLevelFineTune) (\s@Av1QvbrSettings' {} a -> s {qvbrQualityLevelFineTune = a} :: Av1QvbrSettings)

-- | Required when you use QVBR rate control mode. That is, when you specify
-- qvbrSettings within av1Settings. Specify the general target quality
-- level for this output, from 1 to 10. Use higher numbers for greater
-- quality. Level 10 results in nearly lossless compression. The quality
-- level for most broadcast-quality transcodes is between 6 and 9.
-- Optionally, to specify a value between whole numbers, also provide a
-- value for the setting qvbrQualityLevelFineTune. For example, if you want
-- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33.
av1QvbrSettings_qvbrQualityLevel :: Lens.Lens' Av1QvbrSettings (Prelude.Maybe Prelude.Natural)
av1QvbrSettings_qvbrQualityLevel = Lens.lens (\Av1QvbrSettings' {qvbrQualityLevel} -> qvbrQualityLevel) (\s@Av1QvbrSettings' {} a -> s {qvbrQualityLevel = a} :: Av1QvbrSettings)

instance Core.FromJSON Av1QvbrSettings where
  parseJSON =
    Core.withObject
      "Av1QvbrSettings"
      ( \x ->
          Av1QvbrSettings'
            Prelude.<$> (x Core..:? "qvbrQualityLevelFineTune")
            Prelude.<*> (x Core..:? "qvbrQualityLevel")
      )

instance Prelude.Hashable Av1QvbrSettings

instance Prelude.NFData Av1QvbrSettings

instance Core.ToJSON Av1QvbrSettings where
  toJSON Av1QvbrSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("qvbrQualityLevelFineTune" Core..=)
              Prelude.<$> qvbrQualityLevelFineTune,
            ("qvbrQualityLevel" Core..=)
              Prelude.<$> qvbrQualityLevel
          ]
      )
