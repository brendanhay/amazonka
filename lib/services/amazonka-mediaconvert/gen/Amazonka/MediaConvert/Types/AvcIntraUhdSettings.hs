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
-- Module      : Amazonka.MediaConvert.Types.AvcIntraUhdSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AvcIntraUhdSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.AvcIntraUhdQualityTuningLevel
import qualified Amazonka.Prelude as Prelude

-- | Optional when you set AVC-Intra class (avcIntraClass) to Class 4K\/2K
-- (CLASS_4K_2K). When you set AVC-Intra class to a different value, this
-- object isn\'t allowed.
--
-- /See:/ 'newAvcIntraUhdSettings' smart constructor.
data AvcIntraUhdSettings = AvcIntraUhdSettings'
  { -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- many transcoding passes MediaConvert does with your video. When you
    -- choose Multi-pass (MULTI_PASS), your video quality is better and your
    -- output bitrate is more accurate. That is, the actual bitrate of your
    -- output is closer to the target bitrate defined in the specification.
    -- When you choose Single-pass (SINGLE_PASS), your encoding time is faster.
    -- The default behavior is Single-pass (SINGLE_PASS).
    qualityTuningLevel :: Prelude.Maybe AvcIntraUhdQualityTuningLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvcIntraUhdSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualityTuningLevel', 'avcIntraUhdSettings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- many transcoding passes MediaConvert does with your video. When you
-- choose Multi-pass (MULTI_PASS), your video quality is better and your
-- output bitrate is more accurate. That is, the actual bitrate of your
-- output is closer to the target bitrate defined in the specification.
-- When you choose Single-pass (SINGLE_PASS), your encoding time is faster.
-- The default behavior is Single-pass (SINGLE_PASS).
newAvcIntraUhdSettings ::
  AvcIntraUhdSettings
newAvcIntraUhdSettings =
  AvcIntraUhdSettings'
    { qualityTuningLevel =
        Prelude.Nothing
    }

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- many transcoding passes MediaConvert does with your video. When you
-- choose Multi-pass (MULTI_PASS), your video quality is better and your
-- output bitrate is more accurate. That is, the actual bitrate of your
-- output is closer to the target bitrate defined in the specification.
-- When you choose Single-pass (SINGLE_PASS), your encoding time is faster.
-- The default behavior is Single-pass (SINGLE_PASS).
avcIntraUhdSettings_qualityTuningLevel :: Lens.Lens' AvcIntraUhdSettings (Prelude.Maybe AvcIntraUhdQualityTuningLevel)
avcIntraUhdSettings_qualityTuningLevel = Lens.lens (\AvcIntraUhdSettings' {qualityTuningLevel} -> qualityTuningLevel) (\s@AvcIntraUhdSettings' {} a -> s {qualityTuningLevel = a} :: AvcIntraUhdSettings)

instance Core.FromJSON AvcIntraUhdSettings where
  parseJSON =
    Core.withObject
      "AvcIntraUhdSettings"
      ( \x ->
          AvcIntraUhdSettings'
            Prelude.<$> (x Core..:? "qualityTuningLevel")
      )

instance Prelude.Hashable AvcIntraUhdSettings where
  hashWithSalt _salt AvcIntraUhdSettings' {..} =
    _salt `Prelude.hashWithSalt` qualityTuningLevel

instance Prelude.NFData AvcIntraUhdSettings where
  rnf AvcIntraUhdSettings' {..} =
    Prelude.rnf qualityTuningLevel

instance Core.ToJSON AvcIntraUhdSettings where
  toJSON AvcIntraUhdSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("qualityTuningLevel" Core..=)
              Prelude.<$> qualityTuningLevel
          ]
      )
