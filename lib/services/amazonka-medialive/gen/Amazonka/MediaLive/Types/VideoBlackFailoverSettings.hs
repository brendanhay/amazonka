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
-- Module      : Amazonka.MediaLive.Types.VideoBlackFailoverSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoBlackFailoverSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for VideoBlackFailoverSettings
--
-- /See:/ 'newVideoBlackFailoverSettings' smart constructor.
data VideoBlackFailoverSettings = VideoBlackFailoverSettings'
  { -- | A value used in calculating the threshold below which MediaLive
    -- considers a pixel to be \'black\'. For the input to be considered black,
    -- every pixel in a frame must be below this threshold. The threshold is
    -- calculated as a percentage (expressed as a decimal) of white. Therefore
    -- .1 means 10% white (or 90% black). Note how the formula works for any
    -- color depth. For example, if you set this field to 0.1 in 10-bit color
    -- depth: (1023*0.1=102.3), which means a pixel value of 102 or less is
    -- \'black\'. If you set this field to .1 in an 8-bit color depth:
    -- (255*0.1=25.5), which means a pixel value of 25 or less is \'black\'.
    -- The range is 0.0 to 1.0, with any number of decimal places.
    blackDetectThreshold :: Prelude.Maybe Prelude.Double,
    -- | The amount of time (in milliseconds) that the active input must be black
    -- before automatic input failover occurs.
    videoBlackThresholdMsec :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoBlackFailoverSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blackDetectThreshold', 'videoBlackFailoverSettings_blackDetectThreshold' - A value used in calculating the threshold below which MediaLive
-- considers a pixel to be \'black\'. For the input to be considered black,
-- every pixel in a frame must be below this threshold. The threshold is
-- calculated as a percentage (expressed as a decimal) of white. Therefore
-- .1 means 10% white (or 90% black). Note how the formula works for any
-- color depth. For example, if you set this field to 0.1 in 10-bit color
-- depth: (1023*0.1=102.3), which means a pixel value of 102 or less is
-- \'black\'. If you set this field to .1 in an 8-bit color depth:
-- (255*0.1=25.5), which means a pixel value of 25 or less is \'black\'.
-- The range is 0.0 to 1.0, with any number of decimal places.
--
-- 'videoBlackThresholdMsec', 'videoBlackFailoverSettings_videoBlackThresholdMsec' - The amount of time (in milliseconds) that the active input must be black
-- before automatic input failover occurs.
newVideoBlackFailoverSettings ::
  VideoBlackFailoverSettings
newVideoBlackFailoverSettings =
  VideoBlackFailoverSettings'
    { blackDetectThreshold =
        Prelude.Nothing,
      videoBlackThresholdMsec = Prelude.Nothing
    }

-- | A value used in calculating the threshold below which MediaLive
-- considers a pixel to be \'black\'. For the input to be considered black,
-- every pixel in a frame must be below this threshold. The threshold is
-- calculated as a percentage (expressed as a decimal) of white. Therefore
-- .1 means 10% white (or 90% black). Note how the formula works for any
-- color depth. For example, if you set this field to 0.1 in 10-bit color
-- depth: (1023*0.1=102.3), which means a pixel value of 102 or less is
-- \'black\'. If you set this field to .1 in an 8-bit color depth:
-- (255*0.1=25.5), which means a pixel value of 25 or less is \'black\'.
-- The range is 0.0 to 1.0, with any number of decimal places.
videoBlackFailoverSettings_blackDetectThreshold :: Lens.Lens' VideoBlackFailoverSettings (Prelude.Maybe Prelude.Double)
videoBlackFailoverSettings_blackDetectThreshold = Lens.lens (\VideoBlackFailoverSettings' {blackDetectThreshold} -> blackDetectThreshold) (\s@VideoBlackFailoverSettings' {} a -> s {blackDetectThreshold = a} :: VideoBlackFailoverSettings)

-- | The amount of time (in milliseconds) that the active input must be black
-- before automatic input failover occurs.
videoBlackFailoverSettings_videoBlackThresholdMsec :: Lens.Lens' VideoBlackFailoverSettings (Prelude.Maybe Prelude.Natural)
videoBlackFailoverSettings_videoBlackThresholdMsec = Lens.lens (\VideoBlackFailoverSettings' {videoBlackThresholdMsec} -> videoBlackThresholdMsec) (\s@VideoBlackFailoverSettings' {} a -> s {videoBlackThresholdMsec = a} :: VideoBlackFailoverSettings)

instance Data.FromJSON VideoBlackFailoverSettings where
  parseJSON =
    Data.withObject
      "VideoBlackFailoverSettings"
      ( \x ->
          VideoBlackFailoverSettings'
            Prelude.<$> (x Data..:? "blackDetectThreshold")
            Prelude.<*> (x Data..:? "videoBlackThresholdMsec")
      )

instance Prelude.Hashable VideoBlackFailoverSettings where
  hashWithSalt _salt VideoBlackFailoverSettings' {..} =
    _salt
      `Prelude.hashWithSalt` blackDetectThreshold
      `Prelude.hashWithSalt` videoBlackThresholdMsec

instance Prelude.NFData VideoBlackFailoverSettings where
  rnf VideoBlackFailoverSettings' {..} =
    Prelude.rnf blackDetectThreshold
      `Prelude.seq` Prelude.rnf videoBlackThresholdMsec

instance Data.ToJSON VideoBlackFailoverSettings where
  toJSON VideoBlackFailoverSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("blackDetectThreshold" Data..=)
              Prelude.<$> blackDetectThreshold,
            ("videoBlackThresholdMsec" Data..=)
              Prelude.<$> videoBlackThresholdMsec
          ]
      )
