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
-- Module      : Amazonka.MediaConvert.Types.AutomatedAbrSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AutomatedAbrSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR
-- package for you automatically, based on characteristics of your input
-- video. This feature optimizes video quality while minimizing the overall
-- size of your ABR package.
--
-- /See:/ 'newAutomatedAbrSettings' smart constructor.
data AutomatedAbrSettings = AutomatedAbrSettings'
  { -- | Optional. The maximum number of renditions that MediaConvert will create
    -- in your automated ABR stack. The number of renditions is determined
    -- automatically, based on analysis of each job, but will never exceed this
    -- limit. When you set this to Auto in the console, which is equivalent to
    -- excluding it from your JSON job specification, MediaConvert defaults to
    -- a limit of 15.
    maxRenditions :: Prelude.Maybe Prelude.Natural,
    -- | Optional. The maximum target bit rate used in your automated ABR stack.
    -- Use this value to set an upper limit on the bandwidth consumed by the
    -- highest-quality rendition. This is the rendition that is delivered to
    -- viewers with the fastest internet connections. If you don\'t specify a
    -- value, MediaConvert uses 8,000,000 (8 mb\/s) by default.
    maxAbrBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Optional. The minimum target bitrate used in your automated ABR stack.
    -- Use this value to set a lower limit on the bitrate of video delivered to
    -- viewers with slow internet connections. If you don\'t specify a value,
    -- MediaConvert uses 600,000 (600 kb\/s) by default.
    minAbrBitrate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomatedAbrSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRenditions', 'automatedAbrSettings_maxRenditions' - Optional. The maximum number of renditions that MediaConvert will create
-- in your automated ABR stack. The number of renditions is determined
-- automatically, based on analysis of each job, but will never exceed this
-- limit. When you set this to Auto in the console, which is equivalent to
-- excluding it from your JSON job specification, MediaConvert defaults to
-- a limit of 15.
--
-- 'maxAbrBitrate', 'automatedAbrSettings_maxAbrBitrate' - Optional. The maximum target bit rate used in your automated ABR stack.
-- Use this value to set an upper limit on the bandwidth consumed by the
-- highest-quality rendition. This is the rendition that is delivered to
-- viewers with the fastest internet connections. If you don\'t specify a
-- value, MediaConvert uses 8,000,000 (8 mb\/s) by default.
--
-- 'minAbrBitrate', 'automatedAbrSettings_minAbrBitrate' - Optional. The minimum target bitrate used in your automated ABR stack.
-- Use this value to set a lower limit on the bitrate of video delivered to
-- viewers with slow internet connections. If you don\'t specify a value,
-- MediaConvert uses 600,000 (600 kb\/s) by default.
newAutomatedAbrSettings ::
  AutomatedAbrSettings
newAutomatedAbrSettings =
  AutomatedAbrSettings'
    { maxRenditions =
        Prelude.Nothing,
      maxAbrBitrate = Prelude.Nothing,
      minAbrBitrate = Prelude.Nothing
    }

-- | Optional. The maximum number of renditions that MediaConvert will create
-- in your automated ABR stack. The number of renditions is determined
-- automatically, based on analysis of each job, but will never exceed this
-- limit. When you set this to Auto in the console, which is equivalent to
-- excluding it from your JSON job specification, MediaConvert defaults to
-- a limit of 15.
automatedAbrSettings_maxRenditions :: Lens.Lens' AutomatedAbrSettings (Prelude.Maybe Prelude.Natural)
automatedAbrSettings_maxRenditions = Lens.lens (\AutomatedAbrSettings' {maxRenditions} -> maxRenditions) (\s@AutomatedAbrSettings' {} a -> s {maxRenditions = a} :: AutomatedAbrSettings)

-- | Optional. The maximum target bit rate used in your automated ABR stack.
-- Use this value to set an upper limit on the bandwidth consumed by the
-- highest-quality rendition. This is the rendition that is delivered to
-- viewers with the fastest internet connections. If you don\'t specify a
-- value, MediaConvert uses 8,000,000 (8 mb\/s) by default.
automatedAbrSettings_maxAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Prelude.Maybe Prelude.Natural)
automatedAbrSettings_maxAbrBitrate = Lens.lens (\AutomatedAbrSettings' {maxAbrBitrate} -> maxAbrBitrate) (\s@AutomatedAbrSettings' {} a -> s {maxAbrBitrate = a} :: AutomatedAbrSettings)

-- | Optional. The minimum target bitrate used in your automated ABR stack.
-- Use this value to set a lower limit on the bitrate of video delivered to
-- viewers with slow internet connections. If you don\'t specify a value,
-- MediaConvert uses 600,000 (600 kb\/s) by default.
automatedAbrSettings_minAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Prelude.Maybe Prelude.Natural)
automatedAbrSettings_minAbrBitrate = Lens.lens (\AutomatedAbrSettings' {minAbrBitrate} -> minAbrBitrate) (\s@AutomatedAbrSettings' {} a -> s {minAbrBitrate = a} :: AutomatedAbrSettings)

instance Core.FromJSON AutomatedAbrSettings where
  parseJSON =
    Core.withObject
      "AutomatedAbrSettings"
      ( \x ->
          AutomatedAbrSettings'
            Prelude.<$> (x Core..:? "maxRenditions")
            Prelude.<*> (x Core..:? "maxAbrBitrate")
            Prelude.<*> (x Core..:? "minAbrBitrate")
      )

instance Prelude.Hashable AutomatedAbrSettings where
  hashWithSalt _salt AutomatedAbrSettings' {..} =
    _salt `Prelude.hashWithSalt` maxRenditions
      `Prelude.hashWithSalt` maxAbrBitrate
      `Prelude.hashWithSalt` minAbrBitrate

instance Prelude.NFData AutomatedAbrSettings where
  rnf AutomatedAbrSettings' {..} =
    Prelude.rnf maxRenditions
      `Prelude.seq` Prelude.rnf maxAbrBitrate
      `Prelude.seq` Prelude.rnf minAbrBitrate

instance Core.ToJSON AutomatedAbrSettings where
  toJSON AutomatedAbrSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxRenditions" Core..=) Prelude.<$> maxRenditions,
            ("maxAbrBitrate" Core..=) Prelude.<$> maxAbrBitrate,
            ("minAbrBitrate" Core..=) Prelude.<$> minAbrBitrate
          ]
      )
