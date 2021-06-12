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
-- Module      : Network.AWS.MediaConvert.Types.AutomatedAbrSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AutomatedAbrSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR
-- package for you automatically, based on characteristics of your input
-- video. This feature optimizes video quality while minimizing the overall
-- size of your ABR package.
--
-- /See:/ 'newAutomatedAbrSettings' smart constructor.
data AutomatedAbrSettings = AutomatedAbrSettings'
  { -- | Optional. The minimum target bitrate used in your automated ABR stack.
    -- Use this value to set a lower limit on the bitrate of video delivered to
    -- viewers with slow internet connections. If you don\'t specify a value,
    -- MediaConvert uses 600,000 (600 kb\/s) by default.
    minAbrBitrate :: Core.Maybe Core.Natural,
    -- | Optional. The maximum number of renditions that MediaConvert will create
    -- in your automated ABR stack. The number of renditions is determined
    -- automatically, based on analysis of each job, but will never exceed this
    -- limit. When you set this to Auto in the console, which is equivalent to
    -- excluding it from your JSON job specification, MediaConvert defaults to
    -- a limit of 15.
    maxRenditions :: Core.Maybe Core.Natural,
    -- | Optional. The maximum target bit rate used in your automated ABR stack.
    -- Use this value to set an upper limit on the bandwidth consumed by the
    -- highest-quality rendition. This is the rendition that is delivered to
    -- viewers with the fastest internet connections. If you don\'t specify a
    -- value, MediaConvert uses 8,000,000 (8 mb\/s) by default.
    maxAbrBitrate :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutomatedAbrSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minAbrBitrate', 'automatedAbrSettings_minAbrBitrate' - Optional. The minimum target bitrate used in your automated ABR stack.
-- Use this value to set a lower limit on the bitrate of video delivered to
-- viewers with slow internet connections. If you don\'t specify a value,
-- MediaConvert uses 600,000 (600 kb\/s) by default.
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
newAutomatedAbrSettings ::
  AutomatedAbrSettings
newAutomatedAbrSettings =
  AutomatedAbrSettings'
    { minAbrBitrate = Core.Nothing,
      maxRenditions = Core.Nothing,
      maxAbrBitrate = Core.Nothing
    }

-- | Optional. The minimum target bitrate used in your automated ABR stack.
-- Use this value to set a lower limit on the bitrate of video delivered to
-- viewers with slow internet connections. If you don\'t specify a value,
-- MediaConvert uses 600,000 (600 kb\/s) by default.
automatedAbrSettings_minAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Core.Maybe Core.Natural)
automatedAbrSettings_minAbrBitrate = Lens.lens (\AutomatedAbrSettings' {minAbrBitrate} -> minAbrBitrate) (\s@AutomatedAbrSettings' {} a -> s {minAbrBitrate = a} :: AutomatedAbrSettings)

-- | Optional. The maximum number of renditions that MediaConvert will create
-- in your automated ABR stack. The number of renditions is determined
-- automatically, based on analysis of each job, but will never exceed this
-- limit. When you set this to Auto in the console, which is equivalent to
-- excluding it from your JSON job specification, MediaConvert defaults to
-- a limit of 15.
automatedAbrSettings_maxRenditions :: Lens.Lens' AutomatedAbrSettings (Core.Maybe Core.Natural)
automatedAbrSettings_maxRenditions = Lens.lens (\AutomatedAbrSettings' {maxRenditions} -> maxRenditions) (\s@AutomatedAbrSettings' {} a -> s {maxRenditions = a} :: AutomatedAbrSettings)

-- | Optional. The maximum target bit rate used in your automated ABR stack.
-- Use this value to set an upper limit on the bandwidth consumed by the
-- highest-quality rendition. This is the rendition that is delivered to
-- viewers with the fastest internet connections. If you don\'t specify a
-- value, MediaConvert uses 8,000,000 (8 mb\/s) by default.
automatedAbrSettings_maxAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Core.Maybe Core.Natural)
automatedAbrSettings_maxAbrBitrate = Lens.lens (\AutomatedAbrSettings' {maxAbrBitrate} -> maxAbrBitrate) (\s@AutomatedAbrSettings' {} a -> s {maxAbrBitrate = a} :: AutomatedAbrSettings)

instance Core.FromJSON AutomatedAbrSettings where
  parseJSON =
    Core.withObject
      "AutomatedAbrSettings"
      ( \x ->
          AutomatedAbrSettings'
            Core.<$> (x Core..:? "minAbrBitrate")
            Core.<*> (x Core..:? "maxRenditions")
            Core.<*> (x Core..:? "maxAbrBitrate")
      )

instance Core.Hashable AutomatedAbrSettings

instance Core.NFData AutomatedAbrSettings

instance Core.ToJSON AutomatedAbrSettings where
  toJSON AutomatedAbrSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("minAbrBitrate" Core..=) Core.<$> minAbrBitrate,
            ("maxRenditions" Core..=) Core.<$> maxRenditions,
            ("maxAbrBitrate" Core..=) Core.<$> maxAbrBitrate
          ]
      )
