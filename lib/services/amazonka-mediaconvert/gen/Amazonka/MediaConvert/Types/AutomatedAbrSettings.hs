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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AutomatedAbrSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AutomatedAbrRule
import qualified Amazonka.Prelude as Prelude

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR
-- package for you automatically, based on characteristics of your input
-- video. This feature optimizes video quality while minimizing the overall
-- size of your ABR package.
--
-- /See:/ 'newAutomatedAbrSettings' smart constructor.
data AutomatedAbrSettings = AutomatedAbrSettings'
  { -- | Optional. The maximum target bit rate used in your automated ABR stack.
    -- Use this value to set an upper limit on the bandwidth consumed by the
    -- highest-quality rendition. This is the rendition that is delivered to
    -- viewers with the fastest internet connections. If you don\'t specify a
    -- value, MediaConvert uses 8,000,000 (8 mb\/s) by default.
    maxAbrBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Optional. The maximum number of renditions that MediaConvert will create
    -- in your automated ABR stack. The number of renditions is determined
    -- automatically, based on analysis of each job, but will never exceed this
    -- limit. When you set this to Auto in the console, which is equivalent to
    -- excluding it from your JSON job specification, MediaConvert defaults to
    -- a limit of 15.
    maxRenditions :: Prelude.Maybe Prelude.Natural,
    -- | Optional. The minimum target bitrate used in your automated ABR stack.
    -- Use this value to set a lower limit on the bitrate of video delivered to
    -- viewers with slow internet connections. If you don\'t specify a value,
    -- MediaConvert uses 600,000 (600 kb\/s) by default.
    minAbrBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Use Automated ABR rules to specify restrictions for the
    -- rendition sizes MediaConvert will create in your ABR stack. You can use
    -- these rules if your ABR workflow has specific rendition size
    -- requirements, but you still want MediaConvert to optimize for video
    -- quality and overall file size.
    rules :: Prelude.Maybe [AutomatedAbrRule]
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
-- 'maxAbrBitrate', 'automatedAbrSettings_maxAbrBitrate' - Optional. The maximum target bit rate used in your automated ABR stack.
-- Use this value to set an upper limit on the bandwidth consumed by the
-- highest-quality rendition. This is the rendition that is delivered to
-- viewers with the fastest internet connections. If you don\'t specify a
-- value, MediaConvert uses 8,000,000 (8 mb\/s) by default.
--
-- 'maxRenditions', 'automatedAbrSettings_maxRenditions' - Optional. The maximum number of renditions that MediaConvert will create
-- in your automated ABR stack. The number of renditions is determined
-- automatically, based on analysis of each job, but will never exceed this
-- limit. When you set this to Auto in the console, which is equivalent to
-- excluding it from your JSON job specification, MediaConvert defaults to
-- a limit of 15.
--
-- 'minAbrBitrate', 'automatedAbrSettings_minAbrBitrate' - Optional. The minimum target bitrate used in your automated ABR stack.
-- Use this value to set a lower limit on the bitrate of video delivered to
-- viewers with slow internet connections. If you don\'t specify a value,
-- MediaConvert uses 600,000 (600 kb\/s) by default.
--
-- 'rules', 'automatedAbrSettings_rules' - Optional. Use Automated ABR rules to specify restrictions for the
-- rendition sizes MediaConvert will create in your ABR stack. You can use
-- these rules if your ABR workflow has specific rendition size
-- requirements, but you still want MediaConvert to optimize for video
-- quality and overall file size.
newAutomatedAbrSettings ::
  AutomatedAbrSettings
newAutomatedAbrSettings =
  AutomatedAbrSettings'
    { maxAbrBitrate =
        Prelude.Nothing,
      maxRenditions = Prelude.Nothing,
      minAbrBitrate = Prelude.Nothing,
      rules = Prelude.Nothing
    }

-- | Optional. The maximum target bit rate used in your automated ABR stack.
-- Use this value to set an upper limit on the bandwidth consumed by the
-- highest-quality rendition. This is the rendition that is delivered to
-- viewers with the fastest internet connections. If you don\'t specify a
-- value, MediaConvert uses 8,000,000 (8 mb\/s) by default.
automatedAbrSettings_maxAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Prelude.Maybe Prelude.Natural)
automatedAbrSettings_maxAbrBitrate = Lens.lens (\AutomatedAbrSettings' {maxAbrBitrate} -> maxAbrBitrate) (\s@AutomatedAbrSettings' {} a -> s {maxAbrBitrate = a} :: AutomatedAbrSettings)

-- | Optional. The maximum number of renditions that MediaConvert will create
-- in your automated ABR stack. The number of renditions is determined
-- automatically, based on analysis of each job, but will never exceed this
-- limit. When you set this to Auto in the console, which is equivalent to
-- excluding it from your JSON job specification, MediaConvert defaults to
-- a limit of 15.
automatedAbrSettings_maxRenditions :: Lens.Lens' AutomatedAbrSettings (Prelude.Maybe Prelude.Natural)
automatedAbrSettings_maxRenditions = Lens.lens (\AutomatedAbrSettings' {maxRenditions} -> maxRenditions) (\s@AutomatedAbrSettings' {} a -> s {maxRenditions = a} :: AutomatedAbrSettings)

-- | Optional. The minimum target bitrate used in your automated ABR stack.
-- Use this value to set a lower limit on the bitrate of video delivered to
-- viewers with slow internet connections. If you don\'t specify a value,
-- MediaConvert uses 600,000 (600 kb\/s) by default.
automatedAbrSettings_minAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Prelude.Maybe Prelude.Natural)
automatedAbrSettings_minAbrBitrate = Lens.lens (\AutomatedAbrSettings' {minAbrBitrate} -> minAbrBitrate) (\s@AutomatedAbrSettings' {} a -> s {minAbrBitrate = a} :: AutomatedAbrSettings)

-- | Optional. Use Automated ABR rules to specify restrictions for the
-- rendition sizes MediaConvert will create in your ABR stack. You can use
-- these rules if your ABR workflow has specific rendition size
-- requirements, but you still want MediaConvert to optimize for video
-- quality and overall file size.
automatedAbrSettings_rules :: Lens.Lens' AutomatedAbrSettings (Prelude.Maybe [AutomatedAbrRule])
automatedAbrSettings_rules = Lens.lens (\AutomatedAbrSettings' {rules} -> rules) (\s@AutomatedAbrSettings' {} a -> s {rules = a} :: AutomatedAbrSettings) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutomatedAbrSettings where
  parseJSON =
    Data.withObject
      "AutomatedAbrSettings"
      ( \x ->
          AutomatedAbrSettings'
            Prelude.<$> (x Data..:? "maxAbrBitrate")
            Prelude.<*> (x Data..:? "maxRenditions")
            Prelude.<*> (x Data..:? "minAbrBitrate")
            Prelude.<*> (x Data..:? "rules" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AutomatedAbrSettings where
  hashWithSalt _salt AutomatedAbrSettings' {..} =
    _salt
      `Prelude.hashWithSalt` maxAbrBitrate
      `Prelude.hashWithSalt` maxRenditions
      `Prelude.hashWithSalt` minAbrBitrate
      `Prelude.hashWithSalt` rules

instance Prelude.NFData AutomatedAbrSettings where
  rnf AutomatedAbrSettings' {..} =
    Prelude.rnf maxAbrBitrate
      `Prelude.seq` Prelude.rnf maxRenditions
      `Prelude.seq` Prelude.rnf minAbrBitrate
      `Prelude.seq` Prelude.rnf rules

instance Data.ToJSON AutomatedAbrSettings where
  toJSON AutomatedAbrSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxAbrBitrate" Data..=) Prelude.<$> maxAbrBitrate,
            ("maxRenditions" Data..=) Prelude.<$> maxRenditions,
            ("minAbrBitrate" Data..=) Prelude.<$> minAbrBitrate,
            ("rules" Data..=) Prelude.<$> rules
          ]
      )
