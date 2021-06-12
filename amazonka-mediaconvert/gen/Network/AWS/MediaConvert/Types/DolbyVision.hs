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
-- Module      : Network.AWS.MediaConvert.Types.DolbyVision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVision where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
import Network.AWS.MediaConvert.Types.DolbyVisionProfile

-- | Settings for Dolby Vision
--
-- /See:/ 'newDolbyVision' smart constructor.
data DolbyVision = DolbyVision'
  { -- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision
    -- MaxCLL and MaxFALL properies.
    l6Mode :: Core.Maybe DolbyVisionLevel6Mode,
    -- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
    -- override the MaxCLL and MaxFALL values in your input with new values.
    l6Metadata :: Core.Maybe DolbyVisionLevel6Metadata,
    -- | In the current MediaConvert implementation, the Dolby Vision profile is
    -- always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby
    -- Vision frame interleaved data.
    profile :: Core.Maybe DolbyVisionProfile
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DolbyVision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'l6Mode', 'dolbyVision_l6Mode' - Use Dolby Vision Mode to choose how the service will handle Dolby Vision
-- MaxCLL and MaxFALL properies.
--
-- 'l6Metadata', 'dolbyVision_l6Metadata' - Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
-- override the MaxCLL and MaxFALL values in your input with new values.
--
-- 'profile', 'dolbyVision_profile' - In the current MediaConvert implementation, the Dolby Vision profile is
-- always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby
-- Vision frame interleaved data.
newDolbyVision ::
  DolbyVision
newDolbyVision =
  DolbyVision'
    { l6Mode = Core.Nothing,
      l6Metadata = Core.Nothing,
      profile = Core.Nothing
    }

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision
-- MaxCLL and MaxFALL properies.
dolbyVision_l6Mode :: Lens.Lens' DolbyVision (Core.Maybe DolbyVisionLevel6Mode)
dolbyVision_l6Mode = Lens.lens (\DolbyVision' {l6Mode} -> l6Mode) (\s@DolbyVision' {} a -> s {l6Mode = a} :: DolbyVision)

-- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
-- override the MaxCLL and MaxFALL values in your input with new values.
dolbyVision_l6Metadata :: Lens.Lens' DolbyVision (Core.Maybe DolbyVisionLevel6Metadata)
dolbyVision_l6Metadata = Lens.lens (\DolbyVision' {l6Metadata} -> l6Metadata) (\s@DolbyVision' {} a -> s {l6Metadata = a} :: DolbyVision)

-- | In the current MediaConvert implementation, the Dolby Vision profile is
-- always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby
-- Vision frame interleaved data.
dolbyVision_profile :: Lens.Lens' DolbyVision (Core.Maybe DolbyVisionProfile)
dolbyVision_profile = Lens.lens (\DolbyVision' {profile} -> profile) (\s@DolbyVision' {} a -> s {profile = a} :: DolbyVision)

instance Core.FromJSON DolbyVision where
  parseJSON =
    Core.withObject
      "DolbyVision"
      ( \x ->
          DolbyVision'
            Core.<$> (x Core..:? "l6Mode")
            Core.<*> (x Core..:? "l6Metadata")
            Core.<*> (x Core..:? "profile")
      )

instance Core.Hashable DolbyVision

instance Core.NFData DolbyVision

instance Core.ToJSON DolbyVision where
  toJSON DolbyVision' {..} =
    Core.object
      ( Core.catMaybes
          [ ("l6Mode" Core..=) Core.<$> l6Mode,
            ("l6Metadata" Core..=) Core.<$> l6Metadata,
            ("profile" Core..=) Core.<$> profile
          ]
      )
