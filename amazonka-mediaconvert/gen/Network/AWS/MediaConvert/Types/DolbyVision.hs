{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
import Network.AWS.MediaConvert.Types.DolbyVisionProfile
import qualified Network.AWS.Prelude as Prelude

-- | Settings for Dolby Vision
--
-- /See:/ 'newDolbyVision' smart constructor.
data DolbyVision = DolbyVision'
  { -- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision
    -- MaxCLL and MaxFALL properies.
    l6Mode :: Prelude.Maybe DolbyVisionLevel6Mode,
    -- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
    -- override the MaxCLL and MaxFALL values in your input with new values.
    l6Metadata :: Prelude.Maybe DolbyVisionLevel6Metadata,
    -- | In the current MediaConvert implementation, the Dolby Vision profile is
    -- always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby
    -- Vision frame interleaved data.
    profile :: Prelude.Maybe DolbyVisionProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { l6Mode = Prelude.Nothing,
      l6Metadata = Prelude.Nothing,
      profile = Prelude.Nothing
    }

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision
-- MaxCLL and MaxFALL properies.
dolbyVision_l6Mode :: Lens.Lens' DolbyVision (Prelude.Maybe DolbyVisionLevel6Mode)
dolbyVision_l6Mode = Lens.lens (\DolbyVision' {l6Mode} -> l6Mode) (\s@DolbyVision' {} a -> s {l6Mode = a} :: DolbyVision)

-- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
-- override the MaxCLL and MaxFALL values in your input with new values.
dolbyVision_l6Metadata :: Lens.Lens' DolbyVision (Prelude.Maybe DolbyVisionLevel6Metadata)
dolbyVision_l6Metadata = Lens.lens (\DolbyVision' {l6Metadata} -> l6Metadata) (\s@DolbyVision' {} a -> s {l6Metadata = a} :: DolbyVision)

-- | In the current MediaConvert implementation, the Dolby Vision profile is
-- always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby
-- Vision frame interleaved data.
dolbyVision_profile :: Lens.Lens' DolbyVision (Prelude.Maybe DolbyVisionProfile)
dolbyVision_profile = Lens.lens (\DolbyVision' {profile} -> profile) (\s@DolbyVision' {} a -> s {profile = a} :: DolbyVision)

instance Prelude.FromJSON DolbyVision where
  parseJSON =
    Prelude.withObject
      "DolbyVision"
      ( \x ->
          DolbyVision'
            Prelude.<$> (x Prelude..:? "l6Mode")
            Prelude.<*> (x Prelude..:? "l6Metadata")
            Prelude.<*> (x Prelude..:? "profile")
      )

instance Prelude.Hashable DolbyVision

instance Prelude.NFData DolbyVision

instance Prelude.ToJSON DolbyVision where
  toJSON DolbyVision' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("l6Mode" Prelude..=) Prelude.<$> l6Mode,
            ("l6Metadata" Prelude..=) Prelude.<$> l6Metadata,
            ("profile" Prelude..=) Prelude.<$> profile
          ]
      )
