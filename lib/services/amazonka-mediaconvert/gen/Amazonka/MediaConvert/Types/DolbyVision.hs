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
-- Module      : Amazonka.MediaConvert.Types.DolbyVision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DolbyVision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.DolbyVisionLevel6Metadata
import Amazonka.MediaConvert.Types.DolbyVisionLevel6Mode
import Amazonka.MediaConvert.Types.DolbyVisionMapping
import Amazonka.MediaConvert.Types.DolbyVisionProfile
import qualified Amazonka.Prelude as Prelude

-- | Create Dolby Vision Profile 5 or Profile 8.1 compatible video output.
--
-- /See:/ 'newDolbyVision' smart constructor.
data DolbyVision = DolbyVision'
  { -- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
    -- override the MaxCLL and MaxFALL values in your input with new values.
    l6Metadata :: Prelude.Maybe DolbyVisionLevel6Metadata,
    -- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision
    -- MaxCLL and MaxFALL properies.
    l6Mode :: Prelude.Maybe DolbyVisionLevel6Mode,
    -- | Required when you set Dolby Vision Profile to Profile 8.1. When you set
    -- Content mapping to None, content mapping is not applied to the
    -- HDR10-compatible signal. Depending on the source peak nit level,
    -- clipping might occur on HDR devices without Dolby Vision. When you set
    -- Content mapping to HDR10 1000, the transcoder creates a 1,000 nits peak
    -- HDR10-compatible signal by applying static content mapping to the
    -- source. This mode is speed-optimized for PQ10 sources with metadata that
    -- is created from analysis. For graded Dolby Vision content, be aware that
    -- creative intent might not be guaranteed with extreme 1,000 nits trims.
    mapping :: Prelude.Maybe DolbyVisionMapping,
    -- | Required when you enable Dolby Vision. Use Profile 5 to include
    -- frame-interleaved Dolby Vision metadata in your output. Your input must
    -- include Dolby Vision metadata or an HDR10 YUV color space. Use Profile
    -- 8.1 to include frame-interleaved Dolby Vision metadata and HDR10
    -- metadata in your output. Your input must include Dolby Vision metadata.
    profile :: Prelude.Maybe DolbyVisionProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DolbyVision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'l6Metadata', 'dolbyVision_l6Metadata' - Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
-- override the MaxCLL and MaxFALL values in your input with new values.
--
-- 'l6Mode', 'dolbyVision_l6Mode' - Use Dolby Vision Mode to choose how the service will handle Dolby Vision
-- MaxCLL and MaxFALL properies.
--
-- 'mapping', 'dolbyVision_mapping' - Required when you set Dolby Vision Profile to Profile 8.1. When you set
-- Content mapping to None, content mapping is not applied to the
-- HDR10-compatible signal. Depending on the source peak nit level,
-- clipping might occur on HDR devices without Dolby Vision. When you set
-- Content mapping to HDR10 1000, the transcoder creates a 1,000 nits peak
-- HDR10-compatible signal by applying static content mapping to the
-- source. This mode is speed-optimized for PQ10 sources with metadata that
-- is created from analysis. For graded Dolby Vision content, be aware that
-- creative intent might not be guaranteed with extreme 1,000 nits trims.
--
-- 'profile', 'dolbyVision_profile' - Required when you enable Dolby Vision. Use Profile 5 to include
-- frame-interleaved Dolby Vision metadata in your output. Your input must
-- include Dolby Vision metadata or an HDR10 YUV color space. Use Profile
-- 8.1 to include frame-interleaved Dolby Vision metadata and HDR10
-- metadata in your output. Your input must include Dolby Vision metadata.
newDolbyVision ::
  DolbyVision
newDolbyVision =
  DolbyVision'
    { l6Metadata = Prelude.Nothing,
      l6Mode = Prelude.Nothing,
      mapping = Prelude.Nothing,
      profile = Prelude.Nothing
    }

-- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
-- override the MaxCLL and MaxFALL values in your input with new values.
dolbyVision_l6Metadata :: Lens.Lens' DolbyVision (Prelude.Maybe DolbyVisionLevel6Metadata)
dolbyVision_l6Metadata = Lens.lens (\DolbyVision' {l6Metadata} -> l6Metadata) (\s@DolbyVision' {} a -> s {l6Metadata = a} :: DolbyVision)

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision
-- MaxCLL and MaxFALL properies.
dolbyVision_l6Mode :: Lens.Lens' DolbyVision (Prelude.Maybe DolbyVisionLevel6Mode)
dolbyVision_l6Mode = Lens.lens (\DolbyVision' {l6Mode} -> l6Mode) (\s@DolbyVision' {} a -> s {l6Mode = a} :: DolbyVision)

-- | Required when you set Dolby Vision Profile to Profile 8.1. When you set
-- Content mapping to None, content mapping is not applied to the
-- HDR10-compatible signal. Depending on the source peak nit level,
-- clipping might occur on HDR devices without Dolby Vision. When you set
-- Content mapping to HDR10 1000, the transcoder creates a 1,000 nits peak
-- HDR10-compatible signal by applying static content mapping to the
-- source. This mode is speed-optimized for PQ10 sources with metadata that
-- is created from analysis. For graded Dolby Vision content, be aware that
-- creative intent might not be guaranteed with extreme 1,000 nits trims.
dolbyVision_mapping :: Lens.Lens' DolbyVision (Prelude.Maybe DolbyVisionMapping)
dolbyVision_mapping = Lens.lens (\DolbyVision' {mapping} -> mapping) (\s@DolbyVision' {} a -> s {mapping = a} :: DolbyVision)

-- | Required when you enable Dolby Vision. Use Profile 5 to include
-- frame-interleaved Dolby Vision metadata in your output. Your input must
-- include Dolby Vision metadata or an HDR10 YUV color space. Use Profile
-- 8.1 to include frame-interleaved Dolby Vision metadata and HDR10
-- metadata in your output. Your input must include Dolby Vision metadata.
dolbyVision_profile :: Lens.Lens' DolbyVision (Prelude.Maybe DolbyVisionProfile)
dolbyVision_profile = Lens.lens (\DolbyVision' {profile} -> profile) (\s@DolbyVision' {} a -> s {profile = a} :: DolbyVision)

instance Data.FromJSON DolbyVision where
  parseJSON =
    Data.withObject
      "DolbyVision"
      ( \x ->
          DolbyVision'
            Prelude.<$> (x Data..:? "l6Metadata")
            Prelude.<*> (x Data..:? "l6Mode")
            Prelude.<*> (x Data..:? "mapping")
            Prelude.<*> (x Data..:? "profile")
      )

instance Prelude.Hashable DolbyVision where
  hashWithSalt _salt DolbyVision' {..} =
    _salt `Prelude.hashWithSalt` l6Metadata
      `Prelude.hashWithSalt` l6Mode
      `Prelude.hashWithSalt` mapping
      `Prelude.hashWithSalt` profile

instance Prelude.NFData DolbyVision where
  rnf DolbyVision' {..} =
    Prelude.rnf l6Metadata
      `Prelude.seq` Prelude.rnf l6Mode
      `Prelude.seq` Prelude.rnf mapping
      `Prelude.seq` Prelude.rnf profile

instance Data.ToJSON DolbyVision where
  toJSON DolbyVision' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("l6Metadata" Data..=) Prelude.<$> l6Metadata,
            ("l6Mode" Data..=) Prelude.<$> l6Mode,
            ("mapping" Data..=) Prelude.<$> mapping,
            ("profile" Data..=) Prelude.<$> profile
          ]
      )
