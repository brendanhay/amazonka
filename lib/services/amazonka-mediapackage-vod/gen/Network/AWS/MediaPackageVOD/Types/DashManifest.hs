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
-- Module      : Network.AWS.MediaPackageVOD.Types.DashManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackageVOD.Types.DashManifest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackageVOD.Types.ManifestLayout
import Network.AWS.MediaPackageVOD.Types.Profile
import Network.AWS.MediaPackageVOD.Types.StreamSelection
import qualified Network.AWS.Prelude as Prelude

-- | A DASH manifest configuration.
--
-- /See:/ 'newDashManifest' smart constructor.
data DashManifest = DashManifest'
  { -- | Minimum duration (in seconds) that a player will buffer media before
    -- starting the presentation.
    minBufferTimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | An optional string to include in the name of the manifest.
    manifestName :: Prelude.Maybe Prelude.Text,
    -- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
    -- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
    profile :: Prelude.Maybe Profile,
    streamSelection :: Prelude.Maybe StreamSelection,
    -- | Determines the position of some tags in the Media Presentation
    -- Description (MPD). When set to FULL, elements like SegmentTemplate and
    -- ContentProtection are included in each Representation. When set to
    -- COMPACT, duplicate elements are combined and presented at the
    -- AdaptationSet level.
    manifestLayout :: Prelude.Maybe ManifestLayout
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minBufferTimeSeconds', 'dashManifest_minBufferTimeSeconds' - Minimum duration (in seconds) that a player will buffer media before
-- starting the presentation.
--
-- 'manifestName', 'dashManifest_manifestName' - An optional string to include in the name of the manifest.
--
-- 'profile', 'dashManifest_profile' - The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
-- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
--
-- 'streamSelection', 'dashManifest_streamSelection' - Undocumented member.
--
-- 'manifestLayout', 'dashManifest_manifestLayout' - Determines the position of some tags in the Media Presentation
-- Description (MPD). When set to FULL, elements like SegmentTemplate and
-- ContentProtection are included in each Representation. When set to
-- COMPACT, duplicate elements are combined and presented at the
-- AdaptationSet level.
newDashManifest ::
  DashManifest
newDashManifest =
  DashManifest'
    { minBufferTimeSeconds =
        Prelude.Nothing,
      manifestName = Prelude.Nothing,
      profile = Prelude.Nothing,
      streamSelection = Prelude.Nothing,
      manifestLayout = Prelude.Nothing
    }

-- | Minimum duration (in seconds) that a player will buffer media before
-- starting the presentation.
dashManifest_minBufferTimeSeconds :: Lens.Lens' DashManifest (Prelude.Maybe Prelude.Int)
dashManifest_minBufferTimeSeconds = Lens.lens (\DashManifest' {minBufferTimeSeconds} -> minBufferTimeSeconds) (\s@DashManifest' {} a -> s {minBufferTimeSeconds = a} :: DashManifest)

-- | An optional string to include in the name of the manifest.
dashManifest_manifestName :: Lens.Lens' DashManifest (Prelude.Maybe Prelude.Text)
dashManifest_manifestName = Lens.lens (\DashManifest' {manifestName} -> manifestName) (\s@DashManifest' {} a -> s {manifestName = a} :: DashManifest)

-- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
-- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
dashManifest_profile :: Lens.Lens' DashManifest (Prelude.Maybe Profile)
dashManifest_profile = Lens.lens (\DashManifest' {profile} -> profile) (\s@DashManifest' {} a -> s {profile = a} :: DashManifest)

-- | Undocumented member.
dashManifest_streamSelection :: Lens.Lens' DashManifest (Prelude.Maybe StreamSelection)
dashManifest_streamSelection = Lens.lens (\DashManifest' {streamSelection} -> streamSelection) (\s@DashManifest' {} a -> s {streamSelection = a} :: DashManifest)

-- | Determines the position of some tags in the Media Presentation
-- Description (MPD). When set to FULL, elements like SegmentTemplate and
-- ContentProtection are included in each Representation. When set to
-- COMPACT, duplicate elements are combined and presented at the
-- AdaptationSet level.
dashManifest_manifestLayout :: Lens.Lens' DashManifest (Prelude.Maybe ManifestLayout)
dashManifest_manifestLayout = Lens.lens (\DashManifest' {manifestLayout} -> manifestLayout) (\s@DashManifest' {} a -> s {manifestLayout = a} :: DashManifest)

instance Core.FromJSON DashManifest where
  parseJSON =
    Core.withObject
      "DashManifest"
      ( \x ->
          DashManifest'
            Prelude.<$> (x Core..:? "minBufferTimeSeconds")
            Prelude.<*> (x Core..:? "manifestName")
            Prelude.<*> (x Core..:? "profile")
            Prelude.<*> (x Core..:? "streamSelection")
            Prelude.<*> (x Core..:? "manifestLayout")
      )

instance Prelude.Hashable DashManifest

instance Prelude.NFData DashManifest

instance Core.ToJSON DashManifest where
  toJSON DashManifest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("minBufferTimeSeconds" Core..=)
              Prelude.<$> minBufferTimeSeconds,
            ("manifestName" Core..=) Prelude.<$> manifestName,
            ("profile" Core..=) Prelude.<$> profile,
            ("streamSelection" Core..=)
              Prelude.<$> streamSelection,
            ("manifestLayout" Core..=)
              Prelude.<$> manifestLayout
          ]
      )
