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
-- Module      : Amazonka.MediaPackageVOD.Types.DashManifest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.DashManifest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackageVOD.Types.ManifestLayout
import Amazonka.MediaPackageVOD.Types.Profile
import Amazonka.MediaPackageVOD.Types.ScteMarkersSource
import Amazonka.MediaPackageVOD.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude

-- | A DASH manifest configuration.
--
-- /See:/ 'newDashManifest' smart constructor.
data DashManifest = DashManifest'
  { -- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
    -- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
    profile :: Prelude.Maybe Profile,
    streamSelection :: Prelude.Maybe StreamSelection,
    -- | An optional string to include in the name of the manifest.
    manifestName :: Prelude.Maybe Prelude.Text,
    -- | Determines the position of some tags in the Media Presentation
    -- Description (MPD). When set to FULL, elements like SegmentTemplate and
    -- ContentProtection are included in each Representation. When set to
    -- COMPACT, duplicate elements are combined and presented at the
    -- AdaptationSet level.
    manifestLayout :: Prelude.Maybe ManifestLayout,
    -- | The source of scte markers used. When set to SEGMENTS, the scte markers
    -- are sourced from the segments of the ingested content. When set to
    -- MANIFEST, the scte markers are sourced from the manifest of the ingested
    -- content.
    scteMarkersSource :: Prelude.Maybe ScteMarkersSource,
    -- | Minimum duration (in seconds) that a player will buffer media before
    -- starting the presentation.
    minBufferTimeSeconds :: Prelude.Maybe Prelude.Int
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
-- 'profile', 'dashManifest_profile' - The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
-- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
--
-- 'streamSelection', 'dashManifest_streamSelection' - Undocumented member.
--
-- 'manifestName', 'dashManifest_manifestName' - An optional string to include in the name of the manifest.
--
-- 'manifestLayout', 'dashManifest_manifestLayout' - Determines the position of some tags in the Media Presentation
-- Description (MPD). When set to FULL, elements like SegmentTemplate and
-- ContentProtection are included in each Representation. When set to
-- COMPACT, duplicate elements are combined and presented at the
-- AdaptationSet level.
--
-- 'scteMarkersSource', 'dashManifest_scteMarkersSource' - The source of scte markers used. When set to SEGMENTS, the scte markers
-- are sourced from the segments of the ingested content. When set to
-- MANIFEST, the scte markers are sourced from the manifest of the ingested
-- content.
--
-- 'minBufferTimeSeconds', 'dashManifest_minBufferTimeSeconds' - Minimum duration (in seconds) that a player will buffer media before
-- starting the presentation.
newDashManifest ::
  DashManifest
newDashManifest =
  DashManifest'
    { profile = Prelude.Nothing,
      streamSelection = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      manifestLayout = Prelude.Nothing,
      scteMarkersSource = Prelude.Nothing,
      minBufferTimeSeconds = Prelude.Nothing
    }

-- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
-- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
dashManifest_profile :: Lens.Lens' DashManifest (Prelude.Maybe Profile)
dashManifest_profile = Lens.lens (\DashManifest' {profile} -> profile) (\s@DashManifest' {} a -> s {profile = a} :: DashManifest)

-- | Undocumented member.
dashManifest_streamSelection :: Lens.Lens' DashManifest (Prelude.Maybe StreamSelection)
dashManifest_streamSelection = Lens.lens (\DashManifest' {streamSelection} -> streamSelection) (\s@DashManifest' {} a -> s {streamSelection = a} :: DashManifest)

-- | An optional string to include in the name of the manifest.
dashManifest_manifestName :: Lens.Lens' DashManifest (Prelude.Maybe Prelude.Text)
dashManifest_manifestName = Lens.lens (\DashManifest' {manifestName} -> manifestName) (\s@DashManifest' {} a -> s {manifestName = a} :: DashManifest)

-- | Determines the position of some tags in the Media Presentation
-- Description (MPD). When set to FULL, elements like SegmentTemplate and
-- ContentProtection are included in each Representation. When set to
-- COMPACT, duplicate elements are combined and presented at the
-- AdaptationSet level.
dashManifest_manifestLayout :: Lens.Lens' DashManifest (Prelude.Maybe ManifestLayout)
dashManifest_manifestLayout = Lens.lens (\DashManifest' {manifestLayout} -> manifestLayout) (\s@DashManifest' {} a -> s {manifestLayout = a} :: DashManifest)

-- | The source of scte markers used. When set to SEGMENTS, the scte markers
-- are sourced from the segments of the ingested content. When set to
-- MANIFEST, the scte markers are sourced from the manifest of the ingested
-- content.
dashManifest_scteMarkersSource :: Lens.Lens' DashManifest (Prelude.Maybe ScteMarkersSource)
dashManifest_scteMarkersSource = Lens.lens (\DashManifest' {scteMarkersSource} -> scteMarkersSource) (\s@DashManifest' {} a -> s {scteMarkersSource = a} :: DashManifest)

-- | Minimum duration (in seconds) that a player will buffer media before
-- starting the presentation.
dashManifest_minBufferTimeSeconds :: Lens.Lens' DashManifest (Prelude.Maybe Prelude.Int)
dashManifest_minBufferTimeSeconds = Lens.lens (\DashManifest' {minBufferTimeSeconds} -> minBufferTimeSeconds) (\s@DashManifest' {} a -> s {minBufferTimeSeconds = a} :: DashManifest)

instance Core.FromJSON DashManifest where
  parseJSON =
    Core.withObject
      "DashManifest"
      ( \x ->
          DashManifest'
            Prelude.<$> (x Core..:? "profile")
            Prelude.<*> (x Core..:? "streamSelection")
            Prelude.<*> (x Core..:? "manifestName")
            Prelude.<*> (x Core..:? "manifestLayout")
            Prelude.<*> (x Core..:? "scteMarkersSource")
            Prelude.<*> (x Core..:? "minBufferTimeSeconds")
      )

instance Prelude.Hashable DashManifest where
  hashWithSalt _salt DashManifest' {..} =
    _salt `Prelude.hashWithSalt` profile
      `Prelude.hashWithSalt` streamSelection
      `Prelude.hashWithSalt` manifestName
      `Prelude.hashWithSalt` manifestLayout
      `Prelude.hashWithSalt` scteMarkersSource
      `Prelude.hashWithSalt` minBufferTimeSeconds

instance Prelude.NFData DashManifest where
  rnf DashManifest' {..} =
    Prelude.rnf profile
      `Prelude.seq` Prelude.rnf streamSelection
      `Prelude.seq` Prelude.rnf manifestName
      `Prelude.seq` Prelude.rnf manifestLayout
      `Prelude.seq` Prelude.rnf scteMarkersSource
      `Prelude.seq` Prelude.rnf minBufferTimeSeconds

instance Core.ToJSON DashManifest where
  toJSON DashManifest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("profile" Core..=) Prelude.<$> profile,
            ("streamSelection" Core..=)
              Prelude.<$> streamSelection,
            ("manifestName" Core..=) Prelude.<$> manifestName,
            ("manifestLayout" Core..=)
              Prelude.<$> manifestLayout,
            ("scteMarkersSource" Core..=)
              Prelude.<$> scteMarkersSource,
            ("minBufferTimeSeconds" Core..=)
              Prelude.<$> minBufferTimeSeconds
          ]
      )
