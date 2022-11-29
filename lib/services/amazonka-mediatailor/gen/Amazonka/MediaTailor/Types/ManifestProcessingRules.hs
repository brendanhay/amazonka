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
-- Module      : Amazonka.MediaTailor.Types.ManifestProcessingRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.ManifestProcessingRules where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaTailor.Types.AdMarkerPassthrough
import qualified Amazonka.Prelude as Prelude

-- | The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
--
-- /See:/ 'newManifestProcessingRules' smart constructor.
data ManifestProcessingRules = ManifestProcessingRules'
  { -- | For HLS, when set to @true@, MediaTailor passes through @EXT-X-CUE-IN@,
    -- @EXT-X-CUE-OUT@, and @EXT-X-SPLICEPOINT-SCTE35@ ad markers from the
    -- origin manifest to the MediaTailor personalized manifest.
    --
    -- No logic is applied to these ad markers. For example, if @EXT-X-CUE-OUT@
    -- has a value of @60@, but no ads are filled for that ad break,
    -- MediaTailor will not set the value to @0@.
    adMarkerPassthrough :: Prelude.Maybe AdMarkerPassthrough
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManifestProcessingRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adMarkerPassthrough', 'manifestProcessingRules_adMarkerPassthrough' - For HLS, when set to @true@, MediaTailor passes through @EXT-X-CUE-IN@,
-- @EXT-X-CUE-OUT@, and @EXT-X-SPLICEPOINT-SCTE35@ ad markers from the
-- origin manifest to the MediaTailor personalized manifest.
--
-- No logic is applied to these ad markers. For example, if @EXT-X-CUE-OUT@
-- has a value of @60@, but no ads are filled for that ad break,
-- MediaTailor will not set the value to @0@.
newManifestProcessingRules ::
  ManifestProcessingRules
newManifestProcessingRules =
  ManifestProcessingRules'
    { adMarkerPassthrough =
        Prelude.Nothing
    }

-- | For HLS, when set to @true@, MediaTailor passes through @EXT-X-CUE-IN@,
-- @EXT-X-CUE-OUT@, and @EXT-X-SPLICEPOINT-SCTE35@ ad markers from the
-- origin manifest to the MediaTailor personalized manifest.
--
-- No logic is applied to these ad markers. For example, if @EXT-X-CUE-OUT@
-- has a value of @60@, but no ads are filled for that ad break,
-- MediaTailor will not set the value to @0@.
manifestProcessingRules_adMarkerPassthrough :: Lens.Lens' ManifestProcessingRules (Prelude.Maybe AdMarkerPassthrough)
manifestProcessingRules_adMarkerPassthrough = Lens.lens (\ManifestProcessingRules' {adMarkerPassthrough} -> adMarkerPassthrough) (\s@ManifestProcessingRules' {} a -> s {adMarkerPassthrough = a} :: ManifestProcessingRules)

instance Core.FromJSON ManifestProcessingRules where
  parseJSON =
    Core.withObject
      "ManifestProcessingRules"
      ( \x ->
          ManifestProcessingRules'
            Prelude.<$> (x Core..:? "AdMarkerPassthrough")
      )

instance Prelude.Hashable ManifestProcessingRules where
  hashWithSalt _salt ManifestProcessingRules' {..} =
    _salt `Prelude.hashWithSalt` adMarkerPassthrough

instance Prelude.NFData ManifestProcessingRules where
  rnf ManifestProcessingRules' {..} =
    Prelude.rnf adMarkerPassthrough

instance Core.ToJSON ManifestProcessingRules where
  toJSON ManifestProcessingRules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AdMarkerPassthrough" Core..=)
              Prelude.<$> adMarkerPassthrough
          ]
      )
