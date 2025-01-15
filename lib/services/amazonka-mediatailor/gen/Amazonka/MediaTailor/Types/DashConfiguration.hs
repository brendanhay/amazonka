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
-- Module      : Amazonka.MediaTailor.Types.DashConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.DashConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.OriginManifestType
import qualified Amazonka.Prelude as Prelude

-- | The configuration for DASH content.
--
-- /See:/ 'newDashConfiguration' smart constructor.
data DashConfiguration = DashConfiguration'
  { -- | The URL generated by MediaTailor to initiate a playback session. The
    -- session uses server-side reporting. This setting is ignored in PUT
    -- operations.
    manifestEndpointPrefix :: Prelude.Maybe Prelude.Text,
    -- | The setting that controls whether MediaTailor includes the Location tag
    -- in DASH manifests. MediaTailor populates the Location tag with the URL
    -- for manifest update requests, to be used by players that don\'t support
    -- sticky redirects. Disable this if you have CDN routing rules set up for
    -- accessing MediaTailor manifests, and you are either using client-side
    -- reporting or your players support sticky HTTP redirects. Valid values
    -- are @DISABLED@ and @EMT_DEFAULT@. The @EMT_DEFAULT@ setting enables the
    -- inclusion of the tag and is the default value.
    mpdLocation :: Prelude.Maybe Prelude.Text,
    -- | The setting that controls whether MediaTailor handles manifests from the
    -- origin server as multi-period manifests or single-period manifests. If
    -- your origin server produces single-period manifests, set this to
    -- @SINGLE_PERIOD@. The default setting is @MULTI_PERIOD@. For multi-period
    -- manifests, omit this setting or set it to @MULTI_PERIOD@.
    originManifestType :: Prelude.Maybe OriginManifestType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestEndpointPrefix', 'dashConfiguration_manifestEndpointPrefix' - The URL generated by MediaTailor to initiate a playback session. The
-- session uses server-side reporting. This setting is ignored in PUT
-- operations.
--
-- 'mpdLocation', 'dashConfiguration_mpdLocation' - The setting that controls whether MediaTailor includes the Location tag
-- in DASH manifests. MediaTailor populates the Location tag with the URL
-- for manifest update requests, to be used by players that don\'t support
-- sticky redirects. Disable this if you have CDN routing rules set up for
-- accessing MediaTailor manifests, and you are either using client-side
-- reporting or your players support sticky HTTP redirects. Valid values
-- are @DISABLED@ and @EMT_DEFAULT@. The @EMT_DEFAULT@ setting enables the
-- inclusion of the tag and is the default value.
--
-- 'originManifestType', 'dashConfiguration_originManifestType' - The setting that controls whether MediaTailor handles manifests from the
-- origin server as multi-period manifests or single-period manifests. If
-- your origin server produces single-period manifests, set this to
-- @SINGLE_PERIOD@. The default setting is @MULTI_PERIOD@. For multi-period
-- manifests, omit this setting or set it to @MULTI_PERIOD@.
newDashConfiguration ::
  DashConfiguration
newDashConfiguration =
  DashConfiguration'
    { manifestEndpointPrefix =
        Prelude.Nothing,
      mpdLocation = Prelude.Nothing,
      originManifestType = Prelude.Nothing
    }

-- | The URL generated by MediaTailor to initiate a playback session. The
-- session uses server-side reporting. This setting is ignored in PUT
-- operations.
dashConfiguration_manifestEndpointPrefix :: Lens.Lens' DashConfiguration (Prelude.Maybe Prelude.Text)
dashConfiguration_manifestEndpointPrefix = Lens.lens (\DashConfiguration' {manifestEndpointPrefix} -> manifestEndpointPrefix) (\s@DashConfiguration' {} a -> s {manifestEndpointPrefix = a} :: DashConfiguration)

-- | The setting that controls whether MediaTailor includes the Location tag
-- in DASH manifests. MediaTailor populates the Location tag with the URL
-- for manifest update requests, to be used by players that don\'t support
-- sticky redirects. Disable this if you have CDN routing rules set up for
-- accessing MediaTailor manifests, and you are either using client-side
-- reporting or your players support sticky HTTP redirects. Valid values
-- are @DISABLED@ and @EMT_DEFAULT@. The @EMT_DEFAULT@ setting enables the
-- inclusion of the tag and is the default value.
dashConfiguration_mpdLocation :: Lens.Lens' DashConfiguration (Prelude.Maybe Prelude.Text)
dashConfiguration_mpdLocation = Lens.lens (\DashConfiguration' {mpdLocation} -> mpdLocation) (\s@DashConfiguration' {} a -> s {mpdLocation = a} :: DashConfiguration)

-- | The setting that controls whether MediaTailor handles manifests from the
-- origin server as multi-period manifests or single-period manifests. If
-- your origin server produces single-period manifests, set this to
-- @SINGLE_PERIOD@. The default setting is @MULTI_PERIOD@. For multi-period
-- manifests, omit this setting or set it to @MULTI_PERIOD@.
dashConfiguration_originManifestType :: Lens.Lens' DashConfiguration (Prelude.Maybe OriginManifestType)
dashConfiguration_originManifestType = Lens.lens (\DashConfiguration' {originManifestType} -> originManifestType) (\s@DashConfiguration' {} a -> s {originManifestType = a} :: DashConfiguration)

instance Data.FromJSON DashConfiguration where
  parseJSON =
    Data.withObject
      "DashConfiguration"
      ( \x ->
          DashConfiguration'
            Prelude.<$> (x Data..:? "ManifestEndpointPrefix")
            Prelude.<*> (x Data..:? "MpdLocation")
            Prelude.<*> (x Data..:? "OriginManifestType")
      )

instance Prelude.Hashable DashConfiguration where
  hashWithSalt _salt DashConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` manifestEndpointPrefix
      `Prelude.hashWithSalt` mpdLocation
      `Prelude.hashWithSalt` originManifestType

instance Prelude.NFData DashConfiguration where
  rnf DashConfiguration' {..} =
    Prelude.rnf manifestEndpointPrefix `Prelude.seq`
      Prelude.rnf mpdLocation `Prelude.seq`
        Prelude.rnf originManifestType
