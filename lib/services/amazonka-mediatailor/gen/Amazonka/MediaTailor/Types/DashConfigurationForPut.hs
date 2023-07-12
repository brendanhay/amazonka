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
-- Module      : Amazonka.MediaTailor.Types.DashConfigurationForPut
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.DashConfigurationForPut where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.OriginManifestType
import qualified Amazonka.Prelude as Prelude

-- | The configuration for DASH PUT operations.
--
-- /See:/ 'newDashConfigurationForPut' smart constructor.
data DashConfigurationForPut = DashConfigurationForPut'
  { -- | The setting that controls whether MediaTailor includes the Location tag
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
-- Create a value of 'DashConfigurationForPut' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mpdLocation', 'dashConfigurationForPut_mpdLocation' - The setting that controls whether MediaTailor includes the Location tag
-- in DASH manifests. MediaTailor populates the Location tag with the URL
-- for manifest update requests, to be used by players that don\'t support
-- sticky redirects. Disable this if you have CDN routing rules set up for
-- accessing MediaTailor manifests, and you are either using client-side
-- reporting or your players support sticky HTTP redirects. Valid values
-- are @DISABLED@ and @EMT_DEFAULT@. The @EMT_DEFAULT@ setting enables the
-- inclusion of the tag and is the default value.
--
-- 'originManifestType', 'dashConfigurationForPut_originManifestType' - The setting that controls whether MediaTailor handles manifests from the
-- origin server as multi-period manifests or single-period manifests. If
-- your origin server produces single-period manifests, set this to
-- @SINGLE_PERIOD@. The default setting is @MULTI_PERIOD@. For multi-period
-- manifests, omit this setting or set it to @MULTI_PERIOD@.
newDashConfigurationForPut ::
  DashConfigurationForPut
newDashConfigurationForPut =
  DashConfigurationForPut'
    { mpdLocation =
        Prelude.Nothing,
      originManifestType = Prelude.Nothing
    }

-- | The setting that controls whether MediaTailor includes the Location tag
-- in DASH manifests. MediaTailor populates the Location tag with the URL
-- for manifest update requests, to be used by players that don\'t support
-- sticky redirects. Disable this if you have CDN routing rules set up for
-- accessing MediaTailor manifests, and you are either using client-side
-- reporting or your players support sticky HTTP redirects. Valid values
-- are @DISABLED@ and @EMT_DEFAULT@. The @EMT_DEFAULT@ setting enables the
-- inclusion of the tag and is the default value.
dashConfigurationForPut_mpdLocation :: Lens.Lens' DashConfigurationForPut (Prelude.Maybe Prelude.Text)
dashConfigurationForPut_mpdLocation = Lens.lens (\DashConfigurationForPut' {mpdLocation} -> mpdLocation) (\s@DashConfigurationForPut' {} a -> s {mpdLocation = a} :: DashConfigurationForPut)

-- | The setting that controls whether MediaTailor handles manifests from the
-- origin server as multi-period manifests or single-period manifests. If
-- your origin server produces single-period manifests, set this to
-- @SINGLE_PERIOD@. The default setting is @MULTI_PERIOD@. For multi-period
-- manifests, omit this setting or set it to @MULTI_PERIOD@.
dashConfigurationForPut_originManifestType :: Lens.Lens' DashConfigurationForPut (Prelude.Maybe OriginManifestType)
dashConfigurationForPut_originManifestType = Lens.lens (\DashConfigurationForPut' {originManifestType} -> originManifestType) (\s@DashConfigurationForPut' {} a -> s {originManifestType = a} :: DashConfigurationForPut)

instance Prelude.Hashable DashConfigurationForPut where
  hashWithSalt _salt DashConfigurationForPut' {..} =
    _salt
      `Prelude.hashWithSalt` mpdLocation
      `Prelude.hashWithSalt` originManifestType

instance Prelude.NFData DashConfigurationForPut where
  rnf DashConfigurationForPut' {..} =
    Prelude.rnf mpdLocation
      `Prelude.seq` Prelude.rnf originManifestType

instance Data.ToJSON DashConfigurationForPut where
  toJSON DashConfigurationForPut' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MpdLocation" Data..=) Prelude.<$> mpdLocation,
            ("OriginManifestType" Data..=)
              Prelude.<$> originManifestType
          ]
      )
