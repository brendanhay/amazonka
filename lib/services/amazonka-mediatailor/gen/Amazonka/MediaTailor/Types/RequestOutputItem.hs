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
-- Module      : Amazonka.MediaTailor.Types.RequestOutputItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.RequestOutputItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.DashPlaylistSettings
import Amazonka.MediaTailor.Types.HlsPlaylistSettings
import qualified Amazonka.Prelude as Prelude

-- | The output configuration for this channel.
--
-- /See:/ 'newRequestOutputItem' smart constructor.
data RequestOutputItem = RequestOutputItem'
  { -- | HLS playlist configuration parameters.
    hlsPlaylistSettings :: Prelude.Maybe HlsPlaylistSettings,
    -- | DASH manifest configuration parameters.
    dashPlaylistSettings :: Prelude.Maybe DashPlaylistSettings,
    -- | The name of the manifest for the channel. The name appears in the
    -- @PlaybackUrl@.
    manifestName :: Prelude.Text,
    -- | A string used to match which @HttpPackageConfiguration@ is used for each
    -- @VodSource@.
    sourceGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestOutputItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hlsPlaylistSettings', 'requestOutputItem_hlsPlaylistSettings' - HLS playlist configuration parameters.
--
-- 'dashPlaylistSettings', 'requestOutputItem_dashPlaylistSettings' - DASH manifest configuration parameters.
--
-- 'manifestName', 'requestOutputItem_manifestName' - The name of the manifest for the channel. The name appears in the
-- @PlaybackUrl@.
--
-- 'sourceGroup', 'requestOutputItem_sourceGroup' - A string used to match which @HttpPackageConfiguration@ is used for each
-- @VodSource@.
newRequestOutputItem ::
  -- | 'manifestName'
  Prelude.Text ->
  -- | 'sourceGroup'
  Prelude.Text ->
  RequestOutputItem
newRequestOutputItem pManifestName_ pSourceGroup_ =
  RequestOutputItem'
    { hlsPlaylistSettings =
        Prelude.Nothing,
      dashPlaylistSettings = Prelude.Nothing,
      manifestName = pManifestName_,
      sourceGroup = pSourceGroup_
    }

-- | HLS playlist configuration parameters.
requestOutputItem_hlsPlaylistSettings :: Lens.Lens' RequestOutputItem (Prelude.Maybe HlsPlaylistSettings)
requestOutputItem_hlsPlaylistSettings = Lens.lens (\RequestOutputItem' {hlsPlaylistSettings} -> hlsPlaylistSettings) (\s@RequestOutputItem' {} a -> s {hlsPlaylistSettings = a} :: RequestOutputItem)

-- | DASH manifest configuration parameters.
requestOutputItem_dashPlaylistSettings :: Lens.Lens' RequestOutputItem (Prelude.Maybe DashPlaylistSettings)
requestOutputItem_dashPlaylistSettings = Lens.lens (\RequestOutputItem' {dashPlaylistSettings} -> dashPlaylistSettings) (\s@RequestOutputItem' {} a -> s {dashPlaylistSettings = a} :: RequestOutputItem)

-- | The name of the manifest for the channel. The name appears in the
-- @PlaybackUrl@.
requestOutputItem_manifestName :: Lens.Lens' RequestOutputItem Prelude.Text
requestOutputItem_manifestName = Lens.lens (\RequestOutputItem' {manifestName} -> manifestName) (\s@RequestOutputItem' {} a -> s {manifestName = a} :: RequestOutputItem)

-- | A string used to match which @HttpPackageConfiguration@ is used for each
-- @VodSource@.
requestOutputItem_sourceGroup :: Lens.Lens' RequestOutputItem Prelude.Text
requestOutputItem_sourceGroup = Lens.lens (\RequestOutputItem' {sourceGroup} -> sourceGroup) (\s@RequestOutputItem' {} a -> s {sourceGroup = a} :: RequestOutputItem)

instance Prelude.Hashable RequestOutputItem where
  hashWithSalt _salt RequestOutputItem' {..} =
    _salt `Prelude.hashWithSalt` hlsPlaylistSettings
      `Prelude.hashWithSalt` dashPlaylistSettings
      `Prelude.hashWithSalt` manifestName
      `Prelude.hashWithSalt` sourceGroup

instance Prelude.NFData RequestOutputItem where
  rnf RequestOutputItem' {..} =
    Prelude.rnf hlsPlaylistSettings
      `Prelude.seq` Prelude.rnf dashPlaylistSettings
      `Prelude.seq` Prelude.rnf manifestName
      `Prelude.seq` Prelude.rnf sourceGroup

instance Data.ToJSON RequestOutputItem where
  toJSON RequestOutputItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HlsPlaylistSettings" Data..=)
              Prelude.<$> hlsPlaylistSettings,
            ("DashPlaylistSettings" Data..=)
              Prelude.<$> dashPlaylistSettings,
            Prelude.Just ("ManifestName" Data..= manifestName),
            Prelude.Just ("SourceGroup" Data..= sourceGroup)
          ]
      )
