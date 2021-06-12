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
-- Module      : Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.CmafEncryption
import Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
import Network.AWS.MediaPackage.Types.StreamSelection

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'newCmafPackageCreateOrUpdateParameters' smart constructor.
data CmafPackageCreateOrUpdateParameters = CmafPackageCreateOrUpdateParameters'
  { streamSelection :: Core.Maybe StreamSelection,
    -- | A list of HLS manifest configurations
    hlsManifests :: Core.Maybe [HlsManifestCreateOrUpdateParameters],
    -- | An optional custom string that is prepended to the name of each segment.
    -- If not specified, it defaults to the ChannelId.
    segmentPrefix :: Core.Maybe Core.Text,
    encryption :: Core.Maybe CmafEncryption,
    -- | Duration (in seconds) of each segment. Actual segments will be rounded
    -- to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CmafPackageCreateOrUpdateParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamSelection', 'cmafPackageCreateOrUpdateParameters_streamSelection' - Undocumented member.
--
-- 'hlsManifests', 'cmafPackageCreateOrUpdateParameters_hlsManifests' - A list of HLS manifest configurations
--
-- 'segmentPrefix', 'cmafPackageCreateOrUpdateParameters_segmentPrefix' - An optional custom string that is prepended to the name of each segment.
-- If not specified, it defaults to the ChannelId.
--
-- 'encryption', 'cmafPackageCreateOrUpdateParameters_encryption' - Undocumented member.
--
-- 'segmentDurationSeconds', 'cmafPackageCreateOrUpdateParameters_segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
newCmafPackageCreateOrUpdateParameters ::
  CmafPackageCreateOrUpdateParameters
newCmafPackageCreateOrUpdateParameters =
  CmafPackageCreateOrUpdateParameters'
    { streamSelection =
        Core.Nothing,
      hlsManifests = Core.Nothing,
      segmentPrefix = Core.Nothing,
      encryption = Core.Nothing,
      segmentDurationSeconds = Core.Nothing
    }

-- | Undocumented member.
cmafPackageCreateOrUpdateParameters_streamSelection :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe StreamSelection)
cmafPackageCreateOrUpdateParameters_streamSelection = Lens.lens (\CmafPackageCreateOrUpdateParameters' {streamSelection} -> streamSelection) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {streamSelection = a} :: CmafPackageCreateOrUpdateParameters)

-- | A list of HLS manifest configurations
cmafPackageCreateOrUpdateParameters_hlsManifests :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe [HlsManifestCreateOrUpdateParameters])
cmafPackageCreateOrUpdateParameters_hlsManifests = Lens.lens (\CmafPackageCreateOrUpdateParameters' {hlsManifests} -> hlsManifests) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {hlsManifests = a} :: CmafPackageCreateOrUpdateParameters) Core.. Lens.mapping Lens._Coerce

-- | An optional custom string that is prepended to the name of each segment.
-- If not specified, it defaults to the ChannelId.
cmafPackageCreateOrUpdateParameters_segmentPrefix :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe Core.Text)
cmafPackageCreateOrUpdateParameters_segmentPrefix = Lens.lens (\CmafPackageCreateOrUpdateParameters' {segmentPrefix} -> segmentPrefix) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {segmentPrefix = a} :: CmafPackageCreateOrUpdateParameters)

-- | Undocumented member.
cmafPackageCreateOrUpdateParameters_encryption :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe CmafEncryption)
cmafPackageCreateOrUpdateParameters_encryption = Lens.lens (\CmafPackageCreateOrUpdateParameters' {encryption} -> encryption) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {encryption = a} :: CmafPackageCreateOrUpdateParameters)

-- | Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
cmafPackageCreateOrUpdateParameters_segmentDurationSeconds :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe Core.Int)
cmafPackageCreateOrUpdateParameters_segmentDurationSeconds = Lens.lens (\CmafPackageCreateOrUpdateParameters' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {segmentDurationSeconds = a} :: CmafPackageCreateOrUpdateParameters)

instance
  Core.Hashable
    CmafPackageCreateOrUpdateParameters

instance
  Core.NFData
    CmafPackageCreateOrUpdateParameters

instance
  Core.ToJSON
    CmafPackageCreateOrUpdateParameters
  where
  toJSON CmafPackageCreateOrUpdateParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("streamSelection" Core..=)
              Core.<$> streamSelection,
            ("hlsManifests" Core..=) Core.<$> hlsManifests,
            ("segmentPrefix" Core..=) Core.<$> segmentPrefix,
            ("encryption" Core..=) Core.<$> encryption,
            ("segmentDurationSeconds" Core..=)
              Core.<$> segmentDurationSeconds
          ]
      )
