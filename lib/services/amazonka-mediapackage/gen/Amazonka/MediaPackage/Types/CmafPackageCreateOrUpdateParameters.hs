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
-- Module      : Amazonka.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.CmafPackageCreateOrUpdateParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.CmafEncryption
import Amazonka.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
import Amazonka.MediaPackage.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'newCmafPackageCreateOrUpdateParameters' smart constructor.
data CmafPackageCreateOrUpdateParameters = CmafPackageCreateOrUpdateParameters'
  { encryption :: Prelude.Maybe CmafEncryption,
    -- | A list of HLS manifest configurations
    hlsManifests :: Prelude.Maybe [HlsManifestCreateOrUpdateParameters],
    -- | Duration (in seconds) of each segment. Actual segments will be rounded
    -- to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    -- | An optional custom string that is prepended to the name of each segment.
    -- If not specified, it defaults to the ChannelId.
    segmentPrefix :: Prelude.Maybe Prelude.Text,
    streamSelection :: Prelude.Maybe StreamSelection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CmafPackageCreateOrUpdateParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'cmafPackageCreateOrUpdateParameters_encryption' - Undocumented member.
--
-- 'hlsManifests', 'cmafPackageCreateOrUpdateParameters_hlsManifests' - A list of HLS manifest configurations
--
-- 'segmentDurationSeconds', 'cmafPackageCreateOrUpdateParameters_segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
--
-- 'segmentPrefix', 'cmafPackageCreateOrUpdateParameters_segmentPrefix' - An optional custom string that is prepended to the name of each segment.
-- If not specified, it defaults to the ChannelId.
--
-- 'streamSelection', 'cmafPackageCreateOrUpdateParameters_streamSelection' - Undocumented member.
newCmafPackageCreateOrUpdateParameters ::
  CmafPackageCreateOrUpdateParameters
newCmafPackageCreateOrUpdateParameters =
  CmafPackageCreateOrUpdateParameters'
    { encryption =
        Prelude.Nothing,
      hlsManifests = Prelude.Nothing,
      segmentDurationSeconds =
        Prelude.Nothing,
      segmentPrefix = Prelude.Nothing,
      streamSelection = Prelude.Nothing
    }

-- | Undocumented member.
cmafPackageCreateOrUpdateParameters_encryption :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Prelude.Maybe CmafEncryption)
cmafPackageCreateOrUpdateParameters_encryption = Lens.lens (\CmafPackageCreateOrUpdateParameters' {encryption} -> encryption) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {encryption = a} :: CmafPackageCreateOrUpdateParameters)

-- | A list of HLS manifest configurations
cmafPackageCreateOrUpdateParameters_hlsManifests :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Prelude.Maybe [HlsManifestCreateOrUpdateParameters])
cmafPackageCreateOrUpdateParameters_hlsManifests = Lens.lens (\CmafPackageCreateOrUpdateParameters' {hlsManifests} -> hlsManifests) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {hlsManifests = a} :: CmafPackageCreateOrUpdateParameters) Prelude.. Lens.mapping Lens.coerced

-- | Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
cmafPackageCreateOrUpdateParameters_segmentDurationSeconds :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Prelude.Maybe Prelude.Int)
cmafPackageCreateOrUpdateParameters_segmentDurationSeconds = Lens.lens (\CmafPackageCreateOrUpdateParameters' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {segmentDurationSeconds = a} :: CmafPackageCreateOrUpdateParameters)

-- | An optional custom string that is prepended to the name of each segment.
-- If not specified, it defaults to the ChannelId.
cmafPackageCreateOrUpdateParameters_segmentPrefix :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Prelude.Maybe Prelude.Text)
cmafPackageCreateOrUpdateParameters_segmentPrefix = Lens.lens (\CmafPackageCreateOrUpdateParameters' {segmentPrefix} -> segmentPrefix) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {segmentPrefix = a} :: CmafPackageCreateOrUpdateParameters)

-- | Undocumented member.
cmafPackageCreateOrUpdateParameters_streamSelection :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Prelude.Maybe StreamSelection)
cmafPackageCreateOrUpdateParameters_streamSelection = Lens.lens (\CmafPackageCreateOrUpdateParameters' {streamSelection} -> streamSelection) (\s@CmafPackageCreateOrUpdateParameters' {} a -> s {streamSelection = a} :: CmafPackageCreateOrUpdateParameters)

instance
  Prelude.Hashable
    CmafPackageCreateOrUpdateParameters
  where
  hashWithSalt
    _salt
    CmafPackageCreateOrUpdateParameters' {..} =
      _salt `Prelude.hashWithSalt` encryption
        `Prelude.hashWithSalt` hlsManifests
        `Prelude.hashWithSalt` segmentDurationSeconds
        `Prelude.hashWithSalt` segmentPrefix
        `Prelude.hashWithSalt` streamSelection

instance
  Prelude.NFData
    CmafPackageCreateOrUpdateParameters
  where
  rnf CmafPackageCreateOrUpdateParameters' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf hlsManifests
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf segmentPrefix
      `Prelude.seq` Prelude.rnf streamSelection

instance
  Data.ToJSON
    CmafPackageCreateOrUpdateParameters
  where
  toJSON CmafPackageCreateOrUpdateParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryption" Data..=) Prelude.<$> encryption,
            ("hlsManifests" Data..=) Prelude.<$> hlsManifests,
            ("segmentDurationSeconds" Data..=)
              Prelude.<$> segmentDurationSeconds,
            ("segmentPrefix" Data..=) Prelude.<$> segmentPrefix,
            ("streamSelection" Data..=)
              Prelude.<$> streamSelection
          ]
      )
