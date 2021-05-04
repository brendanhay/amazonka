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
-- Module      : Network.AWS.MediaPackage.Types.CmafPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafPackage where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.CmafEncryption
import Network.AWS.MediaPackage.Types.HlsManifest
import Network.AWS.MediaPackage.Types.StreamSelection
import qualified Network.AWS.Prelude as Prelude

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'newCmafPackage' smart constructor.
data CmafPackage = CmafPackage'
  { streamSelection :: Prelude.Maybe StreamSelection,
    -- | A list of HLS manifest configurations
    hlsManifests :: Prelude.Maybe [HlsManifest],
    -- | An optional custom string that is prepended to the name of each segment.
    -- If not specified, it defaults to the ChannelId.
    segmentPrefix :: Prelude.Maybe Prelude.Text,
    encryption :: Prelude.Maybe CmafEncryption,
    -- | Duration (in seconds) of each segment. Actual segments will be rounded
    -- to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CmafPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamSelection', 'cmafPackage_streamSelection' - Undocumented member.
--
-- 'hlsManifests', 'cmafPackage_hlsManifests' - A list of HLS manifest configurations
--
-- 'segmentPrefix', 'cmafPackage_segmentPrefix' - An optional custom string that is prepended to the name of each segment.
-- If not specified, it defaults to the ChannelId.
--
-- 'encryption', 'cmafPackage_encryption' - Undocumented member.
--
-- 'segmentDurationSeconds', 'cmafPackage_segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
newCmafPackage ::
  CmafPackage
newCmafPackage =
  CmafPackage'
    { streamSelection = Prelude.Nothing,
      hlsManifests = Prelude.Nothing,
      segmentPrefix = Prelude.Nothing,
      encryption = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing
    }

-- | Undocumented member.
cmafPackage_streamSelection :: Lens.Lens' CmafPackage (Prelude.Maybe StreamSelection)
cmafPackage_streamSelection = Lens.lens (\CmafPackage' {streamSelection} -> streamSelection) (\s@CmafPackage' {} a -> s {streamSelection = a} :: CmafPackage)

-- | A list of HLS manifest configurations
cmafPackage_hlsManifests :: Lens.Lens' CmafPackage (Prelude.Maybe [HlsManifest])
cmafPackage_hlsManifests = Lens.lens (\CmafPackage' {hlsManifests} -> hlsManifests) (\s@CmafPackage' {} a -> s {hlsManifests = a} :: CmafPackage) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional custom string that is prepended to the name of each segment.
-- If not specified, it defaults to the ChannelId.
cmafPackage_segmentPrefix :: Lens.Lens' CmafPackage (Prelude.Maybe Prelude.Text)
cmafPackage_segmentPrefix = Lens.lens (\CmafPackage' {segmentPrefix} -> segmentPrefix) (\s@CmafPackage' {} a -> s {segmentPrefix = a} :: CmafPackage)

-- | Undocumented member.
cmafPackage_encryption :: Lens.Lens' CmafPackage (Prelude.Maybe CmafEncryption)
cmafPackage_encryption = Lens.lens (\CmafPackage' {encryption} -> encryption) (\s@CmafPackage' {} a -> s {encryption = a} :: CmafPackage)

-- | Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
cmafPackage_segmentDurationSeconds :: Lens.Lens' CmafPackage (Prelude.Maybe Prelude.Int)
cmafPackage_segmentDurationSeconds = Lens.lens (\CmafPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@CmafPackage' {} a -> s {segmentDurationSeconds = a} :: CmafPackage)

instance Prelude.FromJSON CmafPackage where
  parseJSON =
    Prelude.withObject
      "CmafPackage"
      ( \x ->
          CmafPackage'
            Prelude.<$> (x Prelude..:? "streamSelection")
            Prelude.<*> ( x Prelude..:? "hlsManifests"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "segmentPrefix")
            Prelude.<*> (x Prelude..:? "encryption")
            Prelude.<*> (x Prelude..:? "segmentDurationSeconds")
      )

instance Prelude.Hashable CmafPackage

instance Prelude.NFData CmafPackage
