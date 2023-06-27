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
-- Module      : Amazonka.MediaPackage.Types.CmafPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.CmafPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.CmafEncryption
import Amazonka.MediaPackage.Types.HlsManifest
import Amazonka.MediaPackage.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'newCmafPackage' smart constructor.
data CmafPackage = CmafPackage'
  { encryption :: Prelude.Maybe CmafEncryption,
    -- | A list of HLS manifest configurations
    hlsManifests :: Prelude.Maybe [HlsManifest],
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
-- Create a value of 'CmafPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'cmafPackage_encryption' - Undocumented member.
--
-- 'hlsManifests', 'cmafPackage_hlsManifests' - A list of HLS manifest configurations
--
-- 'segmentDurationSeconds', 'cmafPackage_segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
--
-- 'segmentPrefix', 'cmafPackage_segmentPrefix' - An optional custom string that is prepended to the name of each segment.
-- If not specified, it defaults to the ChannelId.
--
-- 'streamSelection', 'cmafPackage_streamSelection' - Undocumented member.
newCmafPackage ::
  CmafPackage
newCmafPackage =
  CmafPackage'
    { encryption = Prelude.Nothing,
      hlsManifests = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      segmentPrefix = Prelude.Nothing,
      streamSelection = Prelude.Nothing
    }

-- | Undocumented member.
cmafPackage_encryption :: Lens.Lens' CmafPackage (Prelude.Maybe CmafEncryption)
cmafPackage_encryption = Lens.lens (\CmafPackage' {encryption} -> encryption) (\s@CmafPackage' {} a -> s {encryption = a} :: CmafPackage)

-- | A list of HLS manifest configurations
cmafPackage_hlsManifests :: Lens.Lens' CmafPackage (Prelude.Maybe [HlsManifest])
cmafPackage_hlsManifests = Lens.lens (\CmafPackage' {hlsManifests} -> hlsManifests) (\s@CmafPackage' {} a -> s {hlsManifests = a} :: CmafPackage) Prelude.. Lens.mapping Lens.coerced

-- | Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
cmafPackage_segmentDurationSeconds :: Lens.Lens' CmafPackage (Prelude.Maybe Prelude.Int)
cmafPackage_segmentDurationSeconds = Lens.lens (\CmafPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@CmafPackage' {} a -> s {segmentDurationSeconds = a} :: CmafPackage)

-- | An optional custom string that is prepended to the name of each segment.
-- If not specified, it defaults to the ChannelId.
cmafPackage_segmentPrefix :: Lens.Lens' CmafPackage (Prelude.Maybe Prelude.Text)
cmafPackage_segmentPrefix = Lens.lens (\CmafPackage' {segmentPrefix} -> segmentPrefix) (\s@CmafPackage' {} a -> s {segmentPrefix = a} :: CmafPackage)

-- | Undocumented member.
cmafPackage_streamSelection :: Lens.Lens' CmafPackage (Prelude.Maybe StreamSelection)
cmafPackage_streamSelection = Lens.lens (\CmafPackage' {streamSelection} -> streamSelection) (\s@CmafPackage' {} a -> s {streamSelection = a} :: CmafPackage)

instance Data.FromJSON CmafPackage where
  parseJSON =
    Data.withObject
      "CmafPackage"
      ( \x ->
          CmafPackage'
            Prelude.<$> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "hlsManifests" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "segmentDurationSeconds")
            Prelude.<*> (x Data..:? "segmentPrefix")
            Prelude.<*> (x Data..:? "streamSelection")
      )

instance Prelude.Hashable CmafPackage where
  hashWithSalt _salt CmafPackage' {..} =
    _salt
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` hlsManifests
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` segmentPrefix
      `Prelude.hashWithSalt` streamSelection

instance Prelude.NFData CmafPackage where
  rnf CmafPackage' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf hlsManifests
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf segmentPrefix
      `Prelude.seq` Prelude.rnf streamSelection
