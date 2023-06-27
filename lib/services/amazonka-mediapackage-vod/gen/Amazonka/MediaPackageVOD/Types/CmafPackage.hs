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
-- Module      : Amazonka.MediaPackageVOD.Types.CmafPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.CmafPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types.CmafEncryption
import Amazonka.MediaPackageVOD.Types.HlsManifest
import qualified Amazonka.Prelude as Prelude

-- | A CMAF packaging configuration.
--
-- /See:/ 'newCmafPackage' smart constructor.
data CmafPackage = CmafPackage'
  { encryption :: Prelude.Maybe CmafEncryption,
    -- | When includeEncoderConfigurationInSegments is set to true, MediaPackage
    -- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
    -- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
    -- instead of in the init fragment. This lets you use different
    -- SPS\/PPS\/VPS settings for your assets during content playback.
    includeEncoderConfigurationInSegments :: Prelude.Maybe Prelude.Bool,
    -- | Duration (in seconds) of each fragment. Actual fragments will be rounded
    -- to the nearest multiple of the source fragment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    -- | A list of HLS manifest configurations.
    hlsManifests :: [HlsManifest]
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
-- 'includeEncoderConfigurationInSegments', 'cmafPackage_includeEncoderConfigurationInSegments' - When includeEncoderConfigurationInSegments is set to true, MediaPackage
-- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
-- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
-- instead of in the init fragment. This lets you use different
-- SPS\/PPS\/VPS settings for your assets during content playback.
--
-- 'segmentDurationSeconds', 'cmafPackage_segmentDurationSeconds' - Duration (in seconds) of each fragment. Actual fragments will be rounded
-- to the nearest multiple of the source fragment duration.
--
-- 'hlsManifests', 'cmafPackage_hlsManifests' - A list of HLS manifest configurations.
newCmafPackage ::
  CmafPackage
newCmafPackage =
  CmafPackage'
    { encryption = Prelude.Nothing,
      includeEncoderConfigurationInSegments =
        Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      hlsManifests = Prelude.mempty
    }

-- | Undocumented member.
cmafPackage_encryption :: Lens.Lens' CmafPackage (Prelude.Maybe CmafEncryption)
cmafPackage_encryption = Lens.lens (\CmafPackage' {encryption} -> encryption) (\s@CmafPackage' {} a -> s {encryption = a} :: CmafPackage)

-- | When includeEncoderConfigurationInSegments is set to true, MediaPackage
-- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
-- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
-- instead of in the init fragment. This lets you use different
-- SPS\/PPS\/VPS settings for your assets during content playback.
cmafPackage_includeEncoderConfigurationInSegments :: Lens.Lens' CmafPackage (Prelude.Maybe Prelude.Bool)
cmafPackage_includeEncoderConfigurationInSegments = Lens.lens (\CmafPackage' {includeEncoderConfigurationInSegments} -> includeEncoderConfigurationInSegments) (\s@CmafPackage' {} a -> s {includeEncoderConfigurationInSegments = a} :: CmafPackage)

-- | Duration (in seconds) of each fragment. Actual fragments will be rounded
-- to the nearest multiple of the source fragment duration.
cmafPackage_segmentDurationSeconds :: Lens.Lens' CmafPackage (Prelude.Maybe Prelude.Int)
cmafPackage_segmentDurationSeconds = Lens.lens (\CmafPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@CmafPackage' {} a -> s {segmentDurationSeconds = a} :: CmafPackage)

-- | A list of HLS manifest configurations.
cmafPackage_hlsManifests :: Lens.Lens' CmafPackage [HlsManifest]
cmafPackage_hlsManifests = Lens.lens (\CmafPackage' {hlsManifests} -> hlsManifests) (\s@CmafPackage' {} a -> s {hlsManifests = a} :: CmafPackage) Prelude.. Lens.coerced

instance Data.FromJSON CmafPackage where
  parseJSON =
    Data.withObject
      "CmafPackage"
      ( \x ->
          CmafPackage'
            Prelude.<$> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "includeEncoderConfigurationInSegments")
            Prelude.<*> (x Data..:? "segmentDurationSeconds")
            Prelude.<*> (x Data..:? "hlsManifests" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CmafPackage where
  hashWithSalt _salt CmafPackage' {..} =
    _salt
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` includeEncoderConfigurationInSegments
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` hlsManifests

instance Prelude.NFData CmafPackage where
  rnf CmafPackage' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf includeEncoderConfigurationInSegments
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf hlsManifests

instance Data.ToJSON CmafPackage where
  toJSON CmafPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryption" Data..=) Prelude.<$> encryption,
            ("includeEncoderConfigurationInSegments" Data..=)
              Prelude.<$> includeEncoderConfigurationInSegments,
            ("segmentDurationSeconds" Data..=)
              Prelude.<$> segmentDurationSeconds,
            Prelude.Just ("hlsManifests" Data..= hlsManifests)
          ]
      )
