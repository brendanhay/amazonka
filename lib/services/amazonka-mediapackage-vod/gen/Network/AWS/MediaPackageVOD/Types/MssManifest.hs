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
-- Module      : Network.AWS.MediaPackageVOD.Types.MssManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackageVOD.Types.MssManifest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackageVOD.Types.StreamSelection
import qualified Network.AWS.Prelude as Prelude

-- | A Microsoft Smooth Streaming (MSS) manifest configuration.
--
-- /See:/ 'newMssManifest' smart constructor.
data MssManifest = MssManifest'
  { -- | An optional string to include in the name of the manifest.
    manifestName :: Prelude.Maybe Prelude.Text,
    streamSelection :: Prelude.Maybe StreamSelection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MssManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestName', 'mssManifest_manifestName' - An optional string to include in the name of the manifest.
--
-- 'streamSelection', 'mssManifest_streamSelection' - Undocumented member.
newMssManifest ::
  MssManifest
newMssManifest =
  MssManifest'
    { manifestName = Prelude.Nothing,
      streamSelection = Prelude.Nothing
    }

-- | An optional string to include in the name of the manifest.
mssManifest_manifestName :: Lens.Lens' MssManifest (Prelude.Maybe Prelude.Text)
mssManifest_manifestName = Lens.lens (\MssManifest' {manifestName} -> manifestName) (\s@MssManifest' {} a -> s {manifestName = a} :: MssManifest)

-- | Undocumented member.
mssManifest_streamSelection :: Lens.Lens' MssManifest (Prelude.Maybe StreamSelection)
mssManifest_streamSelection = Lens.lens (\MssManifest' {streamSelection} -> streamSelection) (\s@MssManifest' {} a -> s {streamSelection = a} :: MssManifest)

instance Core.FromJSON MssManifest where
  parseJSON =
    Core.withObject
      "MssManifest"
      ( \x ->
          MssManifest'
            Prelude.<$> (x Core..:? "manifestName")
            Prelude.<*> (x Core..:? "streamSelection")
      )

instance Prelude.Hashable MssManifest

instance Prelude.NFData MssManifest

instance Core.ToJSON MssManifest where
  toJSON MssManifest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("manifestName" Core..=) Prelude.<$> manifestName,
            ("streamSelection" Core..=)
              Prelude.<$> streamSelection
          ]
      )
