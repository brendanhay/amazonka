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
-- Module      : Amazonka.MediaPackageVOD.Types.MssManifest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.MssManifest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude

-- | A Microsoft Smooth Streaming (MSS) manifest configuration.
--
-- /See:/ 'newMssManifest' smart constructor.
data MssManifest = MssManifest'
  { streamSelection :: Prelude.Maybe StreamSelection,
    -- | An optional string to include in the name of the manifest.
    manifestName :: Prelude.Maybe Prelude.Text
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
-- 'streamSelection', 'mssManifest_streamSelection' - Undocumented member.
--
-- 'manifestName', 'mssManifest_manifestName' - An optional string to include in the name of the manifest.
newMssManifest ::
  MssManifest
newMssManifest =
  MssManifest'
    { streamSelection = Prelude.Nothing,
      manifestName = Prelude.Nothing
    }

-- | Undocumented member.
mssManifest_streamSelection :: Lens.Lens' MssManifest (Prelude.Maybe StreamSelection)
mssManifest_streamSelection = Lens.lens (\MssManifest' {streamSelection} -> streamSelection) (\s@MssManifest' {} a -> s {streamSelection = a} :: MssManifest)

-- | An optional string to include in the name of the manifest.
mssManifest_manifestName :: Lens.Lens' MssManifest (Prelude.Maybe Prelude.Text)
mssManifest_manifestName = Lens.lens (\MssManifest' {manifestName} -> manifestName) (\s@MssManifest' {} a -> s {manifestName = a} :: MssManifest)

instance Data.FromJSON MssManifest where
  parseJSON =
    Data.withObject
      "MssManifest"
      ( \x ->
          MssManifest'
            Prelude.<$> (x Data..:? "streamSelection")
            Prelude.<*> (x Data..:? "manifestName")
      )

instance Prelude.Hashable MssManifest where
  hashWithSalt _salt MssManifest' {..} =
    _salt `Prelude.hashWithSalt` streamSelection
      `Prelude.hashWithSalt` manifestName

instance Prelude.NFData MssManifest where
  rnf MssManifest' {..} =
    Prelude.rnf streamSelection
      `Prelude.seq` Prelude.rnf manifestName

instance Data.ToJSON MssManifest where
  toJSON MssManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("streamSelection" Data..=)
              Prelude.<$> streamSelection,
            ("manifestName" Data..=) Prelude.<$> manifestName
          ]
      )
