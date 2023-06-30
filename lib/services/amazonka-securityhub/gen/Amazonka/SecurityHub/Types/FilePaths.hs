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
-- Module      : Amazonka.SecurityHub.Types.FilePaths
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FilePaths where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the file paths that were affected by the
-- threat.
--
-- /See:/ 'newFilePaths' smart constructor.
data FilePaths = FilePaths'
  { -- | The name of the infected or suspicious file corresponding to the hash.
    fileName :: Prelude.Maybe Prelude.Text,
    -- | Path to the infected or suspicious file on the resource it was detected
    -- on.
    filePath :: Prelude.Maybe Prelude.Text,
    -- | The hash value for the infected or suspicious file.
    hash :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource on which the threat was
    -- detected.
    resourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilePaths' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileName', 'filePaths_fileName' - The name of the infected or suspicious file corresponding to the hash.
--
-- 'filePath', 'filePaths_filePath' - Path to the infected or suspicious file on the resource it was detected
-- on.
--
-- 'hash', 'filePaths_hash' - The hash value for the infected or suspicious file.
--
-- 'resourceId', 'filePaths_resourceId' - The Amazon Resource Name (ARN) of the resource on which the threat was
-- detected.
newFilePaths ::
  FilePaths
newFilePaths =
  FilePaths'
    { fileName = Prelude.Nothing,
      filePath = Prelude.Nothing,
      hash = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | The name of the infected or suspicious file corresponding to the hash.
filePaths_fileName :: Lens.Lens' FilePaths (Prelude.Maybe Prelude.Text)
filePaths_fileName = Lens.lens (\FilePaths' {fileName} -> fileName) (\s@FilePaths' {} a -> s {fileName = a} :: FilePaths)

-- | Path to the infected or suspicious file on the resource it was detected
-- on.
filePaths_filePath :: Lens.Lens' FilePaths (Prelude.Maybe Prelude.Text)
filePaths_filePath = Lens.lens (\FilePaths' {filePath} -> filePath) (\s@FilePaths' {} a -> s {filePath = a} :: FilePaths)

-- | The hash value for the infected or suspicious file.
filePaths_hash :: Lens.Lens' FilePaths (Prelude.Maybe Prelude.Text)
filePaths_hash = Lens.lens (\FilePaths' {hash} -> hash) (\s@FilePaths' {} a -> s {hash = a} :: FilePaths)

-- | The Amazon Resource Name (ARN) of the resource on which the threat was
-- detected.
filePaths_resourceId :: Lens.Lens' FilePaths (Prelude.Maybe Prelude.Text)
filePaths_resourceId = Lens.lens (\FilePaths' {resourceId} -> resourceId) (\s@FilePaths' {} a -> s {resourceId = a} :: FilePaths)

instance Data.FromJSON FilePaths where
  parseJSON =
    Data.withObject
      "FilePaths"
      ( \x ->
          FilePaths'
            Prelude.<$> (x Data..:? "FileName")
            Prelude.<*> (x Data..:? "FilePath")
            Prelude.<*> (x Data..:? "Hash")
            Prelude.<*> (x Data..:? "ResourceId")
      )

instance Prelude.Hashable FilePaths where
  hashWithSalt _salt FilePaths' {..} =
    _salt
      `Prelude.hashWithSalt` fileName
      `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` hash
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData FilePaths where
  rnf FilePaths' {..} =
    Prelude.rnf fileName
      `Prelude.seq` Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf hash
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToJSON FilePaths where
  toJSON FilePaths' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FileName" Data..=) Prelude.<$> fileName,
            ("FilePath" Data..=) Prelude.<$> filePath,
            ("Hash" Data..=) Prelude.<$> hash,
            ("ResourceId" Data..=) Prelude.<$> resourceId
          ]
      )
