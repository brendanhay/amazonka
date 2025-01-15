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
-- Module      : Amazonka.M2.Types.FileBatchJobIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.FileBatchJobIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A batch job identifier in which the batch job to run is identified by
-- the file name and the relative path to the file name.
--
-- /See:/ 'newFileBatchJobIdentifier' smart constructor.
data FileBatchJobIdentifier = FileBatchJobIdentifier'
  { -- | The relative path to the file name for the batch job identifier.
    folderPath :: Prelude.Maybe Prelude.Text,
    -- | The file name for the batch job identifier.
    fileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileBatchJobIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'folderPath', 'fileBatchJobIdentifier_folderPath' - The relative path to the file name for the batch job identifier.
--
-- 'fileName', 'fileBatchJobIdentifier_fileName' - The file name for the batch job identifier.
newFileBatchJobIdentifier ::
  -- | 'fileName'
  Prelude.Text ->
  FileBatchJobIdentifier
newFileBatchJobIdentifier pFileName_ =
  FileBatchJobIdentifier'
    { folderPath =
        Prelude.Nothing,
      fileName = pFileName_
    }

-- | The relative path to the file name for the batch job identifier.
fileBatchJobIdentifier_folderPath :: Lens.Lens' FileBatchJobIdentifier (Prelude.Maybe Prelude.Text)
fileBatchJobIdentifier_folderPath = Lens.lens (\FileBatchJobIdentifier' {folderPath} -> folderPath) (\s@FileBatchJobIdentifier' {} a -> s {folderPath = a} :: FileBatchJobIdentifier)

-- | The file name for the batch job identifier.
fileBatchJobIdentifier_fileName :: Lens.Lens' FileBatchJobIdentifier Prelude.Text
fileBatchJobIdentifier_fileName = Lens.lens (\FileBatchJobIdentifier' {fileName} -> fileName) (\s@FileBatchJobIdentifier' {} a -> s {fileName = a} :: FileBatchJobIdentifier)

instance Prelude.Hashable FileBatchJobIdentifier where
  hashWithSalt _salt FileBatchJobIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` folderPath
      `Prelude.hashWithSalt` fileName

instance Prelude.NFData FileBatchJobIdentifier where
  rnf FileBatchJobIdentifier' {..} =
    Prelude.rnf folderPath `Prelude.seq`
      Prelude.rnf fileName

instance Data.ToJSON FileBatchJobIdentifier where
  toJSON FileBatchJobIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("folderPath" Data..=) Prelude.<$> folderPath,
            Prelude.Just ("fileName" Data..= fileName)
          ]
      )
