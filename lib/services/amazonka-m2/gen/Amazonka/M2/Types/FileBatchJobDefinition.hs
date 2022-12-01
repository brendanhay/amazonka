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
-- Module      : Amazonka.M2.Types.FileBatchJobDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.FileBatchJobDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A file containing a batch job definition.
--
-- /See:/ 'newFileBatchJobDefinition' smart constructor.
data FileBatchJobDefinition = FileBatchJobDefinition'
  { -- | The path to the file containing the batch job definition.
    folderPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the file containing the batch job definition.
    fileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileBatchJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'folderPath', 'fileBatchJobDefinition_folderPath' - The path to the file containing the batch job definition.
--
-- 'fileName', 'fileBatchJobDefinition_fileName' - The name of the file containing the batch job definition.
newFileBatchJobDefinition ::
  -- | 'fileName'
  Prelude.Text ->
  FileBatchJobDefinition
newFileBatchJobDefinition pFileName_ =
  FileBatchJobDefinition'
    { folderPath =
        Prelude.Nothing,
      fileName = pFileName_
    }

-- | The path to the file containing the batch job definition.
fileBatchJobDefinition_folderPath :: Lens.Lens' FileBatchJobDefinition (Prelude.Maybe Prelude.Text)
fileBatchJobDefinition_folderPath = Lens.lens (\FileBatchJobDefinition' {folderPath} -> folderPath) (\s@FileBatchJobDefinition' {} a -> s {folderPath = a} :: FileBatchJobDefinition)

-- | The name of the file containing the batch job definition.
fileBatchJobDefinition_fileName :: Lens.Lens' FileBatchJobDefinition Prelude.Text
fileBatchJobDefinition_fileName = Lens.lens (\FileBatchJobDefinition' {fileName} -> fileName) (\s@FileBatchJobDefinition' {} a -> s {fileName = a} :: FileBatchJobDefinition)

instance Core.FromJSON FileBatchJobDefinition where
  parseJSON =
    Core.withObject
      "FileBatchJobDefinition"
      ( \x ->
          FileBatchJobDefinition'
            Prelude.<$> (x Core..:? "folderPath")
            Prelude.<*> (x Core..: "fileName")
      )

instance Prelude.Hashable FileBatchJobDefinition where
  hashWithSalt _salt FileBatchJobDefinition' {..} =
    _salt `Prelude.hashWithSalt` folderPath
      `Prelude.hashWithSalt` fileName

instance Prelude.NFData FileBatchJobDefinition where
  rnf FileBatchJobDefinition' {..} =
    Prelude.rnf folderPath
      `Prelude.seq` Prelude.rnf fileName
