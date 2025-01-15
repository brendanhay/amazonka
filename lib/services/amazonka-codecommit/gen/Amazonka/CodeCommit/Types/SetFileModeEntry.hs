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
-- Module      : Amazonka.CodeCommit.Types.SetFileModeEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.SetFileModeEntry where

import Amazonka.CodeCommit.Types.FileModeTypeEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the file mode changes.
--
-- /See:/ 'newSetFileModeEntry' smart constructor.
data SetFileModeEntry = SetFileModeEntry'
  { -- | The full path to the file, including the name of the file.
    filePath :: Prelude.Text,
    -- | The file mode for the file.
    fileMode :: FileModeTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetFileModeEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePath', 'setFileModeEntry_filePath' - The full path to the file, including the name of the file.
--
-- 'fileMode', 'setFileModeEntry_fileMode' - The file mode for the file.
newSetFileModeEntry ::
  -- | 'filePath'
  Prelude.Text ->
  -- | 'fileMode'
  FileModeTypeEnum ->
  SetFileModeEntry
newSetFileModeEntry pFilePath_ pFileMode_ =
  SetFileModeEntry'
    { filePath = pFilePath_,
      fileMode = pFileMode_
    }

-- | The full path to the file, including the name of the file.
setFileModeEntry_filePath :: Lens.Lens' SetFileModeEntry Prelude.Text
setFileModeEntry_filePath = Lens.lens (\SetFileModeEntry' {filePath} -> filePath) (\s@SetFileModeEntry' {} a -> s {filePath = a} :: SetFileModeEntry)

-- | The file mode for the file.
setFileModeEntry_fileMode :: Lens.Lens' SetFileModeEntry FileModeTypeEnum
setFileModeEntry_fileMode = Lens.lens (\SetFileModeEntry' {fileMode} -> fileMode) (\s@SetFileModeEntry' {} a -> s {fileMode = a} :: SetFileModeEntry)

instance Prelude.Hashable SetFileModeEntry where
  hashWithSalt _salt SetFileModeEntry' {..} =
    _salt
      `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` fileMode

instance Prelude.NFData SetFileModeEntry where
  rnf SetFileModeEntry' {..} =
    Prelude.rnf filePath `Prelude.seq`
      Prelude.rnf fileMode

instance Data.ToJSON SetFileModeEntry where
  toJSON SetFileModeEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("filePath" Data..= filePath),
            Prelude.Just ("fileMode" Data..= fileMode)
          ]
      )
