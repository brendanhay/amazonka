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
-- Module      : Network.AWS.CodeCommit.Types.SetFileModeEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SetFileModeEntry where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the file mode changes.
--
-- /See:/ 'newSetFileModeEntry' smart constructor.
data SetFileModeEntry = SetFileModeEntry'
  { -- | The full path to the file, including the name of the file.
    filePath :: Prelude.Text,
    -- | The file mode for the file.
    fileMode :: FileModeTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable SetFileModeEntry

instance Prelude.NFData SetFileModeEntry

instance Prelude.ToJSON SetFileModeEntry where
  toJSON SetFileModeEntry' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("filePath" Prelude..= filePath),
            Prelude.Just ("fileMode" Prelude..= fileMode)
          ]
      )
