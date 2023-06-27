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
-- Module      : Amazonka.Inspector2.Types.CodeFilePath
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CodeFilePath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information on where a code vulnerability is located in your
-- Lambda function.
--
-- /See:/ 'newCodeFilePath' smart constructor.
data CodeFilePath = CodeFilePath'
  { -- | The line number of the last line of code that a vulnerability was found
    -- in.
    endLine :: Prelude.Int,
    -- | The name of the file the code vulnerability was found in.
    fileName :: Prelude.Text,
    -- | The file path to the code that a vulnerability was found in.
    filePath :: Prelude.Text,
    -- | The line number of the first line of code that a vulnerability was found
    -- in.
    startLine :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeFilePath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endLine', 'codeFilePath_endLine' - The line number of the last line of code that a vulnerability was found
-- in.
--
-- 'fileName', 'codeFilePath_fileName' - The name of the file the code vulnerability was found in.
--
-- 'filePath', 'codeFilePath_filePath' - The file path to the code that a vulnerability was found in.
--
-- 'startLine', 'codeFilePath_startLine' - The line number of the first line of code that a vulnerability was found
-- in.
newCodeFilePath ::
  -- | 'endLine'
  Prelude.Int ->
  -- | 'fileName'
  Prelude.Text ->
  -- | 'filePath'
  Prelude.Text ->
  -- | 'startLine'
  Prelude.Int ->
  CodeFilePath
newCodeFilePath
  pEndLine_
  pFileName_
  pFilePath_
  pStartLine_ =
    CodeFilePath'
      { endLine = pEndLine_,
        fileName = pFileName_,
        filePath = pFilePath_,
        startLine = pStartLine_
      }

-- | The line number of the last line of code that a vulnerability was found
-- in.
codeFilePath_endLine :: Lens.Lens' CodeFilePath Prelude.Int
codeFilePath_endLine = Lens.lens (\CodeFilePath' {endLine} -> endLine) (\s@CodeFilePath' {} a -> s {endLine = a} :: CodeFilePath)

-- | The name of the file the code vulnerability was found in.
codeFilePath_fileName :: Lens.Lens' CodeFilePath Prelude.Text
codeFilePath_fileName = Lens.lens (\CodeFilePath' {fileName} -> fileName) (\s@CodeFilePath' {} a -> s {fileName = a} :: CodeFilePath)

-- | The file path to the code that a vulnerability was found in.
codeFilePath_filePath :: Lens.Lens' CodeFilePath Prelude.Text
codeFilePath_filePath = Lens.lens (\CodeFilePath' {filePath} -> filePath) (\s@CodeFilePath' {} a -> s {filePath = a} :: CodeFilePath)

-- | The line number of the first line of code that a vulnerability was found
-- in.
codeFilePath_startLine :: Lens.Lens' CodeFilePath Prelude.Int
codeFilePath_startLine = Lens.lens (\CodeFilePath' {startLine} -> startLine) (\s@CodeFilePath' {} a -> s {startLine = a} :: CodeFilePath)

instance Data.FromJSON CodeFilePath where
  parseJSON =
    Data.withObject
      "CodeFilePath"
      ( \x ->
          CodeFilePath'
            Prelude.<$> (x Data..: "endLine")
            Prelude.<*> (x Data..: "fileName")
            Prelude.<*> (x Data..: "filePath")
            Prelude.<*> (x Data..: "startLine")
      )

instance Prelude.Hashable CodeFilePath where
  hashWithSalt _salt CodeFilePath' {..} =
    _salt
      `Prelude.hashWithSalt` endLine
      `Prelude.hashWithSalt` fileName
      `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` startLine

instance Prelude.NFData CodeFilePath where
  rnf CodeFilePath' {..} =
    Prelude.rnf endLine
      `Prelude.seq` Prelude.rnf fileName
      `Prelude.seq` Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf startLine
