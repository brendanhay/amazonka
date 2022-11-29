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
-- Module      : Amazonka.CodeCommit.Types.SourceFileSpecifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.SourceFileSpecifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a source file that is part of changes made in a
-- commit.
--
-- /See:/ 'newSourceFileSpecifier' smart constructor.
data SourceFileSpecifier = SourceFileSpecifier'
  { -- | Whether to remove the source file from the parent commit.
    isMove :: Prelude.Maybe Prelude.Bool,
    -- | The full path to the file, including the name of the file.
    filePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceFileSpecifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isMove', 'sourceFileSpecifier_isMove' - Whether to remove the source file from the parent commit.
--
-- 'filePath', 'sourceFileSpecifier_filePath' - The full path to the file, including the name of the file.
newSourceFileSpecifier ::
  -- | 'filePath'
  Prelude.Text ->
  SourceFileSpecifier
newSourceFileSpecifier pFilePath_ =
  SourceFileSpecifier'
    { isMove = Prelude.Nothing,
      filePath = pFilePath_
    }

-- | Whether to remove the source file from the parent commit.
sourceFileSpecifier_isMove :: Lens.Lens' SourceFileSpecifier (Prelude.Maybe Prelude.Bool)
sourceFileSpecifier_isMove = Lens.lens (\SourceFileSpecifier' {isMove} -> isMove) (\s@SourceFileSpecifier' {} a -> s {isMove = a} :: SourceFileSpecifier)

-- | The full path to the file, including the name of the file.
sourceFileSpecifier_filePath :: Lens.Lens' SourceFileSpecifier Prelude.Text
sourceFileSpecifier_filePath = Lens.lens (\SourceFileSpecifier' {filePath} -> filePath) (\s@SourceFileSpecifier' {} a -> s {filePath = a} :: SourceFileSpecifier)

instance Prelude.Hashable SourceFileSpecifier where
  hashWithSalt _salt SourceFileSpecifier' {..} =
    _salt `Prelude.hashWithSalt` isMove
      `Prelude.hashWithSalt` filePath

instance Prelude.NFData SourceFileSpecifier where
  rnf SourceFileSpecifier' {..} =
    Prelude.rnf isMove
      `Prelude.seq` Prelude.rnf filePath

instance Core.ToJSON SourceFileSpecifier where
  toJSON SourceFileSpecifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("isMove" Core..=) Prelude.<$> isMove,
            Prelude.Just ("filePath" Core..= filePath)
          ]
      )
