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
-- Module      : Network.AWS.CodeCommit.Types.SourceFileSpecifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SourceFileSpecifier where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a source file that is part of changes made in a
-- commit.
--
-- /See:/ 'newSourceFileSpecifier' smart constructor.
data SourceFileSpecifier = SourceFileSpecifier'
  { -- | Whether to remove the source file from the parent commit.
    isMove :: Core.Maybe Core.Bool,
    -- | The full path to the file, including the name of the file.
    filePath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  SourceFileSpecifier
newSourceFileSpecifier pFilePath_ =
  SourceFileSpecifier'
    { isMove = Core.Nothing,
      filePath = pFilePath_
    }

-- | Whether to remove the source file from the parent commit.
sourceFileSpecifier_isMove :: Lens.Lens' SourceFileSpecifier (Core.Maybe Core.Bool)
sourceFileSpecifier_isMove = Lens.lens (\SourceFileSpecifier' {isMove} -> isMove) (\s@SourceFileSpecifier' {} a -> s {isMove = a} :: SourceFileSpecifier)

-- | The full path to the file, including the name of the file.
sourceFileSpecifier_filePath :: Lens.Lens' SourceFileSpecifier Core.Text
sourceFileSpecifier_filePath = Lens.lens (\SourceFileSpecifier' {filePath} -> filePath) (\s@SourceFileSpecifier' {} a -> s {filePath = a} :: SourceFileSpecifier)

instance Core.Hashable SourceFileSpecifier

instance Core.NFData SourceFileSpecifier

instance Core.ToJSON SourceFileSpecifier where
  toJSON SourceFileSpecifier' {..} =
    Core.object
      ( Core.catMaybes
          [ ("isMove" Core..=) Core.<$> isMove,
            Core.Just ("filePath" Core..= filePath)
          ]
      )
