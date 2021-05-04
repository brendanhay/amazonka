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
-- Module      : Network.AWS.CodeCommit.Types.SourceFileSpecifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SourceFileSpecifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable SourceFileSpecifier

instance Prelude.NFData SourceFileSpecifier

instance Prelude.ToJSON SourceFileSpecifier where
  toJSON SourceFileSpecifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("isMove" Prelude..=) Prelude.<$> isMove,
            Prelude.Just ("filePath" Prelude..= filePath)
          ]
      )
