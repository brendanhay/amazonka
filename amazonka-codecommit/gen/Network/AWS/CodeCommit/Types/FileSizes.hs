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
-- Module      : Network.AWS.CodeCommit.Types.FileSizes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileSizes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the size of files in a merge or pull request.
--
-- /See:/ 'newFileSizes' smart constructor.
data FileSizes = FileSizes'
  { -- | The size of a file in the source of a merge or pull request.
    source :: Core.Maybe Core.Integer,
    -- | The size of a file in the destination of a merge or pull request.
    destination :: Core.Maybe Core.Integer,
    -- | The size of a file in the base of a merge or pull request.
    base :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FileSizes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'fileSizes_source' - The size of a file in the source of a merge or pull request.
--
-- 'destination', 'fileSizes_destination' - The size of a file in the destination of a merge or pull request.
--
-- 'base', 'fileSizes_base' - The size of a file in the base of a merge or pull request.
newFileSizes ::
  FileSizes
newFileSizes =
  FileSizes'
    { source = Core.Nothing,
      destination = Core.Nothing,
      base = Core.Nothing
    }

-- | The size of a file in the source of a merge or pull request.
fileSizes_source :: Lens.Lens' FileSizes (Core.Maybe Core.Integer)
fileSizes_source = Lens.lens (\FileSizes' {source} -> source) (\s@FileSizes' {} a -> s {source = a} :: FileSizes)

-- | The size of a file in the destination of a merge or pull request.
fileSizes_destination :: Lens.Lens' FileSizes (Core.Maybe Core.Integer)
fileSizes_destination = Lens.lens (\FileSizes' {destination} -> destination) (\s@FileSizes' {} a -> s {destination = a} :: FileSizes)

-- | The size of a file in the base of a merge or pull request.
fileSizes_base :: Lens.Lens' FileSizes (Core.Maybe Core.Integer)
fileSizes_base = Lens.lens (\FileSizes' {base} -> base) (\s@FileSizes' {} a -> s {base = a} :: FileSizes)

instance Core.FromJSON FileSizes where
  parseJSON =
    Core.withObject
      "FileSizes"
      ( \x ->
          FileSizes'
            Core.<$> (x Core..:? "source")
            Core.<*> (x Core..:? "destination")
            Core.<*> (x Core..:? "base")
      )

instance Core.Hashable FileSizes

instance Core.NFData FileSizes
