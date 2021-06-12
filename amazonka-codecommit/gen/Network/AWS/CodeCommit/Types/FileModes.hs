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
-- Module      : Network.AWS.CodeCommit.Types.FileModes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileModes where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about file modes in a merge or pull request.
--
-- /See:/ 'newFileModes' smart constructor.
data FileModes = FileModes'
  { -- | The file mode of a file in the source of a merge or pull request.
    source :: Core.Maybe FileModeTypeEnum,
    -- | The file mode of a file in the destination of a merge or pull request.
    destination :: Core.Maybe FileModeTypeEnum,
    -- | The file mode of a file in the base of a merge or pull request.
    base :: Core.Maybe FileModeTypeEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FileModes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'fileModes_source' - The file mode of a file in the source of a merge or pull request.
--
-- 'destination', 'fileModes_destination' - The file mode of a file in the destination of a merge or pull request.
--
-- 'base', 'fileModes_base' - The file mode of a file in the base of a merge or pull request.
newFileModes ::
  FileModes
newFileModes =
  FileModes'
    { source = Core.Nothing,
      destination = Core.Nothing,
      base = Core.Nothing
    }

-- | The file mode of a file in the source of a merge or pull request.
fileModes_source :: Lens.Lens' FileModes (Core.Maybe FileModeTypeEnum)
fileModes_source = Lens.lens (\FileModes' {source} -> source) (\s@FileModes' {} a -> s {source = a} :: FileModes)

-- | The file mode of a file in the destination of a merge or pull request.
fileModes_destination :: Lens.Lens' FileModes (Core.Maybe FileModeTypeEnum)
fileModes_destination = Lens.lens (\FileModes' {destination} -> destination) (\s@FileModes' {} a -> s {destination = a} :: FileModes)

-- | The file mode of a file in the base of a merge or pull request.
fileModes_base :: Lens.Lens' FileModes (Core.Maybe FileModeTypeEnum)
fileModes_base = Lens.lens (\FileModes' {base} -> base) (\s@FileModes' {} a -> s {base = a} :: FileModes)

instance Core.FromJSON FileModes where
  parseJSON =
    Core.withObject
      "FileModes"
      ( \x ->
          FileModes'
            Core.<$> (x Core..:? "source")
            Core.<*> (x Core..:? "destination")
            Core.<*> (x Core..:? "base")
      )

instance Core.Hashable FileModes

instance Core.NFData FileModes
