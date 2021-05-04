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
-- Module      : Network.AWS.CodeCommit.Types.FileSizes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileSizes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the size of files in a merge or pull request.
--
-- /See:/ 'newFileSizes' smart constructor.
data FileSizes = FileSizes'
  { -- | The size of a file in the source of a merge or pull request.
    source :: Prelude.Maybe Prelude.Integer,
    -- | The size of a file in the destination of a merge or pull request.
    destination :: Prelude.Maybe Prelude.Integer,
    -- | The size of a file in the base of a merge or pull request.
    base :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { source = Prelude.Nothing,
      destination = Prelude.Nothing,
      base = Prelude.Nothing
    }

-- | The size of a file in the source of a merge or pull request.
fileSizes_source :: Lens.Lens' FileSizes (Prelude.Maybe Prelude.Integer)
fileSizes_source = Lens.lens (\FileSizes' {source} -> source) (\s@FileSizes' {} a -> s {source = a} :: FileSizes)

-- | The size of a file in the destination of a merge or pull request.
fileSizes_destination :: Lens.Lens' FileSizes (Prelude.Maybe Prelude.Integer)
fileSizes_destination = Lens.lens (\FileSizes' {destination} -> destination) (\s@FileSizes' {} a -> s {destination = a} :: FileSizes)

-- | The size of a file in the base of a merge or pull request.
fileSizes_base :: Lens.Lens' FileSizes (Prelude.Maybe Prelude.Integer)
fileSizes_base = Lens.lens (\FileSizes' {base} -> base) (\s@FileSizes' {} a -> s {base = a} :: FileSizes)

instance Prelude.FromJSON FileSizes where
  parseJSON =
    Prelude.withObject
      "FileSizes"
      ( \x ->
          FileSizes'
            Prelude.<$> (x Prelude..:? "source")
            Prelude.<*> (x Prelude..:? "destination")
            Prelude.<*> (x Prelude..:? "base")
      )

instance Prelude.Hashable FileSizes

instance Prelude.NFData FileSizes
