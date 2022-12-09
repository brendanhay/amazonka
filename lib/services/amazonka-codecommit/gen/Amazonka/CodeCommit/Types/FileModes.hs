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
-- Module      : Amazonka.CodeCommit.Types.FileModes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.FileModes where

import Amazonka.CodeCommit.Types.FileModeTypeEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about file modes in a merge or pull request.
--
-- /See:/ 'newFileModes' smart constructor.
data FileModes = FileModes'
  { -- | The file mode of a file in the base of a merge or pull request.
    base :: Prelude.Maybe FileModeTypeEnum,
    -- | The file mode of a file in the destination of a merge or pull request.
    destination :: Prelude.Maybe FileModeTypeEnum,
    -- | The file mode of a file in the source of a merge or pull request.
    source :: Prelude.Maybe FileModeTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileModes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'base', 'fileModes_base' - The file mode of a file in the base of a merge or pull request.
--
-- 'destination', 'fileModes_destination' - The file mode of a file in the destination of a merge or pull request.
--
-- 'source', 'fileModes_source' - The file mode of a file in the source of a merge or pull request.
newFileModes ::
  FileModes
newFileModes =
  FileModes'
    { base = Prelude.Nothing,
      destination = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The file mode of a file in the base of a merge or pull request.
fileModes_base :: Lens.Lens' FileModes (Prelude.Maybe FileModeTypeEnum)
fileModes_base = Lens.lens (\FileModes' {base} -> base) (\s@FileModes' {} a -> s {base = a} :: FileModes)

-- | The file mode of a file in the destination of a merge or pull request.
fileModes_destination :: Lens.Lens' FileModes (Prelude.Maybe FileModeTypeEnum)
fileModes_destination = Lens.lens (\FileModes' {destination} -> destination) (\s@FileModes' {} a -> s {destination = a} :: FileModes)

-- | The file mode of a file in the source of a merge or pull request.
fileModes_source :: Lens.Lens' FileModes (Prelude.Maybe FileModeTypeEnum)
fileModes_source = Lens.lens (\FileModes' {source} -> source) (\s@FileModes' {} a -> s {source = a} :: FileModes)

instance Data.FromJSON FileModes where
  parseJSON =
    Data.withObject
      "FileModes"
      ( \x ->
          FileModes'
            Prelude.<$> (x Data..:? "base")
            Prelude.<*> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "source")
      )

instance Prelude.Hashable FileModes where
  hashWithSalt _salt FileModes' {..} =
    _salt `Prelude.hashWithSalt` base
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` source

instance Prelude.NFData FileModes where
  rnf FileModes' {..} =
    Prelude.rnf base
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf source
