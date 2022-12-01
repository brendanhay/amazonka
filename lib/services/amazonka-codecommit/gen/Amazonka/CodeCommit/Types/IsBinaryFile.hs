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
-- Module      : Amazonka.CodeCommit.Types.IsBinaryFile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.IsBinaryFile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about whether a file is binary or textual in a merge or pull
-- request operation.
--
-- /See:/ 'newIsBinaryFile' smart constructor.
data IsBinaryFile = IsBinaryFile'
  { -- | The binary or non-binary status of a file in the destination of a merge
    -- or pull request.
    destination :: Prelude.Maybe Prelude.Bool,
    -- | The binary or non-binary status of a file in the base of a merge or pull
    -- request.
    base :: Prelude.Maybe Prelude.Bool,
    -- | The binary or non-binary status of file in the source of a merge or pull
    -- request.
    source :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsBinaryFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'isBinaryFile_destination' - The binary or non-binary status of a file in the destination of a merge
-- or pull request.
--
-- 'base', 'isBinaryFile_base' - The binary or non-binary status of a file in the base of a merge or pull
-- request.
--
-- 'source', 'isBinaryFile_source' - The binary or non-binary status of file in the source of a merge or pull
-- request.
newIsBinaryFile ::
  IsBinaryFile
newIsBinaryFile =
  IsBinaryFile'
    { destination = Prelude.Nothing,
      base = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The binary or non-binary status of a file in the destination of a merge
-- or pull request.
isBinaryFile_destination :: Lens.Lens' IsBinaryFile (Prelude.Maybe Prelude.Bool)
isBinaryFile_destination = Lens.lens (\IsBinaryFile' {destination} -> destination) (\s@IsBinaryFile' {} a -> s {destination = a} :: IsBinaryFile)

-- | The binary or non-binary status of a file in the base of a merge or pull
-- request.
isBinaryFile_base :: Lens.Lens' IsBinaryFile (Prelude.Maybe Prelude.Bool)
isBinaryFile_base = Lens.lens (\IsBinaryFile' {base} -> base) (\s@IsBinaryFile' {} a -> s {base = a} :: IsBinaryFile)

-- | The binary or non-binary status of file in the source of a merge or pull
-- request.
isBinaryFile_source :: Lens.Lens' IsBinaryFile (Prelude.Maybe Prelude.Bool)
isBinaryFile_source = Lens.lens (\IsBinaryFile' {source} -> source) (\s@IsBinaryFile' {} a -> s {source = a} :: IsBinaryFile)

instance Core.FromJSON IsBinaryFile where
  parseJSON =
    Core.withObject
      "IsBinaryFile"
      ( \x ->
          IsBinaryFile'
            Prelude.<$> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "base")
            Prelude.<*> (x Core..:? "source")
      )

instance Prelude.Hashable IsBinaryFile where
  hashWithSalt _salt IsBinaryFile' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` base
      `Prelude.hashWithSalt` source

instance Prelude.NFData IsBinaryFile where
  rnf IsBinaryFile' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf base
      `Prelude.seq` Prelude.rnf source
