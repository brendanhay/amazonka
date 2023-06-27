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
-- Module      : Amazonka.CodeCommit.Types.MergeOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.MergeOperations where

import Amazonka.CodeCommit.Types.ChangeTypeEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the file operation conflicts in a merge operation.
--
-- /See:/ 'newMergeOperations' smart constructor.
data MergeOperations = MergeOperations'
  { -- | The operation on a file in the destination of a merge or pull request.
    destination :: Prelude.Maybe ChangeTypeEnum,
    -- | The operation (add, modify, or delete) on a file in the source of a
    -- merge or pull request.
    source :: Prelude.Maybe ChangeTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergeOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'mergeOperations_destination' - The operation on a file in the destination of a merge or pull request.
--
-- 'source', 'mergeOperations_source' - The operation (add, modify, or delete) on a file in the source of a
-- merge or pull request.
newMergeOperations ::
  MergeOperations
newMergeOperations =
  MergeOperations'
    { destination = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The operation on a file in the destination of a merge or pull request.
mergeOperations_destination :: Lens.Lens' MergeOperations (Prelude.Maybe ChangeTypeEnum)
mergeOperations_destination = Lens.lens (\MergeOperations' {destination} -> destination) (\s@MergeOperations' {} a -> s {destination = a} :: MergeOperations)

-- | The operation (add, modify, or delete) on a file in the source of a
-- merge or pull request.
mergeOperations_source :: Lens.Lens' MergeOperations (Prelude.Maybe ChangeTypeEnum)
mergeOperations_source = Lens.lens (\MergeOperations' {source} -> source) (\s@MergeOperations' {} a -> s {source = a} :: MergeOperations)

instance Data.FromJSON MergeOperations where
  parseJSON =
    Data.withObject
      "MergeOperations"
      ( \x ->
          MergeOperations'
            Prelude.<$> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "source")
      )

instance Prelude.Hashable MergeOperations where
  hashWithSalt _salt MergeOperations' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` source

instance Prelude.NFData MergeOperations where
  rnf MergeOperations' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf source
