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
-- Module      : Network.AWS.CodeCommit.Types.MergeOperations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeOperations where

import Network.AWS.CodeCommit.Types.ChangeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the file operation conflicts in a merge operation.
--
-- /See:/ 'newMergeOperations' smart constructor.
data MergeOperations = MergeOperations'
  { -- | The operation (add, modify, or delete) on a file in the source of a
    -- merge or pull request.
    source :: Prelude.Maybe ChangeTypeEnum,
    -- | The operation on a file in the destination of a merge or pull request.
    destination :: Prelude.Maybe ChangeTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MergeOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'mergeOperations_source' - The operation (add, modify, or delete) on a file in the source of a
-- merge or pull request.
--
-- 'destination', 'mergeOperations_destination' - The operation on a file in the destination of a merge or pull request.
newMergeOperations ::
  MergeOperations
newMergeOperations =
  MergeOperations'
    { source = Prelude.Nothing,
      destination = Prelude.Nothing
    }

-- | The operation (add, modify, or delete) on a file in the source of a
-- merge or pull request.
mergeOperations_source :: Lens.Lens' MergeOperations (Prelude.Maybe ChangeTypeEnum)
mergeOperations_source = Lens.lens (\MergeOperations' {source} -> source) (\s@MergeOperations' {} a -> s {source = a} :: MergeOperations)

-- | The operation on a file in the destination of a merge or pull request.
mergeOperations_destination :: Lens.Lens' MergeOperations (Prelude.Maybe ChangeTypeEnum)
mergeOperations_destination = Lens.lens (\MergeOperations' {destination} -> destination) (\s@MergeOperations' {} a -> s {destination = a} :: MergeOperations)

instance Prelude.FromJSON MergeOperations where
  parseJSON =
    Prelude.withObject
      "MergeOperations"
      ( \x ->
          MergeOperations'
            Prelude.<$> (x Prelude..:? "source")
            Prelude.<*> (x Prelude..:? "destination")
      )

instance Prelude.Hashable MergeOperations

instance Prelude.NFData MergeOperations
