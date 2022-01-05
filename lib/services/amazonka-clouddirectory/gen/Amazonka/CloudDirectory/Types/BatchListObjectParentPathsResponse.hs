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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectParentPathsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectParentPathsResponse where

import Amazonka.CloudDirectory.Types.PathToObjectIdentifiers
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListObjectParentPaths response operation.
--
-- /See:/ 'newBatchListObjectParentPathsResponse' smart constructor.
data BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse'
  { -- | Returns the path to the @ObjectIdentifiers@ that are associated with the
    -- directory.
    pathToObjectIdentifiersList :: Prelude.Maybe [PathToObjectIdentifiers],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectParentPathsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathToObjectIdentifiersList', 'batchListObjectParentPathsResponse_pathToObjectIdentifiersList' - Returns the path to the @ObjectIdentifiers@ that are associated with the
-- directory.
--
-- 'nextToken', 'batchListObjectParentPathsResponse_nextToken' - The pagination token.
newBatchListObjectParentPathsResponse ::
  BatchListObjectParentPathsResponse
newBatchListObjectParentPathsResponse =
  BatchListObjectParentPathsResponse'
    { pathToObjectIdentifiersList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Returns the path to the @ObjectIdentifiers@ that are associated with the
-- directory.
batchListObjectParentPathsResponse_pathToObjectIdentifiersList :: Lens.Lens' BatchListObjectParentPathsResponse (Prelude.Maybe [PathToObjectIdentifiers])
batchListObjectParentPathsResponse_pathToObjectIdentifiersList = Lens.lens (\BatchListObjectParentPathsResponse' {pathToObjectIdentifiersList} -> pathToObjectIdentifiersList) (\s@BatchListObjectParentPathsResponse' {} a -> s {pathToObjectIdentifiersList = a} :: BatchListObjectParentPathsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
batchListObjectParentPathsResponse_nextToken :: Lens.Lens' BatchListObjectParentPathsResponse (Prelude.Maybe Prelude.Text)
batchListObjectParentPathsResponse_nextToken = Lens.lens (\BatchListObjectParentPathsResponse' {nextToken} -> nextToken) (\s@BatchListObjectParentPathsResponse' {} a -> s {nextToken = a} :: BatchListObjectParentPathsResponse)

instance
  Core.FromJSON
    BatchListObjectParentPathsResponse
  where
  parseJSON =
    Core.withObject
      "BatchListObjectParentPathsResponse"
      ( \x ->
          BatchListObjectParentPathsResponse'
            Prelude.<$> ( x Core..:? "PathToObjectIdentifiersList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "NextToken")
      )

instance
  Prelude.Hashable
    BatchListObjectParentPathsResponse
  where
  hashWithSalt
    _salt
    BatchListObjectParentPathsResponse' {..} =
      _salt
        `Prelude.hashWithSalt` pathToObjectIdentifiersList
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    BatchListObjectParentPathsResponse
  where
  rnf BatchListObjectParentPathsResponse' {..} =
    Prelude.rnf pathToObjectIdentifiersList
      `Prelude.seq` Prelude.rnf nextToken
