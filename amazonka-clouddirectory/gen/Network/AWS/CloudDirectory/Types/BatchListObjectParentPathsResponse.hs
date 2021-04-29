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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse where

import Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a ListObjectParentPaths response operation.
--
-- /See:/ 'newBatchListObjectParentPathsResponse' smart constructor.
data BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns the path to the @ObjectIdentifiers@ that are associated with the
    -- directory.
    pathToObjectIdentifiersList :: Prelude.Maybe [PathToObjectIdentifiers]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectParentPathsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectParentPathsResponse_nextToken' - The pagination token.
--
-- 'pathToObjectIdentifiersList', 'batchListObjectParentPathsResponse_pathToObjectIdentifiersList' - Returns the path to the @ObjectIdentifiers@ that are associated with the
-- directory.
newBatchListObjectParentPathsResponse ::
  BatchListObjectParentPathsResponse
newBatchListObjectParentPathsResponse =
  BatchListObjectParentPathsResponse'
    { nextToken =
        Prelude.Nothing,
      pathToObjectIdentifiersList =
        Prelude.Nothing
    }

-- | The pagination token.
batchListObjectParentPathsResponse_nextToken :: Lens.Lens' BatchListObjectParentPathsResponse (Prelude.Maybe Prelude.Text)
batchListObjectParentPathsResponse_nextToken = Lens.lens (\BatchListObjectParentPathsResponse' {nextToken} -> nextToken) (\s@BatchListObjectParentPathsResponse' {} a -> s {nextToken = a} :: BatchListObjectParentPathsResponse)

-- | Returns the path to the @ObjectIdentifiers@ that are associated with the
-- directory.
batchListObjectParentPathsResponse_pathToObjectIdentifiersList :: Lens.Lens' BatchListObjectParentPathsResponse (Prelude.Maybe [PathToObjectIdentifiers])
batchListObjectParentPathsResponse_pathToObjectIdentifiersList = Lens.lens (\BatchListObjectParentPathsResponse' {pathToObjectIdentifiersList} -> pathToObjectIdentifiersList) (\s@BatchListObjectParentPathsResponse' {} a -> s {pathToObjectIdentifiersList = a} :: BatchListObjectParentPathsResponse) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    BatchListObjectParentPathsResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchListObjectParentPathsResponse"
      ( \x ->
          BatchListObjectParentPathsResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> ( x Prelude..:? "PathToObjectIdentifiersList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchListObjectParentPathsResponse

instance
  Prelude.NFData
    BatchListObjectParentPathsResponse
