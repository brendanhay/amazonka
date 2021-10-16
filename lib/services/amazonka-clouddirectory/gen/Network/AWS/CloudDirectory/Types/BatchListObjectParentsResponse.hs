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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse where

import Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a ListObjectParents response operation.
--
-- /See:/ 'newBatchListObjectParentsResponse' smart constructor.
data BatchListObjectParentsResponse = BatchListObjectParentsResponse'
  { -- | Returns a list of parent reference and LinkName Tuples.
    parentLinks :: Prelude.Maybe [ObjectIdentifierAndLinkNameTuple],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectParentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentLinks', 'batchListObjectParentsResponse_parentLinks' - Returns a list of parent reference and LinkName Tuples.
--
-- 'nextToken', 'batchListObjectParentsResponse_nextToken' - The pagination token.
newBatchListObjectParentsResponse ::
  BatchListObjectParentsResponse
newBatchListObjectParentsResponse =
  BatchListObjectParentsResponse'
    { parentLinks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Returns a list of parent reference and LinkName Tuples.
batchListObjectParentsResponse_parentLinks :: Lens.Lens' BatchListObjectParentsResponse (Prelude.Maybe [ObjectIdentifierAndLinkNameTuple])
batchListObjectParentsResponse_parentLinks = Lens.lens (\BatchListObjectParentsResponse' {parentLinks} -> parentLinks) (\s@BatchListObjectParentsResponse' {} a -> s {parentLinks = a} :: BatchListObjectParentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The pagination token.
batchListObjectParentsResponse_nextToken :: Lens.Lens' BatchListObjectParentsResponse (Prelude.Maybe Prelude.Text)
batchListObjectParentsResponse_nextToken = Lens.lens (\BatchListObjectParentsResponse' {nextToken} -> nextToken) (\s@BatchListObjectParentsResponse' {} a -> s {nextToken = a} :: BatchListObjectParentsResponse)

instance Core.FromJSON BatchListObjectParentsResponse where
  parseJSON =
    Core.withObject
      "BatchListObjectParentsResponse"
      ( \x ->
          BatchListObjectParentsResponse'
            Prelude.<$> (x Core..:? "ParentLinks" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "NextToken")
      )

instance
  Prelude.Hashable
    BatchListObjectParentsResponse

instance
  Prelude.NFData
    BatchListObjectParentsResponse
