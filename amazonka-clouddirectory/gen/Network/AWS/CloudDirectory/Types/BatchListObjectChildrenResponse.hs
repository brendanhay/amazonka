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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a ListObjectChildren response operation.
--
-- /See:/ 'newBatchListObjectChildrenResponse' smart constructor.
data BatchListObjectChildrenResponse = BatchListObjectChildrenResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The children structure, which is a map with the key as the @LinkName@
    -- and @ObjectIdentifier@ as the value.
    children :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchListObjectChildrenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectChildrenResponse_nextToken' - The pagination token.
--
-- 'children', 'batchListObjectChildrenResponse_children' - The children structure, which is a map with the key as the @LinkName@
-- and @ObjectIdentifier@ as the value.
newBatchListObjectChildrenResponse ::
  BatchListObjectChildrenResponse
newBatchListObjectChildrenResponse =
  BatchListObjectChildrenResponse'
    { nextToken =
        Core.Nothing,
      children = Core.Nothing
    }

-- | The pagination token.
batchListObjectChildrenResponse_nextToken :: Lens.Lens' BatchListObjectChildrenResponse (Core.Maybe Core.Text)
batchListObjectChildrenResponse_nextToken = Lens.lens (\BatchListObjectChildrenResponse' {nextToken} -> nextToken) (\s@BatchListObjectChildrenResponse' {} a -> s {nextToken = a} :: BatchListObjectChildrenResponse)

-- | The children structure, which is a map with the key as the @LinkName@
-- and @ObjectIdentifier@ as the value.
batchListObjectChildrenResponse_children :: Lens.Lens' BatchListObjectChildrenResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
batchListObjectChildrenResponse_children = Lens.lens (\BatchListObjectChildrenResponse' {children} -> children) (\s@BatchListObjectChildrenResponse' {} a -> s {children = a} :: BatchListObjectChildrenResponse) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    BatchListObjectChildrenResponse
  where
  parseJSON =
    Core.withObject
      "BatchListObjectChildrenResponse"
      ( \x ->
          BatchListObjectChildrenResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Children" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    BatchListObjectChildrenResponse

instance Core.NFData BatchListObjectChildrenResponse
