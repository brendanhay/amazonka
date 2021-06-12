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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndexResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIndexResponse where

import Network.AWS.CloudDirectory.Types.IndexAttachment
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a ListIndex response operation.
--
-- /See:/ 'newBatchListIndexResponse' smart constructor.
data BatchListIndexResponse = BatchListIndexResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The objects and indexed values attached to the index.
    indexAttachments :: Core.Maybe [IndexAttachment]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchListIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListIndexResponse_nextToken' - The pagination token.
--
-- 'indexAttachments', 'batchListIndexResponse_indexAttachments' - The objects and indexed values attached to the index.
newBatchListIndexResponse ::
  BatchListIndexResponse
newBatchListIndexResponse =
  BatchListIndexResponse'
    { nextToken = Core.Nothing,
      indexAttachments = Core.Nothing
    }

-- | The pagination token.
batchListIndexResponse_nextToken :: Lens.Lens' BatchListIndexResponse (Core.Maybe Core.Text)
batchListIndexResponse_nextToken = Lens.lens (\BatchListIndexResponse' {nextToken} -> nextToken) (\s@BatchListIndexResponse' {} a -> s {nextToken = a} :: BatchListIndexResponse)

-- | The objects and indexed values attached to the index.
batchListIndexResponse_indexAttachments :: Lens.Lens' BatchListIndexResponse (Core.Maybe [IndexAttachment])
batchListIndexResponse_indexAttachments = Lens.lens (\BatchListIndexResponse' {indexAttachments} -> indexAttachments) (\s@BatchListIndexResponse' {} a -> s {indexAttachments = a} :: BatchListIndexResponse) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON BatchListIndexResponse where
  parseJSON =
    Core.withObject
      "BatchListIndexResponse"
      ( \x ->
          BatchListIndexResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "IndexAttachments" Core..!= Core.mempty)
      )

instance Core.Hashable BatchListIndexResponse

instance Core.NFData BatchListIndexResponse
