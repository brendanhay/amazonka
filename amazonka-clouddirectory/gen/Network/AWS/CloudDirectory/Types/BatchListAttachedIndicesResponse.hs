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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse where

import Network.AWS.CloudDirectory.Types.IndexAttachment
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a ListAttachedIndices response operation.
--
-- /See:/ 'newBatchListAttachedIndicesResponse' smart constructor.
data BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The indices attached to the specified object.
    indexAttachments :: Core.Maybe [IndexAttachment]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchListAttachedIndicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListAttachedIndicesResponse_nextToken' - The pagination token.
--
-- 'indexAttachments', 'batchListAttachedIndicesResponse_indexAttachments' - The indices attached to the specified object.
newBatchListAttachedIndicesResponse ::
  BatchListAttachedIndicesResponse
newBatchListAttachedIndicesResponse =
  BatchListAttachedIndicesResponse'
    { nextToken =
        Core.Nothing,
      indexAttachments = Core.Nothing
    }

-- | The pagination token.
batchListAttachedIndicesResponse_nextToken :: Lens.Lens' BatchListAttachedIndicesResponse (Core.Maybe Core.Text)
batchListAttachedIndicesResponse_nextToken = Lens.lens (\BatchListAttachedIndicesResponse' {nextToken} -> nextToken) (\s@BatchListAttachedIndicesResponse' {} a -> s {nextToken = a} :: BatchListAttachedIndicesResponse)

-- | The indices attached to the specified object.
batchListAttachedIndicesResponse_indexAttachments :: Lens.Lens' BatchListAttachedIndicesResponse (Core.Maybe [IndexAttachment])
batchListAttachedIndicesResponse_indexAttachments = Lens.lens (\BatchListAttachedIndicesResponse' {indexAttachments} -> indexAttachments) (\s@BatchListAttachedIndicesResponse' {} a -> s {indexAttachments = a} :: BatchListAttachedIndicesResponse) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    BatchListAttachedIndicesResponse
  where
  parseJSON =
    Core.withObject
      "BatchListAttachedIndicesResponse"
      ( \x ->
          BatchListAttachedIndicesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "IndexAttachments" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    BatchListAttachedIndicesResponse

instance Core.NFData BatchListAttachedIndicesResponse
