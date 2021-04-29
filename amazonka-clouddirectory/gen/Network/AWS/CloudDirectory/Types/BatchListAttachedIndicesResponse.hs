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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse where

import Network.AWS.CloudDirectory.Types.IndexAttachment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a ListAttachedIndices response operation.
--
-- /See:/ 'newBatchListAttachedIndicesResponse' smart constructor.
data BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The indices attached to the specified object.
    indexAttachments :: Prelude.Maybe [IndexAttachment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      indexAttachments = Prelude.Nothing
    }

-- | The pagination token.
batchListAttachedIndicesResponse_nextToken :: Lens.Lens' BatchListAttachedIndicesResponse (Prelude.Maybe Prelude.Text)
batchListAttachedIndicesResponse_nextToken = Lens.lens (\BatchListAttachedIndicesResponse' {nextToken} -> nextToken) (\s@BatchListAttachedIndicesResponse' {} a -> s {nextToken = a} :: BatchListAttachedIndicesResponse)

-- | The indices attached to the specified object.
batchListAttachedIndicesResponse_indexAttachments :: Lens.Lens' BatchListAttachedIndicesResponse (Prelude.Maybe [IndexAttachment])
batchListAttachedIndicesResponse_indexAttachments = Lens.lens (\BatchListAttachedIndicesResponse' {indexAttachments} -> indexAttachments) (\s@BatchListAttachedIndicesResponse' {} a -> s {indexAttachments = a} :: BatchListAttachedIndicesResponse) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    BatchListAttachedIndicesResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchListAttachedIndicesResponse"
      ( \x ->
          BatchListAttachedIndicesResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> ( x Prelude..:? "IndexAttachments"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchListAttachedIndicesResponse

instance
  Prelude.NFData
    BatchListAttachedIndicesResponse
