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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndexResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIndexResponse where

import Network.AWS.CloudDirectory.Types.IndexAttachment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a ListIndex response operation.
--
-- /See:/ 'newBatchListIndexResponse' smart constructor.
data BatchListIndexResponse = BatchListIndexResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The objects and indexed values attached to the index.
    indexAttachments :: Prelude.Maybe [IndexAttachment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      indexAttachments = Prelude.Nothing
    }

-- | The pagination token.
batchListIndexResponse_nextToken :: Lens.Lens' BatchListIndexResponse (Prelude.Maybe Prelude.Text)
batchListIndexResponse_nextToken = Lens.lens (\BatchListIndexResponse' {nextToken} -> nextToken) (\s@BatchListIndexResponse' {} a -> s {nextToken = a} :: BatchListIndexResponse)

-- | The objects and indexed values attached to the index.
batchListIndexResponse_indexAttachments :: Lens.Lens' BatchListIndexResponse (Prelude.Maybe [IndexAttachment])
batchListIndexResponse_indexAttachments = Lens.lens (\BatchListIndexResponse' {indexAttachments} -> indexAttachments) (\s@BatchListIndexResponse' {} a -> s {indexAttachments = a} :: BatchListIndexResponse) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON BatchListIndexResponse where
  parseJSON =
    Prelude.withObject
      "BatchListIndexResponse"
      ( \x ->
          BatchListIndexResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> ( x Prelude..:? "IndexAttachments"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchListIndexResponse

instance Prelude.NFData BatchListIndexResponse
