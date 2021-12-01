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
-- Module      : Amazonka.CloudDirectory.Types.BatchListIndexResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListIndexResponse where

import Amazonka.CloudDirectory.Types.IndexAttachment
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListIndex response operation.
--
-- /See:/ 'newBatchListIndexResponse' smart constructor.
data BatchListIndexResponse = BatchListIndexResponse'
  { -- | The objects and indexed values attached to the index.
    indexAttachments :: Prelude.Maybe [IndexAttachment],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexAttachments', 'batchListIndexResponse_indexAttachments' - The objects and indexed values attached to the index.
--
-- 'nextToken', 'batchListIndexResponse_nextToken' - The pagination token.
newBatchListIndexResponse ::
  BatchListIndexResponse
newBatchListIndexResponse =
  BatchListIndexResponse'
    { indexAttachments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The objects and indexed values attached to the index.
batchListIndexResponse_indexAttachments :: Lens.Lens' BatchListIndexResponse (Prelude.Maybe [IndexAttachment])
batchListIndexResponse_indexAttachments = Lens.lens (\BatchListIndexResponse' {indexAttachments} -> indexAttachments) (\s@BatchListIndexResponse' {} a -> s {indexAttachments = a} :: BatchListIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
batchListIndexResponse_nextToken :: Lens.Lens' BatchListIndexResponse (Prelude.Maybe Prelude.Text)
batchListIndexResponse_nextToken = Lens.lens (\BatchListIndexResponse' {nextToken} -> nextToken) (\s@BatchListIndexResponse' {} a -> s {nextToken = a} :: BatchListIndexResponse)

instance Core.FromJSON BatchListIndexResponse where
  parseJSON =
    Core.withObject
      "BatchListIndexResponse"
      ( \x ->
          BatchListIndexResponse'
            Prelude.<$> ( x Core..:? "IndexAttachments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "NextToken")
      )

instance Prelude.Hashable BatchListIndexResponse where
  hashWithSalt salt' BatchListIndexResponse' {..} =
    salt' `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` indexAttachments

instance Prelude.NFData BatchListIndexResponse where
  rnf BatchListIndexResponse' {..} =
    Prelude.rnf indexAttachments
      `Prelude.seq` Prelude.rnf nextToken
