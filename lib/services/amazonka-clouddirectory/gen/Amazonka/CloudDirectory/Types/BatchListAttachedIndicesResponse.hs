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
-- Module      : Amazonka.CloudDirectory.Types.BatchListAttachedIndicesResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListAttachedIndicesResponse where

import Amazonka.CloudDirectory.Types.IndexAttachment
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListAttachedIndices response operation.
--
-- /See:/ 'newBatchListAttachedIndicesResponse' smart constructor.
data BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The indices attached to the specified object.
    indexAttachments :: Prelude.Maybe [IndexAttachment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
batchListAttachedIndicesResponse_indexAttachments = Lens.lens (\BatchListAttachedIndicesResponse' {indexAttachments} -> indexAttachments) (\s@BatchListAttachedIndicesResponse' {} a -> s {indexAttachments = a} :: BatchListAttachedIndicesResponse) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    BatchListAttachedIndicesResponse
  where
  parseJSON =
    Data.withObject
      "BatchListAttachedIndicesResponse"
      ( \x ->
          BatchListAttachedIndicesResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> ( x Data..:? "IndexAttachments"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchListAttachedIndicesResponse
  where
  hashWithSalt
    _salt
    BatchListAttachedIndicesResponse' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` indexAttachments

instance
  Prelude.NFData
    BatchListAttachedIndicesResponse
  where
  rnf BatchListAttachedIndicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf indexAttachments
