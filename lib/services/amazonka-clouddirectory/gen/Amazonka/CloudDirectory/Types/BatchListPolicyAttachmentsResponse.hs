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
-- Module      : Amazonka.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListPolicyAttachmentsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListPolicyAttachments response operation.
--
-- /See:/ 'newBatchListPolicyAttachmentsResponse' smart constructor.
data BatchListPolicyAttachmentsResponse = BatchListPolicyAttachmentsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @ObjectIdentifiers@ to which the policy is attached.
    objectIdentifiers :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListPolicyAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListPolicyAttachmentsResponse_nextToken' - The pagination token.
--
-- 'objectIdentifiers', 'batchListPolicyAttachmentsResponse_objectIdentifiers' - A list of @ObjectIdentifiers@ to which the policy is attached.
newBatchListPolicyAttachmentsResponse ::
  BatchListPolicyAttachmentsResponse
newBatchListPolicyAttachmentsResponse =
  BatchListPolicyAttachmentsResponse'
    { nextToken =
        Prelude.Nothing,
      objectIdentifiers = Prelude.Nothing
    }

-- | The pagination token.
batchListPolicyAttachmentsResponse_nextToken :: Lens.Lens' BatchListPolicyAttachmentsResponse (Prelude.Maybe Prelude.Text)
batchListPolicyAttachmentsResponse_nextToken = Lens.lens (\BatchListPolicyAttachmentsResponse' {nextToken} -> nextToken) (\s@BatchListPolicyAttachmentsResponse' {} a -> s {nextToken = a} :: BatchListPolicyAttachmentsResponse)

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
batchListPolicyAttachmentsResponse_objectIdentifiers :: Lens.Lens' BatchListPolicyAttachmentsResponse (Prelude.Maybe [Prelude.Text])
batchListPolicyAttachmentsResponse_objectIdentifiers = Lens.lens (\BatchListPolicyAttachmentsResponse' {objectIdentifiers} -> objectIdentifiers) (\s@BatchListPolicyAttachmentsResponse' {} a -> s {objectIdentifiers = a} :: BatchListPolicyAttachmentsResponse) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    BatchListPolicyAttachmentsResponse
  where
  parseJSON =
    Data.withObject
      "BatchListPolicyAttachmentsResponse"
      ( \x ->
          BatchListPolicyAttachmentsResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> ( x
                            Data..:? "ObjectIdentifiers"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchListPolicyAttachmentsResponse
  where
  hashWithSalt
    _salt
    BatchListPolicyAttachmentsResponse' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` objectIdentifiers

instance
  Prelude.NFData
    BatchListPolicyAttachmentsResponse
  where
  rnf BatchListPolicyAttachmentsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf objectIdentifiers
