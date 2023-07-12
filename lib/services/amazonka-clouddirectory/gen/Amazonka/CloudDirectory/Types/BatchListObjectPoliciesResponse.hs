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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectPoliciesResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectPoliciesResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListObjectPolicies response operation.
--
-- /See:/ 'newBatchListObjectPoliciesResponse' smart constructor.
data BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse'
  { -- | A list of policy @ObjectIdentifiers@, that are attached to the object.
    attachedPolicyIds :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedPolicyIds', 'batchListObjectPoliciesResponse_attachedPolicyIds' - A list of policy @ObjectIdentifiers@, that are attached to the object.
--
-- 'nextToken', 'batchListObjectPoliciesResponse_nextToken' - The pagination token.
newBatchListObjectPoliciesResponse ::
  BatchListObjectPoliciesResponse
newBatchListObjectPoliciesResponse =
  BatchListObjectPoliciesResponse'
    { attachedPolicyIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A list of policy @ObjectIdentifiers@, that are attached to the object.
batchListObjectPoliciesResponse_attachedPolicyIds :: Lens.Lens' BatchListObjectPoliciesResponse (Prelude.Maybe [Prelude.Text])
batchListObjectPoliciesResponse_attachedPolicyIds = Lens.lens (\BatchListObjectPoliciesResponse' {attachedPolicyIds} -> attachedPolicyIds) (\s@BatchListObjectPoliciesResponse' {} a -> s {attachedPolicyIds = a} :: BatchListObjectPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
batchListObjectPoliciesResponse_nextToken :: Lens.Lens' BatchListObjectPoliciesResponse (Prelude.Maybe Prelude.Text)
batchListObjectPoliciesResponse_nextToken = Lens.lens (\BatchListObjectPoliciesResponse' {nextToken} -> nextToken) (\s@BatchListObjectPoliciesResponse' {} a -> s {nextToken = a} :: BatchListObjectPoliciesResponse)

instance
  Data.FromJSON
    BatchListObjectPoliciesResponse
  where
  parseJSON =
    Data.withObject
      "BatchListObjectPoliciesResponse"
      ( \x ->
          BatchListObjectPoliciesResponse'
            Prelude.<$> ( x
                            Data..:? "AttachedPolicyIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NextToken")
      )

instance
  Prelude.Hashable
    BatchListObjectPoliciesResponse
  where
  hashWithSalt
    _salt
    BatchListObjectPoliciesResponse' {..} =
      _salt
        `Prelude.hashWithSalt` attachedPolicyIds
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    BatchListObjectPoliciesResponse
  where
  rnf BatchListObjectPoliciesResponse' {..} =
    Prelude.rnf attachedPolicyIds
      `Prelude.seq` Prelude.rnf nextToken
