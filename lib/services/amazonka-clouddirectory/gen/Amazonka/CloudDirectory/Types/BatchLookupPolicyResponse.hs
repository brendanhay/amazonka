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
-- Module      : Amazonka.CloudDirectory.Types.BatchLookupPolicyResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchLookupPolicyResponse where

import Amazonka.CloudDirectory.Types.PolicyToPath
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a LookupPolicy response operation.
--
-- /See:/ 'newBatchLookupPolicyResponse' smart constructor.
data BatchLookupPolicyResponse = BatchLookupPolicyResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides list of path to policies. Policies contain @PolicyId@,
    -- @ObjectIdentifier@, and @PolicyType@. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
    policyToPathList :: Prelude.Maybe [PolicyToPath]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchLookupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchLookupPolicyResponse_nextToken' - The pagination token.
--
-- 'policyToPathList', 'batchLookupPolicyResponse_policyToPathList' - Provides list of path to policies. Policies contain @PolicyId@,
-- @ObjectIdentifier@, and @PolicyType@. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
newBatchLookupPolicyResponse ::
  BatchLookupPolicyResponse
newBatchLookupPolicyResponse =
  BatchLookupPolicyResponse'
    { nextToken =
        Prelude.Nothing,
      policyToPathList = Prelude.Nothing
    }

-- | The pagination token.
batchLookupPolicyResponse_nextToken :: Lens.Lens' BatchLookupPolicyResponse (Prelude.Maybe Prelude.Text)
batchLookupPolicyResponse_nextToken = Lens.lens (\BatchLookupPolicyResponse' {nextToken} -> nextToken) (\s@BatchLookupPolicyResponse' {} a -> s {nextToken = a} :: BatchLookupPolicyResponse)

-- | Provides list of path to policies. Policies contain @PolicyId@,
-- @ObjectIdentifier@, and @PolicyType@. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
batchLookupPolicyResponse_policyToPathList :: Lens.Lens' BatchLookupPolicyResponse (Prelude.Maybe [PolicyToPath])
batchLookupPolicyResponse_policyToPathList = Lens.lens (\BatchLookupPolicyResponse' {policyToPathList} -> policyToPathList) (\s@BatchLookupPolicyResponse' {} a -> s {policyToPathList = a} :: BatchLookupPolicyResponse) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BatchLookupPolicyResponse where
  parseJSON =
    Data.withObject
      "BatchLookupPolicyResponse"
      ( \x ->
          BatchLookupPolicyResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> ( x
                            Data..:? "PolicyToPathList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchLookupPolicyResponse where
  hashWithSalt _salt BatchLookupPolicyResponse' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` policyToPathList

instance Prelude.NFData BatchLookupPolicyResponse where
  rnf BatchLookupPolicyResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf policyToPathList
