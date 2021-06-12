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
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse where

import Network.AWS.CloudDirectory.Types.PolicyToPath
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a LookupPolicy response operation.
--
-- /See:/ 'newBatchLookupPolicyResponse' smart constructor.
data BatchLookupPolicyResponse = BatchLookupPolicyResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Provides list of path to policies. Policies contain @PolicyId@,
    -- @ObjectIdentifier@, and @PolicyType@. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
    policyToPathList :: Core.Maybe [PolicyToPath]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      policyToPathList = Core.Nothing
    }

-- | The pagination token.
batchLookupPolicyResponse_nextToken :: Lens.Lens' BatchLookupPolicyResponse (Core.Maybe Core.Text)
batchLookupPolicyResponse_nextToken = Lens.lens (\BatchLookupPolicyResponse' {nextToken} -> nextToken) (\s@BatchLookupPolicyResponse' {} a -> s {nextToken = a} :: BatchLookupPolicyResponse)

-- | Provides list of path to policies. Policies contain @PolicyId@,
-- @ObjectIdentifier@, and @PolicyType@. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
batchLookupPolicyResponse_policyToPathList :: Lens.Lens' BatchLookupPolicyResponse (Core.Maybe [PolicyToPath])
batchLookupPolicyResponse_policyToPathList = Lens.lens (\BatchLookupPolicyResponse' {policyToPathList} -> policyToPathList) (\s@BatchLookupPolicyResponse' {} a -> s {policyToPathList = a} :: BatchLookupPolicyResponse) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON BatchLookupPolicyResponse where
  parseJSON =
    Core.withObject
      "BatchLookupPolicyResponse"
      ( \x ->
          BatchLookupPolicyResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PolicyToPathList" Core..!= Core.mempty)
      )

instance Core.Hashable BatchLookupPolicyResponse

instance Core.NFData BatchLookupPolicyResponse
