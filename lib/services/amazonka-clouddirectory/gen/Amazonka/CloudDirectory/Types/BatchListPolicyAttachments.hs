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
-- Module      : Amazonka.CloudDirectory.Types.BatchListPolicyAttachments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListPolicyAttachments where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is
-- attached inside a BatchRead operation. For more information, see
-- ListPolicyAttachments and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListPolicyAttachments' smart constructor.
data BatchListPolicyAttachments = BatchListPolicyAttachments'
  { -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The reference that identifies the policy object.
    policyReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListPolicyAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'batchListPolicyAttachments_maxResults' - The maximum number of results to retrieve.
--
-- 'nextToken', 'batchListPolicyAttachments_nextToken' - The pagination token.
--
-- 'policyReference', 'batchListPolicyAttachments_policyReference' - The reference that identifies the policy object.
newBatchListPolicyAttachments ::
  -- | 'policyReference'
  ObjectReference ->
  BatchListPolicyAttachments
newBatchListPolicyAttachments pPolicyReference_ =
  BatchListPolicyAttachments'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      policyReference = pPolicyReference_
    }

-- | The maximum number of results to retrieve.
batchListPolicyAttachments_maxResults :: Lens.Lens' BatchListPolicyAttachments (Prelude.Maybe Prelude.Natural)
batchListPolicyAttachments_maxResults = Lens.lens (\BatchListPolicyAttachments' {maxResults} -> maxResults) (\s@BatchListPolicyAttachments' {} a -> s {maxResults = a} :: BatchListPolicyAttachments)

-- | The pagination token.
batchListPolicyAttachments_nextToken :: Lens.Lens' BatchListPolicyAttachments (Prelude.Maybe Prelude.Text)
batchListPolicyAttachments_nextToken = Lens.lens (\BatchListPolicyAttachments' {nextToken} -> nextToken) (\s@BatchListPolicyAttachments' {} a -> s {nextToken = a} :: BatchListPolicyAttachments)

-- | The reference that identifies the policy object.
batchListPolicyAttachments_policyReference :: Lens.Lens' BatchListPolicyAttachments ObjectReference
batchListPolicyAttachments_policyReference = Lens.lens (\BatchListPolicyAttachments' {policyReference} -> policyReference) (\s@BatchListPolicyAttachments' {} a -> s {policyReference = a} :: BatchListPolicyAttachments)

instance Prelude.Hashable BatchListPolicyAttachments where
  hashWithSalt _salt BatchListPolicyAttachments' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` policyReference

instance Prelude.NFData BatchListPolicyAttachments where
  rnf BatchListPolicyAttachments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf policyReference

instance Data.ToJSON BatchListPolicyAttachments where
  toJSON BatchListPolicyAttachments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("PolicyReference" Data..= policyReference)
          ]
      )
