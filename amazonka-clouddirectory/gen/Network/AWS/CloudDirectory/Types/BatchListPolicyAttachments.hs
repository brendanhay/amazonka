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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is
-- attached inside a BatchRead operation. For more information, see
-- ListPolicyAttachments and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListPolicyAttachments' smart constructor.
data BatchListPolicyAttachments = BatchListPolicyAttachments'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'batchListPolicyAttachments_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListPolicyAttachments_maxResults' - The maximum number of results to retrieve.
--
-- 'policyReference', 'batchListPolicyAttachments_policyReference' - The reference that identifies the policy object.
newBatchListPolicyAttachments ::
  -- | 'policyReference'
  ObjectReference ->
  BatchListPolicyAttachments
newBatchListPolicyAttachments pPolicyReference_ =
  BatchListPolicyAttachments'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      policyReference = pPolicyReference_
    }

-- | The pagination token.
batchListPolicyAttachments_nextToken :: Lens.Lens' BatchListPolicyAttachments (Prelude.Maybe Prelude.Text)
batchListPolicyAttachments_nextToken = Lens.lens (\BatchListPolicyAttachments' {nextToken} -> nextToken) (\s@BatchListPolicyAttachments' {} a -> s {nextToken = a} :: BatchListPolicyAttachments)

-- | The maximum number of results to retrieve.
batchListPolicyAttachments_maxResults :: Lens.Lens' BatchListPolicyAttachments (Prelude.Maybe Prelude.Natural)
batchListPolicyAttachments_maxResults = Lens.lens (\BatchListPolicyAttachments' {maxResults} -> maxResults) (\s@BatchListPolicyAttachments' {} a -> s {maxResults = a} :: BatchListPolicyAttachments)

-- | The reference that identifies the policy object.
batchListPolicyAttachments_policyReference :: Lens.Lens' BatchListPolicyAttachments ObjectReference
batchListPolicyAttachments_policyReference = Lens.lens (\BatchListPolicyAttachments' {policyReference} -> policyReference) (\s@BatchListPolicyAttachments' {} a -> s {policyReference = a} :: BatchListPolicyAttachments)

instance Prelude.Hashable BatchListPolicyAttachments

instance Prelude.NFData BatchListPolicyAttachments

instance Core.ToJSON BatchListPolicyAttachments where
  toJSON BatchListPolicyAttachments' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("PolicyReference" Core..= policyReference)
          ]
      )
