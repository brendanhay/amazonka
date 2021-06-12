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
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchLookupPolicy where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Lists all policies from the root of the Directory to the object
-- specified inside a BatchRead operation. For more information, see
-- LookupPolicy and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchLookupPolicy' smart constructor.
data BatchLookupPolicy = BatchLookupPolicy'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | Reference that identifies the object whose policies will be looked up.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchLookupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchLookupPolicy_nextToken' - The pagination token.
--
-- 'maxResults', 'batchLookupPolicy_maxResults' - The maximum number of results to retrieve.
--
-- 'objectReference', 'batchLookupPolicy_objectReference' - Reference that identifies the object whose policies will be looked up.
newBatchLookupPolicy ::
  -- | 'objectReference'
  ObjectReference ->
  BatchLookupPolicy
newBatchLookupPolicy pObjectReference_ =
  BatchLookupPolicy'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
batchLookupPolicy_nextToken :: Lens.Lens' BatchLookupPolicy (Core.Maybe Core.Text)
batchLookupPolicy_nextToken = Lens.lens (\BatchLookupPolicy' {nextToken} -> nextToken) (\s@BatchLookupPolicy' {} a -> s {nextToken = a} :: BatchLookupPolicy)

-- | The maximum number of results to retrieve.
batchLookupPolicy_maxResults :: Lens.Lens' BatchLookupPolicy (Core.Maybe Core.Natural)
batchLookupPolicy_maxResults = Lens.lens (\BatchLookupPolicy' {maxResults} -> maxResults) (\s@BatchLookupPolicy' {} a -> s {maxResults = a} :: BatchLookupPolicy)

-- | Reference that identifies the object whose policies will be looked up.
batchLookupPolicy_objectReference :: Lens.Lens' BatchLookupPolicy ObjectReference
batchLookupPolicy_objectReference = Lens.lens (\BatchLookupPolicy' {objectReference} -> objectReference) (\s@BatchLookupPolicy' {} a -> s {objectReference = a} :: BatchLookupPolicy)

instance Core.Hashable BatchLookupPolicy

instance Core.NFData BatchLookupPolicy

instance Core.ToJSON BatchLookupPolicy where
  toJSON BatchLookupPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
