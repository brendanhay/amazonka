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
-- Module      : Amazonka.CloudDirectory.Types.BatchLookupPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchLookupPolicy where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Lists all policies from the root of the Directory to the object
-- specified inside a BatchRead operation. For more information, see
-- LookupPolicy and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchLookupPolicy' smart constructor.
data BatchLookupPolicy = BatchLookupPolicy'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Reference that identifies the object whose policies will be looked up.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
batchLookupPolicy_nextToken :: Lens.Lens' BatchLookupPolicy (Prelude.Maybe Prelude.Text)
batchLookupPolicy_nextToken = Lens.lens (\BatchLookupPolicy' {nextToken} -> nextToken) (\s@BatchLookupPolicy' {} a -> s {nextToken = a} :: BatchLookupPolicy)

-- | The maximum number of results to retrieve.
batchLookupPolicy_maxResults :: Lens.Lens' BatchLookupPolicy (Prelude.Maybe Prelude.Natural)
batchLookupPolicy_maxResults = Lens.lens (\BatchLookupPolicy' {maxResults} -> maxResults) (\s@BatchLookupPolicy' {} a -> s {maxResults = a} :: BatchLookupPolicy)

-- | Reference that identifies the object whose policies will be looked up.
batchLookupPolicy_objectReference :: Lens.Lens' BatchLookupPolicy ObjectReference
batchLookupPolicy_objectReference = Lens.lens (\BatchLookupPolicy' {objectReference} -> objectReference) (\s@BatchLookupPolicy' {} a -> s {objectReference = a} :: BatchLookupPolicy)

instance Prelude.Hashable BatchLookupPolicy where
  hashWithSalt _salt BatchLookupPolicy' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchLookupPolicy where
  rnf BatchLookupPolicy' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf objectReference

instance Core.ToJSON BatchLookupPolicy where
  toJSON BatchLookupPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
