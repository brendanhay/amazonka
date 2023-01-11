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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectPolicies where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns policies attached to an object in pagination fashion inside a
-- BatchRead operation. For more information, see ListObjectPolicies and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListObjectPolicies' smart constructor.
data BatchListObjectPolicies = BatchListObjectPolicies'
  { -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The reference that identifies the object whose attributes will be
    -- listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'batchListObjectPolicies_maxResults' - The maximum number of results to retrieve.
--
-- 'nextToken', 'batchListObjectPolicies_nextToken' - The pagination token.
--
-- 'objectReference', 'batchListObjectPolicies_objectReference' - The reference that identifies the object whose attributes will be
-- listed.
newBatchListObjectPolicies ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectPolicies
newBatchListObjectPolicies pObjectReference_ =
  BatchListObjectPolicies'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      objectReference = pObjectReference_
    }

-- | The maximum number of results to retrieve.
batchListObjectPolicies_maxResults :: Lens.Lens' BatchListObjectPolicies (Prelude.Maybe Prelude.Natural)
batchListObjectPolicies_maxResults = Lens.lens (\BatchListObjectPolicies' {maxResults} -> maxResults) (\s@BatchListObjectPolicies' {} a -> s {maxResults = a} :: BatchListObjectPolicies)

-- | The pagination token.
batchListObjectPolicies_nextToken :: Lens.Lens' BatchListObjectPolicies (Prelude.Maybe Prelude.Text)
batchListObjectPolicies_nextToken = Lens.lens (\BatchListObjectPolicies' {nextToken} -> nextToken) (\s@BatchListObjectPolicies' {} a -> s {nextToken = a} :: BatchListObjectPolicies)

-- | The reference that identifies the object whose attributes will be
-- listed.
batchListObjectPolicies_objectReference :: Lens.Lens' BatchListObjectPolicies ObjectReference
batchListObjectPolicies_objectReference = Lens.lens (\BatchListObjectPolicies' {objectReference} -> objectReference) (\s@BatchListObjectPolicies' {} a -> s {objectReference = a} :: BatchListObjectPolicies)

instance Prelude.Hashable BatchListObjectPolicies where
  hashWithSalt _salt BatchListObjectPolicies' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchListObjectPolicies where
  rnf BatchListObjectPolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToJSON BatchListObjectPolicies where
  toJSON BatchListObjectPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
