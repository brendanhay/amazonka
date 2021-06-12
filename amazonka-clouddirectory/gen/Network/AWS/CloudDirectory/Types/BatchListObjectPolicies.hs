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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectPolicies where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns policies attached to an object in pagination fashion inside a
-- BatchRead operation. For more information, see ListObjectPolicies and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListObjectPolicies' smart constructor.
data BatchListObjectPolicies = BatchListObjectPolicies'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The reference that identifies the object whose attributes will be
    -- listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchListObjectPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectPolicies_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListObjectPolicies_maxResults' - The maximum number of results to retrieve.
--
-- 'objectReference', 'batchListObjectPolicies_objectReference' - The reference that identifies the object whose attributes will be
-- listed.
newBatchListObjectPolicies ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectPolicies
newBatchListObjectPolicies pObjectReference_ =
  BatchListObjectPolicies'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
batchListObjectPolicies_nextToken :: Lens.Lens' BatchListObjectPolicies (Core.Maybe Core.Text)
batchListObjectPolicies_nextToken = Lens.lens (\BatchListObjectPolicies' {nextToken} -> nextToken) (\s@BatchListObjectPolicies' {} a -> s {nextToken = a} :: BatchListObjectPolicies)

-- | The maximum number of results to retrieve.
batchListObjectPolicies_maxResults :: Lens.Lens' BatchListObjectPolicies (Core.Maybe Core.Natural)
batchListObjectPolicies_maxResults = Lens.lens (\BatchListObjectPolicies' {maxResults} -> maxResults) (\s@BatchListObjectPolicies' {} a -> s {maxResults = a} :: BatchListObjectPolicies)

-- | The reference that identifies the object whose attributes will be
-- listed.
batchListObjectPolicies_objectReference :: Lens.Lens' BatchListObjectPolicies ObjectReference
batchListObjectPolicies_objectReference = Lens.lens (\BatchListObjectPolicies' {objectReference} -> objectReference) (\s@BatchListObjectPolicies' {} a -> s {objectReference = a} :: BatchListObjectPolicies)

instance Core.Hashable BatchListObjectPolicies

instance Core.NFData BatchListObjectPolicies

instance Core.ToJSON BatchListObjectPolicies where
  toJSON BatchListObjectPolicies' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
