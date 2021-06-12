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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Retrieves all available parent paths for any object type such as node,
-- leaf node, policy node, and index node objects inside a BatchRead
-- operation. For more information, see ListObjectParentPaths and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListObjectParentPaths' smart constructor.
data BatchListObjectParentPaths = BatchListObjectParentPaths'
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
-- Create a value of 'BatchListObjectParentPaths' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectParentPaths_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListObjectParentPaths_maxResults' - The maximum number of results to retrieve.
--
-- 'objectReference', 'batchListObjectParentPaths_objectReference' - The reference that identifies the object whose attributes will be
-- listed.
newBatchListObjectParentPaths ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectParentPaths
newBatchListObjectParentPaths pObjectReference_ =
  BatchListObjectParentPaths'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
batchListObjectParentPaths_nextToken :: Lens.Lens' BatchListObjectParentPaths (Core.Maybe Core.Text)
batchListObjectParentPaths_nextToken = Lens.lens (\BatchListObjectParentPaths' {nextToken} -> nextToken) (\s@BatchListObjectParentPaths' {} a -> s {nextToken = a} :: BatchListObjectParentPaths)

-- | The maximum number of results to retrieve.
batchListObjectParentPaths_maxResults :: Lens.Lens' BatchListObjectParentPaths (Core.Maybe Core.Natural)
batchListObjectParentPaths_maxResults = Lens.lens (\BatchListObjectParentPaths' {maxResults} -> maxResults) (\s@BatchListObjectParentPaths' {} a -> s {maxResults = a} :: BatchListObjectParentPaths)

-- | The reference that identifies the object whose attributes will be
-- listed.
batchListObjectParentPaths_objectReference :: Lens.Lens' BatchListObjectParentPaths ObjectReference
batchListObjectParentPaths_objectReference = Lens.lens (\BatchListObjectParentPaths' {objectReference} -> objectReference) (\s@BatchListObjectParentPaths' {} a -> s {objectReference = a} :: BatchListObjectParentPaths)

instance Core.Hashable BatchListObjectParentPaths

instance Core.NFData BatchListObjectParentPaths

instance Core.ToJSON BatchListObjectParentPaths where
  toJSON BatchListObjectParentPaths' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
