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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildren
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectChildren where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a ListObjectChildren operation.
--
-- /See:/ 'newBatchListObjectChildren' smart constructor.
data BatchListObjectChildren = BatchListObjectChildren'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | Reference of the object for which child objects are being listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchListObjectChildren' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectChildren_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListObjectChildren_maxResults' - Maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'objectReference', 'batchListObjectChildren_objectReference' - Reference of the object for which child objects are being listed.
newBatchListObjectChildren ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectChildren
newBatchListObjectChildren pObjectReference_ =
  BatchListObjectChildren'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
batchListObjectChildren_nextToken :: Lens.Lens' BatchListObjectChildren (Core.Maybe Core.Text)
batchListObjectChildren_nextToken = Lens.lens (\BatchListObjectChildren' {nextToken} -> nextToken) (\s@BatchListObjectChildren' {} a -> s {nextToken = a} :: BatchListObjectChildren)

-- | Maximum number of items to be retrieved in a single call. This is an
-- approximate number.
batchListObjectChildren_maxResults :: Lens.Lens' BatchListObjectChildren (Core.Maybe Core.Natural)
batchListObjectChildren_maxResults = Lens.lens (\BatchListObjectChildren' {maxResults} -> maxResults) (\s@BatchListObjectChildren' {} a -> s {maxResults = a} :: BatchListObjectChildren)

-- | Reference of the object for which child objects are being listed.
batchListObjectChildren_objectReference :: Lens.Lens' BatchListObjectChildren ObjectReference
batchListObjectChildren_objectReference = Lens.lens (\BatchListObjectChildren' {objectReference} -> objectReference) (\s@BatchListObjectChildren' {} a -> s {objectReference = a} :: BatchListObjectChildren)

instance Core.Hashable BatchListObjectChildren

instance Core.NFData BatchListObjectChildren

instance Core.ToJSON BatchListObjectChildren where
  toJSON BatchListObjectChildren' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
