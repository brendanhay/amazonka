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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndices where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Lists indices attached to an object inside a BatchRead operation. For
-- more information, see ListAttachedIndices and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListAttachedIndices' smart constructor.
data BatchListAttachedIndices = BatchListAttachedIndices'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A reference to the object that has indices attached.
    targetReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListAttachedIndices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListAttachedIndices_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListAttachedIndices_maxResults' - The maximum number of results to retrieve.
--
-- 'targetReference', 'batchListAttachedIndices_targetReference' - A reference to the object that has indices attached.
newBatchListAttachedIndices ::
  -- | 'targetReference'
  ObjectReference ->
  BatchListAttachedIndices
newBatchListAttachedIndices pTargetReference_ =
  BatchListAttachedIndices'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      targetReference = pTargetReference_
    }

-- | The pagination token.
batchListAttachedIndices_nextToken :: Lens.Lens' BatchListAttachedIndices (Prelude.Maybe Prelude.Text)
batchListAttachedIndices_nextToken = Lens.lens (\BatchListAttachedIndices' {nextToken} -> nextToken) (\s@BatchListAttachedIndices' {} a -> s {nextToken = a} :: BatchListAttachedIndices)

-- | The maximum number of results to retrieve.
batchListAttachedIndices_maxResults :: Lens.Lens' BatchListAttachedIndices (Prelude.Maybe Prelude.Natural)
batchListAttachedIndices_maxResults = Lens.lens (\BatchListAttachedIndices' {maxResults} -> maxResults) (\s@BatchListAttachedIndices' {} a -> s {maxResults = a} :: BatchListAttachedIndices)

-- | A reference to the object that has indices attached.
batchListAttachedIndices_targetReference :: Lens.Lens' BatchListAttachedIndices ObjectReference
batchListAttachedIndices_targetReference = Lens.lens (\BatchListAttachedIndices' {targetReference} -> targetReference) (\s@BatchListAttachedIndices' {} a -> s {targetReference = a} :: BatchListAttachedIndices)

instance Prelude.Hashable BatchListAttachedIndices

instance Prelude.NFData BatchListAttachedIndices

instance Core.ToJSON BatchListAttachedIndices where
  toJSON BatchListAttachedIndices' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("TargetReference" Core..= targetReference)
          ]
      )
