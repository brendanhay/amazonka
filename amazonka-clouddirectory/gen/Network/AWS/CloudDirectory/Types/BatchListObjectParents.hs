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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParents where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newBatchListObjectParents' smart constructor.
data BatchListObjectParents = BatchListObjectParents'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectParents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectParents_nextToken' - Undocumented member.
--
-- 'maxResults', 'batchListObjectParents_maxResults' - Undocumented member.
--
-- 'objectReference', 'batchListObjectParents_objectReference' - Undocumented member.
newBatchListObjectParents ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectParents
newBatchListObjectParents pObjectReference_ =
  BatchListObjectParents'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      objectReference = pObjectReference_
    }

-- | Undocumented member.
batchListObjectParents_nextToken :: Lens.Lens' BatchListObjectParents (Prelude.Maybe Prelude.Text)
batchListObjectParents_nextToken = Lens.lens (\BatchListObjectParents' {nextToken} -> nextToken) (\s@BatchListObjectParents' {} a -> s {nextToken = a} :: BatchListObjectParents)

-- | Undocumented member.
batchListObjectParents_maxResults :: Lens.Lens' BatchListObjectParents (Prelude.Maybe Prelude.Natural)
batchListObjectParents_maxResults = Lens.lens (\BatchListObjectParents' {maxResults} -> maxResults) (\s@BatchListObjectParents' {} a -> s {maxResults = a} :: BatchListObjectParents)

-- | Undocumented member.
batchListObjectParents_objectReference :: Lens.Lens' BatchListObjectParents ObjectReference
batchListObjectParents_objectReference = Lens.lens (\BatchListObjectParents' {objectReference} -> objectReference) (\s@BatchListObjectParents' {} a -> s {objectReference = a} :: BatchListObjectParents)

instance Prelude.Hashable BatchListObjectParents

instance Prelude.NFData BatchListObjectParents

instance Core.ToJSON BatchListObjectParents where
  toJSON BatchListObjectParents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
