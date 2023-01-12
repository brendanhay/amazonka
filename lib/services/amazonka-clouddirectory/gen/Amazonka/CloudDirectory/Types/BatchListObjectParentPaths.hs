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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectParentPaths
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectParentPaths where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Retrieves all available parent paths for any object type such as node,
-- leaf node, policy node, and index node objects inside a BatchRead
-- operation. For more information, see ListObjectParentPaths and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListObjectParentPaths' smart constructor.
data BatchListObjectParentPaths = BatchListObjectParentPaths'
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
-- Create a value of 'BatchListObjectParentPaths' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'batchListObjectParentPaths_maxResults' - The maximum number of results to retrieve.
--
-- 'nextToken', 'batchListObjectParentPaths_nextToken' - The pagination token.
--
-- 'objectReference', 'batchListObjectParentPaths_objectReference' - The reference that identifies the object whose attributes will be
-- listed.
newBatchListObjectParentPaths ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectParentPaths
newBatchListObjectParentPaths pObjectReference_ =
  BatchListObjectParentPaths'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      objectReference = pObjectReference_
    }

-- | The maximum number of results to retrieve.
batchListObjectParentPaths_maxResults :: Lens.Lens' BatchListObjectParentPaths (Prelude.Maybe Prelude.Natural)
batchListObjectParentPaths_maxResults = Lens.lens (\BatchListObjectParentPaths' {maxResults} -> maxResults) (\s@BatchListObjectParentPaths' {} a -> s {maxResults = a} :: BatchListObjectParentPaths)

-- | The pagination token.
batchListObjectParentPaths_nextToken :: Lens.Lens' BatchListObjectParentPaths (Prelude.Maybe Prelude.Text)
batchListObjectParentPaths_nextToken = Lens.lens (\BatchListObjectParentPaths' {nextToken} -> nextToken) (\s@BatchListObjectParentPaths' {} a -> s {nextToken = a} :: BatchListObjectParentPaths)

-- | The reference that identifies the object whose attributes will be
-- listed.
batchListObjectParentPaths_objectReference :: Lens.Lens' BatchListObjectParentPaths ObjectReference
batchListObjectParentPaths_objectReference = Lens.lens (\BatchListObjectParentPaths' {objectReference} -> objectReference) (\s@BatchListObjectParentPaths' {} a -> s {objectReference = a} :: BatchListObjectParentPaths)

instance Prelude.Hashable BatchListObjectParentPaths where
  hashWithSalt _salt BatchListObjectParentPaths' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchListObjectParentPaths where
  rnf BatchListObjectParentPaths' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToJSON BatchListObjectParentPaths where
  toJSON BatchListObjectParentPaths' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
