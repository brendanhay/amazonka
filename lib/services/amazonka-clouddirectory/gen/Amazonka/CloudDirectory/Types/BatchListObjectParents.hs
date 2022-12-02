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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectParents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectParents where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lists parent objects that are associated with a given object in
-- pagination fashion.
--
-- /See:/ 'newBatchListObjectParents' smart constructor.
data BatchListObjectParents = BatchListObjectParents'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
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
-- 'nextToken', 'batchListObjectParents_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListObjectParents_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
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

-- | The pagination token.
batchListObjectParents_nextToken :: Lens.Lens' BatchListObjectParents (Prelude.Maybe Prelude.Text)
batchListObjectParents_nextToken = Lens.lens (\BatchListObjectParents' {nextToken} -> nextToken) (\s@BatchListObjectParents' {} a -> s {nextToken = a} :: BatchListObjectParents)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
batchListObjectParents_maxResults :: Lens.Lens' BatchListObjectParents (Prelude.Maybe Prelude.Natural)
batchListObjectParents_maxResults = Lens.lens (\BatchListObjectParents' {maxResults} -> maxResults) (\s@BatchListObjectParents' {} a -> s {maxResults = a} :: BatchListObjectParents)

-- | Undocumented member.
batchListObjectParents_objectReference :: Lens.Lens' BatchListObjectParents ObjectReference
batchListObjectParents_objectReference = Lens.lens (\BatchListObjectParents' {objectReference} -> objectReference) (\s@BatchListObjectParents' {} a -> s {objectReference = a} :: BatchListObjectParents)

instance Prelude.Hashable BatchListObjectParents where
  hashWithSalt _salt BatchListObjectParents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchListObjectParents where
  rnf BatchListObjectParents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToJSON BatchListObjectParents where
  toJSON BatchListObjectParents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
