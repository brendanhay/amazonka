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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectChildren
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectChildren where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListObjectChildren operation.
--
-- /See:/ 'newBatchListObjectChildren' smart constructor.
data BatchListObjectChildren = BatchListObjectChildren'
  { -- | Maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Reference of the object for which child objects are being listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectChildren' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'batchListObjectChildren_maxResults' - Maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'nextToken', 'batchListObjectChildren_nextToken' - The pagination token.
--
-- 'objectReference', 'batchListObjectChildren_objectReference' - Reference of the object for which child objects are being listed.
newBatchListObjectChildren ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectChildren
newBatchListObjectChildren pObjectReference_ =
  BatchListObjectChildren'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      objectReference = pObjectReference_
    }

-- | Maximum number of items to be retrieved in a single call. This is an
-- approximate number.
batchListObjectChildren_maxResults :: Lens.Lens' BatchListObjectChildren (Prelude.Maybe Prelude.Natural)
batchListObjectChildren_maxResults = Lens.lens (\BatchListObjectChildren' {maxResults} -> maxResults) (\s@BatchListObjectChildren' {} a -> s {maxResults = a} :: BatchListObjectChildren)

-- | The pagination token.
batchListObjectChildren_nextToken :: Lens.Lens' BatchListObjectChildren (Prelude.Maybe Prelude.Text)
batchListObjectChildren_nextToken = Lens.lens (\BatchListObjectChildren' {nextToken} -> nextToken) (\s@BatchListObjectChildren' {} a -> s {nextToken = a} :: BatchListObjectChildren)

-- | Reference of the object for which child objects are being listed.
batchListObjectChildren_objectReference :: Lens.Lens' BatchListObjectChildren ObjectReference
batchListObjectChildren_objectReference = Lens.lens (\BatchListObjectChildren' {objectReference} -> objectReference) (\s@BatchListObjectChildren' {} a -> s {objectReference = a} :: BatchListObjectChildren)

instance Prelude.Hashable BatchListObjectChildren where
  hashWithSalt _salt BatchListObjectChildren' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchListObjectChildren where
  rnf BatchListObjectChildren' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToJSON BatchListObjectChildren where
  toJSON BatchListObjectChildren' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
