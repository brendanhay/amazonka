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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectChildrenResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectChildrenResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListObjectChildren response operation.
--
-- /See:/ 'newBatchListObjectChildrenResponse' smart constructor.
data BatchListObjectChildrenResponse = BatchListObjectChildrenResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The children structure, which is a map with the key as the @LinkName@
    -- and @ObjectIdentifier@ as the value.
    children :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectChildrenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectChildrenResponse_nextToken' - The pagination token.
--
-- 'children', 'batchListObjectChildrenResponse_children' - The children structure, which is a map with the key as the @LinkName@
-- and @ObjectIdentifier@ as the value.
newBatchListObjectChildrenResponse ::
  BatchListObjectChildrenResponse
newBatchListObjectChildrenResponse =
  BatchListObjectChildrenResponse'
    { nextToken =
        Prelude.Nothing,
      children = Prelude.Nothing
    }

-- | The pagination token.
batchListObjectChildrenResponse_nextToken :: Lens.Lens' BatchListObjectChildrenResponse (Prelude.Maybe Prelude.Text)
batchListObjectChildrenResponse_nextToken = Lens.lens (\BatchListObjectChildrenResponse' {nextToken} -> nextToken) (\s@BatchListObjectChildrenResponse' {} a -> s {nextToken = a} :: BatchListObjectChildrenResponse)

-- | The children structure, which is a map with the key as the @LinkName@
-- and @ObjectIdentifier@ as the value.
batchListObjectChildrenResponse_children :: Lens.Lens' BatchListObjectChildrenResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
batchListObjectChildrenResponse_children = Lens.lens (\BatchListObjectChildrenResponse' {children} -> children) (\s@BatchListObjectChildrenResponse' {} a -> s {children = a} :: BatchListObjectChildrenResponse) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    BatchListObjectChildrenResponse
  where
  parseJSON =
    Core.withObject
      "BatchListObjectChildrenResponse"
      ( \x ->
          BatchListObjectChildrenResponse'
            Prelude.<$> (x Core..:? "NextToken")
            Prelude.<*> (x Core..:? "Children" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchListObjectChildrenResponse
  where
  hashWithSalt
    _salt
    BatchListObjectChildrenResponse' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` children

instance
  Prelude.NFData
    BatchListObjectChildrenResponse
  where
  rnf BatchListObjectChildrenResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf children
