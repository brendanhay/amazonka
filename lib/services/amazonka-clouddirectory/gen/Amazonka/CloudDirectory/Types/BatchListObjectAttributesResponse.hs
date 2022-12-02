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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectAttributesResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectAttributesResponse where

import Amazonka.CloudDirectory.Types.AttributeKeyAndValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListObjectAttributes response operation.
--
-- /See:/ 'newBatchListObjectAttributesResponse' smart constructor.
data BatchListObjectAttributesResponse = BatchListObjectAttributesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The attributes map that is associated with the object. @AttributeArn@ is
    -- the key; attribute value is the value.
    attributes :: Prelude.Maybe [AttributeKeyAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectAttributesResponse_nextToken' - The pagination token.
--
-- 'attributes', 'batchListObjectAttributesResponse_attributes' - The attributes map that is associated with the object. @AttributeArn@ is
-- the key; attribute value is the value.
newBatchListObjectAttributesResponse ::
  BatchListObjectAttributesResponse
newBatchListObjectAttributesResponse =
  BatchListObjectAttributesResponse'
    { nextToken =
        Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | The pagination token.
batchListObjectAttributesResponse_nextToken :: Lens.Lens' BatchListObjectAttributesResponse (Prelude.Maybe Prelude.Text)
batchListObjectAttributesResponse_nextToken = Lens.lens (\BatchListObjectAttributesResponse' {nextToken} -> nextToken) (\s@BatchListObjectAttributesResponse' {} a -> s {nextToken = a} :: BatchListObjectAttributesResponse)

-- | The attributes map that is associated with the object. @AttributeArn@ is
-- the key; attribute value is the value.
batchListObjectAttributesResponse_attributes :: Lens.Lens' BatchListObjectAttributesResponse (Prelude.Maybe [AttributeKeyAndValue])
batchListObjectAttributesResponse_attributes = Lens.lens (\BatchListObjectAttributesResponse' {attributes} -> attributes) (\s@BatchListObjectAttributesResponse' {} a -> s {attributes = a} :: BatchListObjectAttributesResponse) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    BatchListObjectAttributesResponse
  where
  parseJSON =
    Data.withObject
      "BatchListObjectAttributesResponse"
      ( \x ->
          BatchListObjectAttributesResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Attributes" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchListObjectAttributesResponse
  where
  hashWithSalt
    _salt
    BatchListObjectAttributesResponse' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` attributes

instance
  Prelude.NFData
    BatchListObjectAttributesResponse
  where
  rnf BatchListObjectAttributesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attributes
