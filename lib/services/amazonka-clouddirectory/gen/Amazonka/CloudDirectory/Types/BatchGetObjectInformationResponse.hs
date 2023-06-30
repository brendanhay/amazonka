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
-- Module      : Amazonka.CloudDirectory.Types.BatchGetObjectInformationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchGetObjectInformationResponse where

import Amazonka.CloudDirectory.Types.SchemaFacet
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a GetObjectInformation response operation.
--
-- /See:/ 'newBatchGetObjectInformationResponse' smart constructor.
data BatchGetObjectInformationResponse = BatchGetObjectInformationResponse'
  { -- | The @ObjectIdentifier@ of the specified object.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The facets attached to the specified object.
    schemaFacets :: Prelude.Maybe [SchemaFacet]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetObjectInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'batchGetObjectInformationResponse_objectIdentifier' - The @ObjectIdentifier@ of the specified object.
--
-- 'schemaFacets', 'batchGetObjectInformationResponse_schemaFacets' - The facets attached to the specified object.
newBatchGetObjectInformationResponse ::
  BatchGetObjectInformationResponse
newBatchGetObjectInformationResponse =
  BatchGetObjectInformationResponse'
    { objectIdentifier =
        Prelude.Nothing,
      schemaFacets = Prelude.Nothing
    }

-- | The @ObjectIdentifier@ of the specified object.
batchGetObjectInformationResponse_objectIdentifier :: Lens.Lens' BatchGetObjectInformationResponse (Prelude.Maybe Prelude.Text)
batchGetObjectInformationResponse_objectIdentifier = Lens.lens (\BatchGetObjectInformationResponse' {objectIdentifier} -> objectIdentifier) (\s@BatchGetObjectInformationResponse' {} a -> s {objectIdentifier = a} :: BatchGetObjectInformationResponse)

-- | The facets attached to the specified object.
batchGetObjectInformationResponse_schemaFacets :: Lens.Lens' BatchGetObjectInformationResponse (Prelude.Maybe [SchemaFacet])
batchGetObjectInformationResponse_schemaFacets = Lens.lens (\BatchGetObjectInformationResponse' {schemaFacets} -> schemaFacets) (\s@BatchGetObjectInformationResponse' {} a -> s {schemaFacets = a} :: BatchGetObjectInformationResponse) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    BatchGetObjectInformationResponse
  where
  parseJSON =
    Data.withObject
      "BatchGetObjectInformationResponse"
      ( \x ->
          BatchGetObjectInformationResponse'
            Prelude.<$> (x Data..:? "ObjectIdentifier")
            Prelude.<*> (x Data..:? "SchemaFacets" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchGetObjectInformationResponse
  where
  hashWithSalt
    _salt
    BatchGetObjectInformationResponse' {..} =
      _salt
        `Prelude.hashWithSalt` objectIdentifier
        `Prelude.hashWithSalt` schemaFacets

instance
  Prelude.NFData
    BatchGetObjectInformationResponse
  where
  rnf BatchGetObjectInformationResponse' {..} =
    Prelude.rnf objectIdentifier
      `Prelude.seq` Prelude.rnf schemaFacets
