{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse where

import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a GetObjectInformation response operation.
--
-- /See:/ 'newBatchGetObjectInformationResponse' smart constructor.
data BatchGetObjectInformationResponse = BatchGetObjectInformationResponse'
  { -- | The facets attached to the specified object.
    schemaFacets :: Prelude.Maybe [SchemaFacet],
    -- | The @ObjectIdentifier@ of the specified object.
    objectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchGetObjectInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaFacets', 'batchGetObjectInformationResponse_schemaFacets' - The facets attached to the specified object.
--
-- 'objectIdentifier', 'batchGetObjectInformationResponse_objectIdentifier' - The @ObjectIdentifier@ of the specified object.
newBatchGetObjectInformationResponse ::
  BatchGetObjectInformationResponse
newBatchGetObjectInformationResponse =
  BatchGetObjectInformationResponse'
    { schemaFacets =
        Prelude.Nothing,
      objectIdentifier = Prelude.Nothing
    }

-- | The facets attached to the specified object.
batchGetObjectInformationResponse_schemaFacets :: Lens.Lens' BatchGetObjectInformationResponse (Prelude.Maybe [SchemaFacet])
batchGetObjectInformationResponse_schemaFacets = Lens.lens (\BatchGetObjectInformationResponse' {schemaFacets} -> schemaFacets) (\s@BatchGetObjectInformationResponse' {} a -> s {schemaFacets = a} :: BatchGetObjectInformationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The @ObjectIdentifier@ of the specified object.
batchGetObjectInformationResponse_objectIdentifier :: Lens.Lens' BatchGetObjectInformationResponse (Prelude.Maybe Prelude.Text)
batchGetObjectInformationResponse_objectIdentifier = Lens.lens (\BatchGetObjectInformationResponse' {objectIdentifier} -> objectIdentifier) (\s@BatchGetObjectInformationResponse' {} a -> s {objectIdentifier = a} :: BatchGetObjectInformationResponse)

instance
  Prelude.FromJSON
    BatchGetObjectInformationResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchGetObjectInformationResponse"
      ( \x ->
          BatchGetObjectInformationResponse'
            Prelude.<$> ( x Prelude..:? "SchemaFacets"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ObjectIdentifier")
      )

instance
  Prelude.Hashable
    BatchGetObjectInformationResponse

instance
  Prelude.NFData
    BatchGetObjectInformationResponse
