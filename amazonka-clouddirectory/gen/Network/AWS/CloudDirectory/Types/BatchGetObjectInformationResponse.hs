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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a GetObjectInformation response operation.
--
-- /See:/ 'newBatchGetObjectInformationResponse' smart constructor.
data BatchGetObjectInformationResponse = BatchGetObjectInformationResponse'
  { -- | The facets attached to the specified object.
    schemaFacets :: Core.Maybe [SchemaFacet],
    -- | The @ObjectIdentifier@ of the specified object.
    objectIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      objectIdentifier = Core.Nothing
    }

-- | The facets attached to the specified object.
batchGetObjectInformationResponse_schemaFacets :: Lens.Lens' BatchGetObjectInformationResponse (Core.Maybe [SchemaFacet])
batchGetObjectInformationResponse_schemaFacets = Lens.lens (\BatchGetObjectInformationResponse' {schemaFacets} -> schemaFacets) (\s@BatchGetObjectInformationResponse' {} a -> s {schemaFacets = a} :: BatchGetObjectInformationResponse) Core.. Lens.mapping Lens._Coerce

-- | The @ObjectIdentifier@ of the specified object.
batchGetObjectInformationResponse_objectIdentifier :: Lens.Lens' BatchGetObjectInformationResponse (Core.Maybe Core.Text)
batchGetObjectInformationResponse_objectIdentifier = Lens.lens (\BatchGetObjectInformationResponse' {objectIdentifier} -> objectIdentifier) (\s@BatchGetObjectInformationResponse' {} a -> s {objectIdentifier = a} :: BatchGetObjectInformationResponse)

instance
  Core.FromJSON
    BatchGetObjectInformationResponse
  where
  parseJSON =
    Core.withObject
      "BatchGetObjectInformationResponse"
      ( \x ->
          BatchGetObjectInformationResponse'
            Core.<$> (x Core..:? "SchemaFacets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ObjectIdentifier")
      )

instance
  Core.Hashable
    BatchGetObjectInformationResponse

instance
  Core.NFData
    BatchGetObjectInformationResponse
