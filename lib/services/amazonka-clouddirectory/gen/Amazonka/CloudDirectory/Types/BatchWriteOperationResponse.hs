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
-- Module      : Amazonka.CloudDirectory.Types.BatchWriteOperationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchWriteOperationResponse where

import Amazonka.CloudDirectory.Types.BatchAddFacetToObjectResponse
import Amazonka.CloudDirectory.Types.BatchAttachObjectResponse
import Amazonka.CloudDirectory.Types.BatchAttachPolicyResponse
import Amazonka.CloudDirectory.Types.BatchAttachToIndexResponse
import Amazonka.CloudDirectory.Types.BatchAttachTypedLinkResponse
import Amazonka.CloudDirectory.Types.BatchCreateIndexResponse
import Amazonka.CloudDirectory.Types.BatchCreateObjectResponse
import Amazonka.CloudDirectory.Types.BatchDeleteObjectResponse
import Amazonka.CloudDirectory.Types.BatchDetachFromIndexResponse
import Amazonka.CloudDirectory.Types.BatchDetachObjectResponse
import Amazonka.CloudDirectory.Types.BatchDetachPolicyResponse
import Amazonka.CloudDirectory.Types.BatchDetachTypedLinkResponse
import Amazonka.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
import Amazonka.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
import Amazonka.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a @BatchWrite@ response operation.
--
-- /See:/ 'newBatchWriteOperationResponse' smart constructor.
data BatchWriteOperationResponse = BatchWriteOperationResponse'
  { -- | The result of an add facet to object batch operation.
    addFacetToObject :: Prelude.Maybe BatchAddFacetToObjectResponse,
    -- | Attaches an object to a Directory.
    attachObject :: Prelude.Maybe BatchAttachObjectResponse,
    -- | Attaches a policy object to a regular object. An object can have a
    -- limited number of attached policies.
    attachPolicy :: Prelude.Maybe BatchAttachPolicyResponse,
    -- | Attaches the specified object to the specified index.
    attachToIndex :: Prelude.Maybe BatchAttachToIndexResponse,
    -- | Attaches a typed link to a specified source and target object. For more
    -- information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    attachTypedLink :: Prelude.Maybe BatchAttachTypedLinkResponse,
    -- | Creates an index object. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
    -- for more information.
    createIndex :: Prelude.Maybe BatchCreateIndexResponse,
    -- | Creates an object in a Directory.
    createObject :: Prelude.Maybe BatchCreateObjectResponse,
    -- | Deletes an object in a Directory.
    deleteObject :: Prelude.Maybe BatchDeleteObjectResponse,
    -- | Detaches the specified object from the specified index.
    detachFromIndex :: Prelude.Maybe BatchDetachFromIndexResponse,
    -- | Detaches an object from a Directory.
    detachObject :: Prelude.Maybe BatchDetachObjectResponse,
    -- | Detaches a policy from a Directory.
    detachPolicy :: Prelude.Maybe BatchDetachPolicyResponse,
    -- | Detaches a typed link from a specified source and target object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    detachTypedLink :: Prelude.Maybe BatchDetachTypedLinkResponse,
    -- | The result of a batch remove facet from object operation.
    removeFacetFromObject :: Prelude.Maybe BatchRemoveFacetFromObjectResponse,
    -- | Represents the output of a @BatchWrite@ response operation.
    updateLinkAttributes :: Prelude.Maybe BatchUpdateLinkAttributesResponse,
    -- | Updates a given object’s attributes.
    updateObjectAttributes :: Prelude.Maybe BatchUpdateObjectAttributesResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchWriteOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addFacetToObject', 'batchWriteOperationResponse_addFacetToObject' - The result of an add facet to object batch operation.
--
-- 'attachObject', 'batchWriteOperationResponse_attachObject' - Attaches an object to a Directory.
--
-- 'attachPolicy', 'batchWriteOperationResponse_attachPolicy' - Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
--
-- 'attachToIndex', 'batchWriteOperationResponse_attachToIndex' - Attaches the specified object to the specified index.
--
-- 'attachTypedLink', 'batchWriteOperationResponse_attachTypedLink' - Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'createIndex', 'batchWriteOperationResponse_createIndex' - Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
--
-- 'createObject', 'batchWriteOperationResponse_createObject' - Creates an object in a Directory.
--
-- 'deleteObject', 'batchWriteOperationResponse_deleteObject' - Deletes an object in a Directory.
--
-- 'detachFromIndex', 'batchWriteOperationResponse_detachFromIndex' - Detaches the specified object from the specified index.
--
-- 'detachObject', 'batchWriteOperationResponse_detachObject' - Detaches an object from a Directory.
--
-- 'detachPolicy', 'batchWriteOperationResponse_detachPolicy' - Detaches a policy from a Directory.
--
-- 'detachTypedLink', 'batchWriteOperationResponse_detachTypedLink' - Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'removeFacetFromObject', 'batchWriteOperationResponse_removeFacetFromObject' - The result of a batch remove facet from object operation.
--
-- 'updateLinkAttributes', 'batchWriteOperationResponse_updateLinkAttributes' - Represents the output of a @BatchWrite@ response operation.
--
-- 'updateObjectAttributes', 'batchWriteOperationResponse_updateObjectAttributes' - Updates a given object’s attributes.
newBatchWriteOperationResponse ::
  BatchWriteOperationResponse
newBatchWriteOperationResponse =
  BatchWriteOperationResponse'
    { addFacetToObject =
        Prelude.Nothing,
      attachObject = Prelude.Nothing,
      attachPolicy = Prelude.Nothing,
      attachToIndex = Prelude.Nothing,
      attachTypedLink = Prelude.Nothing,
      createIndex = Prelude.Nothing,
      createObject = Prelude.Nothing,
      deleteObject = Prelude.Nothing,
      detachFromIndex = Prelude.Nothing,
      detachObject = Prelude.Nothing,
      detachPolicy = Prelude.Nothing,
      detachTypedLink = Prelude.Nothing,
      removeFacetFromObject = Prelude.Nothing,
      updateLinkAttributes = Prelude.Nothing,
      updateObjectAttributes = Prelude.Nothing
    }

-- | The result of an add facet to object batch operation.
batchWriteOperationResponse_addFacetToObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAddFacetToObjectResponse)
batchWriteOperationResponse_addFacetToObject = Lens.lens (\BatchWriteOperationResponse' {addFacetToObject} -> addFacetToObject) (\s@BatchWriteOperationResponse' {} a -> s {addFacetToObject = a} :: BatchWriteOperationResponse)

-- | Attaches an object to a Directory.
batchWriteOperationResponse_attachObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachObjectResponse)
batchWriteOperationResponse_attachObject = Lens.lens (\BatchWriteOperationResponse' {attachObject} -> attachObject) (\s@BatchWriteOperationResponse' {} a -> s {attachObject = a} :: BatchWriteOperationResponse)

-- | Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
batchWriteOperationResponse_attachPolicy :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachPolicyResponse)
batchWriteOperationResponse_attachPolicy = Lens.lens (\BatchWriteOperationResponse' {attachPolicy} -> attachPolicy) (\s@BatchWriteOperationResponse' {} a -> s {attachPolicy = a} :: BatchWriteOperationResponse)

-- | Attaches the specified object to the specified index.
batchWriteOperationResponse_attachToIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachToIndexResponse)
batchWriteOperationResponse_attachToIndex = Lens.lens (\BatchWriteOperationResponse' {attachToIndex} -> attachToIndex) (\s@BatchWriteOperationResponse' {} a -> s {attachToIndex = a} :: BatchWriteOperationResponse)

-- | Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperationResponse_attachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachTypedLinkResponse)
batchWriteOperationResponse_attachTypedLink = Lens.lens (\BatchWriteOperationResponse' {attachTypedLink} -> attachTypedLink) (\s@BatchWriteOperationResponse' {} a -> s {attachTypedLink = a} :: BatchWriteOperationResponse)

-- | Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
batchWriteOperationResponse_createIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchCreateIndexResponse)
batchWriteOperationResponse_createIndex = Lens.lens (\BatchWriteOperationResponse' {createIndex} -> createIndex) (\s@BatchWriteOperationResponse' {} a -> s {createIndex = a} :: BatchWriteOperationResponse)

-- | Creates an object in a Directory.
batchWriteOperationResponse_createObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchCreateObjectResponse)
batchWriteOperationResponse_createObject = Lens.lens (\BatchWriteOperationResponse' {createObject} -> createObject) (\s@BatchWriteOperationResponse' {} a -> s {createObject = a} :: BatchWriteOperationResponse)

-- | Deletes an object in a Directory.
batchWriteOperationResponse_deleteObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDeleteObjectResponse)
batchWriteOperationResponse_deleteObject = Lens.lens (\BatchWriteOperationResponse' {deleteObject} -> deleteObject) (\s@BatchWriteOperationResponse' {} a -> s {deleteObject = a} :: BatchWriteOperationResponse)

-- | Detaches the specified object from the specified index.
batchWriteOperationResponse_detachFromIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachFromIndexResponse)
batchWriteOperationResponse_detachFromIndex = Lens.lens (\BatchWriteOperationResponse' {detachFromIndex} -> detachFromIndex) (\s@BatchWriteOperationResponse' {} a -> s {detachFromIndex = a} :: BatchWriteOperationResponse)

-- | Detaches an object from a Directory.
batchWriteOperationResponse_detachObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachObjectResponse)
batchWriteOperationResponse_detachObject = Lens.lens (\BatchWriteOperationResponse' {detachObject} -> detachObject) (\s@BatchWriteOperationResponse' {} a -> s {detachObject = a} :: BatchWriteOperationResponse)

-- | Detaches a policy from a Directory.
batchWriteOperationResponse_detachPolicy :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachPolicyResponse)
batchWriteOperationResponse_detachPolicy = Lens.lens (\BatchWriteOperationResponse' {detachPolicy} -> detachPolicy) (\s@BatchWriteOperationResponse' {} a -> s {detachPolicy = a} :: BatchWriteOperationResponse)

-- | Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperationResponse_detachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachTypedLinkResponse)
batchWriteOperationResponse_detachTypedLink = Lens.lens (\BatchWriteOperationResponse' {detachTypedLink} -> detachTypedLink) (\s@BatchWriteOperationResponse' {} a -> s {detachTypedLink = a} :: BatchWriteOperationResponse)

-- | The result of a batch remove facet from object operation.
batchWriteOperationResponse_removeFacetFromObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchRemoveFacetFromObjectResponse)
batchWriteOperationResponse_removeFacetFromObject = Lens.lens (\BatchWriteOperationResponse' {removeFacetFromObject} -> removeFacetFromObject) (\s@BatchWriteOperationResponse' {} a -> s {removeFacetFromObject = a} :: BatchWriteOperationResponse)

-- | Represents the output of a @BatchWrite@ response operation.
batchWriteOperationResponse_updateLinkAttributes :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchUpdateLinkAttributesResponse)
batchWriteOperationResponse_updateLinkAttributes = Lens.lens (\BatchWriteOperationResponse' {updateLinkAttributes} -> updateLinkAttributes) (\s@BatchWriteOperationResponse' {} a -> s {updateLinkAttributes = a} :: BatchWriteOperationResponse)

-- | Updates a given object’s attributes.
batchWriteOperationResponse_updateObjectAttributes :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchUpdateObjectAttributesResponse)
batchWriteOperationResponse_updateObjectAttributes = Lens.lens (\BatchWriteOperationResponse' {updateObjectAttributes} -> updateObjectAttributes) (\s@BatchWriteOperationResponse' {} a -> s {updateObjectAttributes = a} :: BatchWriteOperationResponse)

instance Data.FromJSON BatchWriteOperationResponse where
  parseJSON =
    Data.withObject
      "BatchWriteOperationResponse"
      ( \x ->
          BatchWriteOperationResponse'
            Prelude.<$> (x Data..:? "AddFacetToObject")
            Prelude.<*> (x Data..:? "AttachObject")
            Prelude.<*> (x Data..:? "AttachPolicy")
            Prelude.<*> (x Data..:? "AttachToIndex")
            Prelude.<*> (x Data..:? "AttachTypedLink")
            Prelude.<*> (x Data..:? "CreateIndex")
            Prelude.<*> (x Data..:? "CreateObject")
            Prelude.<*> (x Data..:? "DeleteObject")
            Prelude.<*> (x Data..:? "DetachFromIndex")
            Prelude.<*> (x Data..:? "DetachObject")
            Prelude.<*> (x Data..:? "DetachPolicy")
            Prelude.<*> (x Data..:? "DetachTypedLink")
            Prelude.<*> (x Data..:? "RemoveFacetFromObject")
            Prelude.<*> (x Data..:? "UpdateLinkAttributes")
            Prelude.<*> (x Data..:? "UpdateObjectAttributes")
      )

instance Prelude.Hashable BatchWriteOperationResponse where
  hashWithSalt _salt BatchWriteOperationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` addFacetToObject
      `Prelude.hashWithSalt` attachObject
      `Prelude.hashWithSalt` attachPolicy
      `Prelude.hashWithSalt` attachToIndex
      `Prelude.hashWithSalt` attachTypedLink
      `Prelude.hashWithSalt` createIndex
      `Prelude.hashWithSalt` createObject
      `Prelude.hashWithSalt` deleteObject
      `Prelude.hashWithSalt` detachFromIndex
      `Prelude.hashWithSalt` detachObject
      `Prelude.hashWithSalt` detachPolicy
      `Prelude.hashWithSalt` detachTypedLink
      `Prelude.hashWithSalt` removeFacetFromObject
      `Prelude.hashWithSalt` updateLinkAttributes
      `Prelude.hashWithSalt` updateObjectAttributes

instance Prelude.NFData BatchWriteOperationResponse where
  rnf BatchWriteOperationResponse' {..} =
    Prelude.rnf addFacetToObject
      `Prelude.seq` Prelude.rnf attachObject
      `Prelude.seq` Prelude.rnf attachPolicy
      `Prelude.seq` Prelude.rnf attachToIndex
      `Prelude.seq` Prelude.rnf attachTypedLink
      `Prelude.seq` Prelude.rnf createIndex
      `Prelude.seq` Prelude.rnf createObject
      `Prelude.seq` Prelude.rnf deleteObject
      `Prelude.seq` Prelude.rnf detachFromIndex
      `Prelude.seq` Prelude.rnf detachObject
      `Prelude.seq` Prelude.rnf detachPolicy
      `Prelude.seq` Prelude.rnf detachTypedLink
      `Prelude.seq` Prelude.rnf removeFacetFromObject
      `Prelude.seq` Prelude.rnf updateLinkAttributes
      `Prelude.seq` Prelude.rnf updateObjectAttributes
