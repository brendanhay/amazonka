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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | Attaches the specified object to the specified index.
    attachToIndex :: Prelude.Maybe BatchAttachToIndexResponse,
    -- | Updates a given object’s attributes.
    updateObjectAttributes :: Prelude.Maybe BatchUpdateObjectAttributesResponse,
    -- | Deletes an object in a Directory.
    deleteObject :: Prelude.Maybe BatchDeleteObjectResponse,
    -- | Detaches an object from a Directory.
    detachObject :: Prelude.Maybe BatchDetachObjectResponse,
    -- | Detaches a typed link from a specified source and target object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    detachTypedLink :: Prelude.Maybe BatchDetachTypedLinkResponse,
    -- | Represents the output of a @BatchWrite@ response operation.
    updateLinkAttributes :: Prelude.Maybe BatchUpdateLinkAttributesResponse,
    -- | Creates an object in a Directory.
    createObject :: Prelude.Maybe BatchCreateObjectResponse,
    -- | The result of an add facet to object batch operation.
    addFacetToObject :: Prelude.Maybe BatchAddFacetToObjectResponse,
    -- | Detaches the specified object from the specified index.
    detachFromIndex :: Prelude.Maybe BatchDetachFromIndexResponse,
    -- | Attaches a policy object to a regular object. An object can have a
    -- limited number of attached policies.
    attachPolicy :: Prelude.Maybe BatchAttachPolicyResponse,
    -- | Creates an index object. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
    -- for more information.
    createIndex :: Prelude.Maybe BatchCreateIndexResponse,
    -- | Detaches a policy from a Directory.
    detachPolicy :: Prelude.Maybe BatchDetachPolicyResponse,
    -- | The result of a batch remove facet from object operation.
    removeFacetFromObject :: Prelude.Maybe BatchRemoveFacetFromObjectResponse,
    -- | Attaches a typed link to a specified source and target object. For more
    -- information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    attachTypedLink :: Prelude.Maybe BatchAttachTypedLinkResponse,
    -- | Attaches an object to a Directory.
    attachObject :: Prelude.Maybe BatchAttachObjectResponse
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
-- 'attachToIndex', 'batchWriteOperationResponse_attachToIndex' - Attaches the specified object to the specified index.
--
-- 'updateObjectAttributes', 'batchWriteOperationResponse_updateObjectAttributes' - Updates a given object’s attributes.
--
-- 'deleteObject', 'batchWriteOperationResponse_deleteObject' - Deletes an object in a Directory.
--
-- 'detachObject', 'batchWriteOperationResponse_detachObject' - Detaches an object from a Directory.
--
-- 'detachTypedLink', 'batchWriteOperationResponse_detachTypedLink' - Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'updateLinkAttributes', 'batchWriteOperationResponse_updateLinkAttributes' - Represents the output of a @BatchWrite@ response operation.
--
-- 'createObject', 'batchWriteOperationResponse_createObject' - Creates an object in a Directory.
--
-- 'addFacetToObject', 'batchWriteOperationResponse_addFacetToObject' - The result of an add facet to object batch operation.
--
-- 'detachFromIndex', 'batchWriteOperationResponse_detachFromIndex' - Detaches the specified object from the specified index.
--
-- 'attachPolicy', 'batchWriteOperationResponse_attachPolicy' - Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
--
-- 'createIndex', 'batchWriteOperationResponse_createIndex' - Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
--
-- 'detachPolicy', 'batchWriteOperationResponse_detachPolicy' - Detaches a policy from a Directory.
--
-- 'removeFacetFromObject', 'batchWriteOperationResponse_removeFacetFromObject' - The result of a batch remove facet from object operation.
--
-- 'attachTypedLink', 'batchWriteOperationResponse_attachTypedLink' - Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'attachObject', 'batchWriteOperationResponse_attachObject' - Attaches an object to a Directory.
newBatchWriteOperationResponse ::
  BatchWriteOperationResponse
newBatchWriteOperationResponse =
  BatchWriteOperationResponse'
    { attachToIndex =
        Prelude.Nothing,
      updateObjectAttributes = Prelude.Nothing,
      deleteObject = Prelude.Nothing,
      detachObject = Prelude.Nothing,
      detachTypedLink = Prelude.Nothing,
      updateLinkAttributes = Prelude.Nothing,
      createObject = Prelude.Nothing,
      addFacetToObject = Prelude.Nothing,
      detachFromIndex = Prelude.Nothing,
      attachPolicy = Prelude.Nothing,
      createIndex = Prelude.Nothing,
      detachPolicy = Prelude.Nothing,
      removeFacetFromObject = Prelude.Nothing,
      attachTypedLink = Prelude.Nothing,
      attachObject = Prelude.Nothing
    }

-- | Attaches the specified object to the specified index.
batchWriteOperationResponse_attachToIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachToIndexResponse)
batchWriteOperationResponse_attachToIndex = Lens.lens (\BatchWriteOperationResponse' {attachToIndex} -> attachToIndex) (\s@BatchWriteOperationResponse' {} a -> s {attachToIndex = a} :: BatchWriteOperationResponse)

-- | Updates a given object’s attributes.
batchWriteOperationResponse_updateObjectAttributes :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchUpdateObjectAttributesResponse)
batchWriteOperationResponse_updateObjectAttributes = Lens.lens (\BatchWriteOperationResponse' {updateObjectAttributes} -> updateObjectAttributes) (\s@BatchWriteOperationResponse' {} a -> s {updateObjectAttributes = a} :: BatchWriteOperationResponse)

-- | Deletes an object in a Directory.
batchWriteOperationResponse_deleteObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDeleteObjectResponse)
batchWriteOperationResponse_deleteObject = Lens.lens (\BatchWriteOperationResponse' {deleteObject} -> deleteObject) (\s@BatchWriteOperationResponse' {} a -> s {deleteObject = a} :: BatchWriteOperationResponse)

-- | Detaches an object from a Directory.
batchWriteOperationResponse_detachObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachObjectResponse)
batchWriteOperationResponse_detachObject = Lens.lens (\BatchWriteOperationResponse' {detachObject} -> detachObject) (\s@BatchWriteOperationResponse' {} a -> s {detachObject = a} :: BatchWriteOperationResponse)

-- | Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperationResponse_detachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachTypedLinkResponse)
batchWriteOperationResponse_detachTypedLink = Lens.lens (\BatchWriteOperationResponse' {detachTypedLink} -> detachTypedLink) (\s@BatchWriteOperationResponse' {} a -> s {detachTypedLink = a} :: BatchWriteOperationResponse)

-- | Represents the output of a @BatchWrite@ response operation.
batchWriteOperationResponse_updateLinkAttributes :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchUpdateLinkAttributesResponse)
batchWriteOperationResponse_updateLinkAttributes = Lens.lens (\BatchWriteOperationResponse' {updateLinkAttributes} -> updateLinkAttributes) (\s@BatchWriteOperationResponse' {} a -> s {updateLinkAttributes = a} :: BatchWriteOperationResponse)

-- | Creates an object in a Directory.
batchWriteOperationResponse_createObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchCreateObjectResponse)
batchWriteOperationResponse_createObject = Lens.lens (\BatchWriteOperationResponse' {createObject} -> createObject) (\s@BatchWriteOperationResponse' {} a -> s {createObject = a} :: BatchWriteOperationResponse)

-- | The result of an add facet to object batch operation.
batchWriteOperationResponse_addFacetToObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAddFacetToObjectResponse)
batchWriteOperationResponse_addFacetToObject = Lens.lens (\BatchWriteOperationResponse' {addFacetToObject} -> addFacetToObject) (\s@BatchWriteOperationResponse' {} a -> s {addFacetToObject = a} :: BatchWriteOperationResponse)

-- | Detaches the specified object from the specified index.
batchWriteOperationResponse_detachFromIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachFromIndexResponse)
batchWriteOperationResponse_detachFromIndex = Lens.lens (\BatchWriteOperationResponse' {detachFromIndex} -> detachFromIndex) (\s@BatchWriteOperationResponse' {} a -> s {detachFromIndex = a} :: BatchWriteOperationResponse)

-- | Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
batchWriteOperationResponse_attachPolicy :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachPolicyResponse)
batchWriteOperationResponse_attachPolicy = Lens.lens (\BatchWriteOperationResponse' {attachPolicy} -> attachPolicy) (\s@BatchWriteOperationResponse' {} a -> s {attachPolicy = a} :: BatchWriteOperationResponse)

-- | Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
batchWriteOperationResponse_createIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchCreateIndexResponse)
batchWriteOperationResponse_createIndex = Lens.lens (\BatchWriteOperationResponse' {createIndex} -> createIndex) (\s@BatchWriteOperationResponse' {} a -> s {createIndex = a} :: BatchWriteOperationResponse)

-- | Detaches a policy from a Directory.
batchWriteOperationResponse_detachPolicy :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachPolicyResponse)
batchWriteOperationResponse_detachPolicy = Lens.lens (\BatchWriteOperationResponse' {detachPolicy} -> detachPolicy) (\s@BatchWriteOperationResponse' {} a -> s {detachPolicy = a} :: BatchWriteOperationResponse)

-- | The result of a batch remove facet from object operation.
batchWriteOperationResponse_removeFacetFromObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchRemoveFacetFromObjectResponse)
batchWriteOperationResponse_removeFacetFromObject = Lens.lens (\BatchWriteOperationResponse' {removeFacetFromObject} -> removeFacetFromObject) (\s@BatchWriteOperationResponse' {} a -> s {removeFacetFromObject = a} :: BatchWriteOperationResponse)

-- | Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperationResponse_attachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachTypedLinkResponse)
batchWriteOperationResponse_attachTypedLink = Lens.lens (\BatchWriteOperationResponse' {attachTypedLink} -> attachTypedLink) (\s@BatchWriteOperationResponse' {} a -> s {attachTypedLink = a} :: BatchWriteOperationResponse)

-- | Attaches an object to a Directory.
batchWriteOperationResponse_attachObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachObjectResponse)
batchWriteOperationResponse_attachObject = Lens.lens (\BatchWriteOperationResponse' {attachObject} -> attachObject) (\s@BatchWriteOperationResponse' {} a -> s {attachObject = a} :: BatchWriteOperationResponse)

instance Data.FromJSON BatchWriteOperationResponse where
  parseJSON =
    Data.withObject
      "BatchWriteOperationResponse"
      ( \x ->
          BatchWriteOperationResponse'
            Prelude.<$> (x Data..:? "AttachToIndex")
            Prelude.<*> (x Data..:? "UpdateObjectAttributes")
            Prelude.<*> (x Data..:? "DeleteObject")
            Prelude.<*> (x Data..:? "DetachObject")
            Prelude.<*> (x Data..:? "DetachTypedLink")
            Prelude.<*> (x Data..:? "UpdateLinkAttributes")
            Prelude.<*> (x Data..:? "CreateObject")
            Prelude.<*> (x Data..:? "AddFacetToObject")
            Prelude.<*> (x Data..:? "DetachFromIndex")
            Prelude.<*> (x Data..:? "AttachPolicy")
            Prelude.<*> (x Data..:? "CreateIndex")
            Prelude.<*> (x Data..:? "DetachPolicy")
            Prelude.<*> (x Data..:? "RemoveFacetFromObject")
            Prelude.<*> (x Data..:? "AttachTypedLink")
            Prelude.<*> (x Data..:? "AttachObject")
      )

instance Prelude.Hashable BatchWriteOperationResponse where
  hashWithSalt _salt BatchWriteOperationResponse' {..} =
    _salt `Prelude.hashWithSalt` attachToIndex
      `Prelude.hashWithSalt` updateObjectAttributes
      `Prelude.hashWithSalt` deleteObject
      `Prelude.hashWithSalt` detachObject
      `Prelude.hashWithSalt` detachTypedLink
      `Prelude.hashWithSalt` updateLinkAttributes
      `Prelude.hashWithSalt` createObject
      `Prelude.hashWithSalt` addFacetToObject
      `Prelude.hashWithSalt` detachFromIndex
      `Prelude.hashWithSalt` attachPolicy
      `Prelude.hashWithSalt` createIndex
      `Prelude.hashWithSalt` detachPolicy
      `Prelude.hashWithSalt` removeFacetFromObject
      `Prelude.hashWithSalt` attachTypedLink
      `Prelude.hashWithSalt` attachObject

instance Prelude.NFData BatchWriteOperationResponse where
  rnf BatchWriteOperationResponse' {..} =
    Prelude.rnf attachToIndex
      `Prelude.seq` Prelude.rnf updateObjectAttributes
      `Prelude.seq` Prelude.rnf deleteObject
      `Prelude.seq` Prelude.rnf detachObject
      `Prelude.seq` Prelude.rnf detachTypedLink
      `Prelude.seq` Prelude.rnf updateLinkAttributes
      `Prelude.seq` Prelude.rnf createObject
      `Prelude.seq` Prelude.rnf addFacetToObject
      `Prelude.seq` Prelude.rnf detachFromIndex
      `Prelude.seq` Prelude.rnf attachPolicy
      `Prelude.seq` Prelude.rnf createIndex
      `Prelude.seq` Prelude.rnf detachPolicy
      `Prelude.seq` Prelude.rnf removeFacetFromObject
      `Prelude.seq` Prelude.rnf attachTypedLink
      `Prelude.seq` Prelude.rnf attachObject
