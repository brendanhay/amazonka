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
-- Module      : Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse where

import Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse
import Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
import Network.AWS.CloudDirectory.Types.BatchAttachPolicyResponse
import Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
import Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse
import Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse
import Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
import Network.AWS.CloudDirectory.Types.BatchDeleteObjectResponse
import Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
import Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
import Network.AWS.CloudDirectory.Types.BatchDetachPolicyResponse
import Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse
import Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
import Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a @BatchWrite@ response operation.
--
-- /See:/ 'newBatchWriteOperationResponse' smart constructor.
data BatchWriteOperationResponse = BatchWriteOperationResponse'
  { -- | Attaches a typed link to a specified source and target object. For more
    -- information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    attachTypedLink :: Prelude.Maybe BatchAttachTypedLinkResponse,
    -- | Deletes an object in a Directory.
    deleteObject :: Prelude.Maybe BatchDeleteObjectResponse,
    -- | Creates an object in a Directory.
    createObject :: Prelude.Maybe BatchCreateObjectResponse,
    -- | Represents the output of a @BatchWrite@ response operation.
    updateLinkAttributes :: Prelude.Maybe BatchUpdateLinkAttributesResponse,
    -- | Detaches a typed link from a specified source and target object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    detachTypedLink :: Prelude.Maybe BatchDetachTypedLinkResponse,
    -- | Creates an index object. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
    -- for more information.
    createIndex :: Prelude.Maybe BatchCreateIndexResponse,
    -- | Detaches the specified object from the specified index.
    detachFromIndex :: Prelude.Maybe BatchDetachFromIndexResponse,
    -- | Detaches a policy from a Directory.
    detachPolicy :: Prelude.Maybe BatchDetachPolicyResponse,
    -- | Attaches an object to a Directory.
    attachObject :: Prelude.Maybe BatchAttachObjectResponse,
    -- | Attaches the specified object to the specified index.
    attachToIndex :: Prelude.Maybe BatchAttachToIndexResponse,
    -- | Attaches a policy object to a regular object. An object can have a
    -- limited number of attached policies.
    attachPolicy :: Prelude.Maybe BatchAttachPolicyResponse,
    -- | Updates a given object’s attributes.
    updateObjectAttributes :: Prelude.Maybe BatchUpdateObjectAttributesResponse,
    -- | The result of a batch remove facet from object operation.
    removeFacetFromObject :: Prelude.Maybe BatchRemoveFacetFromObjectResponse,
    -- | Detaches an object from a Directory.
    detachObject :: Prelude.Maybe BatchDetachObjectResponse,
    -- | The result of an add facet to object batch operation.
    addFacetToObject :: Prelude.Maybe BatchAddFacetToObjectResponse
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
-- 'attachTypedLink', 'batchWriteOperationResponse_attachTypedLink' - Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'deleteObject', 'batchWriteOperationResponse_deleteObject' - Deletes an object in a Directory.
--
-- 'createObject', 'batchWriteOperationResponse_createObject' - Creates an object in a Directory.
--
-- 'updateLinkAttributes', 'batchWriteOperationResponse_updateLinkAttributes' - Represents the output of a @BatchWrite@ response operation.
--
-- 'detachTypedLink', 'batchWriteOperationResponse_detachTypedLink' - Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'createIndex', 'batchWriteOperationResponse_createIndex' - Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
--
-- 'detachFromIndex', 'batchWriteOperationResponse_detachFromIndex' - Detaches the specified object from the specified index.
--
-- 'detachPolicy', 'batchWriteOperationResponse_detachPolicy' - Detaches a policy from a Directory.
--
-- 'attachObject', 'batchWriteOperationResponse_attachObject' - Attaches an object to a Directory.
--
-- 'attachToIndex', 'batchWriteOperationResponse_attachToIndex' - Attaches the specified object to the specified index.
--
-- 'attachPolicy', 'batchWriteOperationResponse_attachPolicy' - Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
--
-- 'updateObjectAttributes', 'batchWriteOperationResponse_updateObjectAttributes' - Updates a given object’s attributes.
--
-- 'removeFacetFromObject', 'batchWriteOperationResponse_removeFacetFromObject' - The result of a batch remove facet from object operation.
--
-- 'detachObject', 'batchWriteOperationResponse_detachObject' - Detaches an object from a Directory.
--
-- 'addFacetToObject', 'batchWriteOperationResponse_addFacetToObject' - The result of an add facet to object batch operation.
newBatchWriteOperationResponse ::
  BatchWriteOperationResponse
newBatchWriteOperationResponse =
  BatchWriteOperationResponse'
    { attachTypedLink =
        Prelude.Nothing,
      deleteObject = Prelude.Nothing,
      createObject = Prelude.Nothing,
      updateLinkAttributes = Prelude.Nothing,
      detachTypedLink = Prelude.Nothing,
      createIndex = Prelude.Nothing,
      detachFromIndex = Prelude.Nothing,
      detachPolicy = Prelude.Nothing,
      attachObject = Prelude.Nothing,
      attachToIndex = Prelude.Nothing,
      attachPolicy = Prelude.Nothing,
      updateObjectAttributes = Prelude.Nothing,
      removeFacetFromObject = Prelude.Nothing,
      detachObject = Prelude.Nothing,
      addFacetToObject = Prelude.Nothing
    }

-- | Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperationResponse_attachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachTypedLinkResponse)
batchWriteOperationResponse_attachTypedLink = Lens.lens (\BatchWriteOperationResponse' {attachTypedLink} -> attachTypedLink) (\s@BatchWriteOperationResponse' {} a -> s {attachTypedLink = a} :: BatchWriteOperationResponse)

-- | Deletes an object in a Directory.
batchWriteOperationResponse_deleteObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDeleteObjectResponse)
batchWriteOperationResponse_deleteObject = Lens.lens (\BatchWriteOperationResponse' {deleteObject} -> deleteObject) (\s@BatchWriteOperationResponse' {} a -> s {deleteObject = a} :: BatchWriteOperationResponse)

-- | Creates an object in a Directory.
batchWriteOperationResponse_createObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchCreateObjectResponse)
batchWriteOperationResponse_createObject = Lens.lens (\BatchWriteOperationResponse' {createObject} -> createObject) (\s@BatchWriteOperationResponse' {} a -> s {createObject = a} :: BatchWriteOperationResponse)

-- | Represents the output of a @BatchWrite@ response operation.
batchWriteOperationResponse_updateLinkAttributes :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchUpdateLinkAttributesResponse)
batchWriteOperationResponse_updateLinkAttributes = Lens.lens (\BatchWriteOperationResponse' {updateLinkAttributes} -> updateLinkAttributes) (\s@BatchWriteOperationResponse' {} a -> s {updateLinkAttributes = a} :: BatchWriteOperationResponse)

-- | Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperationResponse_detachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachTypedLinkResponse)
batchWriteOperationResponse_detachTypedLink = Lens.lens (\BatchWriteOperationResponse' {detachTypedLink} -> detachTypedLink) (\s@BatchWriteOperationResponse' {} a -> s {detachTypedLink = a} :: BatchWriteOperationResponse)

-- | Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
batchWriteOperationResponse_createIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchCreateIndexResponse)
batchWriteOperationResponse_createIndex = Lens.lens (\BatchWriteOperationResponse' {createIndex} -> createIndex) (\s@BatchWriteOperationResponse' {} a -> s {createIndex = a} :: BatchWriteOperationResponse)

-- | Detaches the specified object from the specified index.
batchWriteOperationResponse_detachFromIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachFromIndexResponse)
batchWriteOperationResponse_detachFromIndex = Lens.lens (\BatchWriteOperationResponse' {detachFromIndex} -> detachFromIndex) (\s@BatchWriteOperationResponse' {} a -> s {detachFromIndex = a} :: BatchWriteOperationResponse)

-- | Detaches a policy from a Directory.
batchWriteOperationResponse_detachPolicy :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachPolicyResponse)
batchWriteOperationResponse_detachPolicy = Lens.lens (\BatchWriteOperationResponse' {detachPolicy} -> detachPolicy) (\s@BatchWriteOperationResponse' {} a -> s {detachPolicy = a} :: BatchWriteOperationResponse)

-- | Attaches an object to a Directory.
batchWriteOperationResponse_attachObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachObjectResponse)
batchWriteOperationResponse_attachObject = Lens.lens (\BatchWriteOperationResponse' {attachObject} -> attachObject) (\s@BatchWriteOperationResponse' {} a -> s {attachObject = a} :: BatchWriteOperationResponse)

-- | Attaches the specified object to the specified index.
batchWriteOperationResponse_attachToIndex :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachToIndexResponse)
batchWriteOperationResponse_attachToIndex = Lens.lens (\BatchWriteOperationResponse' {attachToIndex} -> attachToIndex) (\s@BatchWriteOperationResponse' {} a -> s {attachToIndex = a} :: BatchWriteOperationResponse)

-- | Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
batchWriteOperationResponse_attachPolicy :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAttachPolicyResponse)
batchWriteOperationResponse_attachPolicy = Lens.lens (\BatchWriteOperationResponse' {attachPolicy} -> attachPolicy) (\s@BatchWriteOperationResponse' {} a -> s {attachPolicy = a} :: BatchWriteOperationResponse)

-- | Updates a given object’s attributes.
batchWriteOperationResponse_updateObjectAttributes :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchUpdateObjectAttributesResponse)
batchWriteOperationResponse_updateObjectAttributes = Lens.lens (\BatchWriteOperationResponse' {updateObjectAttributes} -> updateObjectAttributes) (\s@BatchWriteOperationResponse' {} a -> s {updateObjectAttributes = a} :: BatchWriteOperationResponse)

-- | The result of a batch remove facet from object operation.
batchWriteOperationResponse_removeFacetFromObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchRemoveFacetFromObjectResponse)
batchWriteOperationResponse_removeFacetFromObject = Lens.lens (\BatchWriteOperationResponse' {removeFacetFromObject} -> removeFacetFromObject) (\s@BatchWriteOperationResponse' {} a -> s {removeFacetFromObject = a} :: BatchWriteOperationResponse)

-- | Detaches an object from a Directory.
batchWriteOperationResponse_detachObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchDetachObjectResponse)
batchWriteOperationResponse_detachObject = Lens.lens (\BatchWriteOperationResponse' {detachObject} -> detachObject) (\s@BatchWriteOperationResponse' {} a -> s {detachObject = a} :: BatchWriteOperationResponse)

-- | The result of an add facet to object batch operation.
batchWriteOperationResponse_addFacetToObject :: Lens.Lens' BatchWriteOperationResponse (Prelude.Maybe BatchAddFacetToObjectResponse)
batchWriteOperationResponse_addFacetToObject = Lens.lens (\BatchWriteOperationResponse' {addFacetToObject} -> addFacetToObject) (\s@BatchWriteOperationResponse' {} a -> s {addFacetToObject = a} :: BatchWriteOperationResponse)

instance Core.FromJSON BatchWriteOperationResponse where
  parseJSON =
    Core.withObject
      "BatchWriteOperationResponse"
      ( \x ->
          BatchWriteOperationResponse'
            Prelude.<$> (x Core..:? "AttachTypedLink")
            Prelude.<*> (x Core..:? "DeleteObject")
            Prelude.<*> (x Core..:? "CreateObject")
            Prelude.<*> (x Core..:? "UpdateLinkAttributes")
            Prelude.<*> (x Core..:? "DetachTypedLink")
            Prelude.<*> (x Core..:? "CreateIndex")
            Prelude.<*> (x Core..:? "DetachFromIndex")
            Prelude.<*> (x Core..:? "DetachPolicy")
            Prelude.<*> (x Core..:? "AttachObject")
            Prelude.<*> (x Core..:? "AttachToIndex")
            Prelude.<*> (x Core..:? "AttachPolicy")
            Prelude.<*> (x Core..:? "UpdateObjectAttributes")
            Prelude.<*> (x Core..:? "RemoveFacetFromObject")
            Prelude.<*> (x Core..:? "DetachObject")
            Prelude.<*> (x Core..:? "AddFacetToObject")
      )

instance Prelude.Hashable BatchWriteOperationResponse

instance Prelude.NFData BatchWriteOperationResponse
