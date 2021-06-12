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
-- Module      : Network.AWS.CloudDirectory.Types.BatchWriteOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchWriteOperation where

import Network.AWS.CloudDirectory.Types.BatchAddFacetToObject
import Network.AWS.CloudDirectory.Types.BatchAttachObject
import Network.AWS.CloudDirectory.Types.BatchAttachPolicy
import Network.AWS.CloudDirectory.Types.BatchAttachToIndex
import Network.AWS.CloudDirectory.Types.BatchAttachTypedLink
import Network.AWS.CloudDirectory.Types.BatchCreateIndex
import Network.AWS.CloudDirectory.Types.BatchCreateObject
import Network.AWS.CloudDirectory.Types.BatchDeleteObject
import Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
import Network.AWS.CloudDirectory.Types.BatchDetachObject
import Network.AWS.CloudDirectory.Types.BatchDetachPolicy
import Network.AWS.CloudDirectory.Types.BatchDetachTypedLink
import Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject
import Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes
import Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a @BatchWrite@ operation.
--
-- /See:/ 'newBatchWriteOperation' smart constructor.
data BatchWriteOperation = BatchWriteOperation'
  { -- | Attaches a typed link to a specified source and target object. For more
    -- information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    attachTypedLink :: Core.Maybe BatchAttachTypedLink,
    -- | Deletes an object in a Directory.
    deleteObject :: Core.Maybe BatchDeleteObject,
    -- | Creates an object.
    createObject :: Core.Maybe BatchCreateObject,
    -- | Updates a given object\'s attributes.
    updateLinkAttributes :: Core.Maybe BatchUpdateLinkAttributes,
    -- | Detaches a typed link from a specified source and target object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    detachTypedLink :: Core.Maybe BatchDetachTypedLink,
    -- | Creates an index object. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
    -- for more information.
    createIndex :: Core.Maybe BatchCreateIndex,
    -- | Detaches a policy from a Directory.
    detachPolicy :: Core.Maybe BatchDetachPolicy,
    -- | Detaches the specified object from the specified index.
    detachFromIndex :: Core.Maybe BatchDetachFromIndex,
    -- | Attaches an object to a Directory.
    attachObject :: Core.Maybe BatchAttachObject,
    -- | Attaches the specified object to the specified index.
    attachToIndex :: Core.Maybe BatchAttachToIndex,
    -- | Updates a given object\'s attributes.
    updateObjectAttributes :: Core.Maybe BatchUpdateObjectAttributes,
    -- | Attaches a policy object to a regular object. An object can have a
    -- limited number of attached policies.
    attachPolicy :: Core.Maybe BatchAttachPolicy,
    -- | A batch operation that removes a facet from an object.
    removeFacetFromObject :: Core.Maybe BatchRemoveFacetFromObject,
    -- | A batch operation that adds a facet to an object.
    addFacetToObject :: Core.Maybe BatchAddFacetToObject,
    -- | Detaches an object from a Directory.
    detachObject :: Core.Maybe BatchDetachObject
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchWriteOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachTypedLink', 'batchWriteOperation_attachTypedLink' - Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'deleteObject', 'batchWriteOperation_deleteObject' - Deletes an object in a Directory.
--
-- 'createObject', 'batchWriteOperation_createObject' - Creates an object.
--
-- 'updateLinkAttributes', 'batchWriteOperation_updateLinkAttributes' - Updates a given object\'s attributes.
--
-- 'detachTypedLink', 'batchWriteOperation_detachTypedLink' - Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'createIndex', 'batchWriteOperation_createIndex' - Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
--
-- 'detachPolicy', 'batchWriteOperation_detachPolicy' - Detaches a policy from a Directory.
--
-- 'detachFromIndex', 'batchWriteOperation_detachFromIndex' - Detaches the specified object from the specified index.
--
-- 'attachObject', 'batchWriteOperation_attachObject' - Attaches an object to a Directory.
--
-- 'attachToIndex', 'batchWriteOperation_attachToIndex' - Attaches the specified object to the specified index.
--
-- 'updateObjectAttributes', 'batchWriteOperation_updateObjectAttributes' - Updates a given object\'s attributes.
--
-- 'attachPolicy', 'batchWriteOperation_attachPolicy' - Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
--
-- 'removeFacetFromObject', 'batchWriteOperation_removeFacetFromObject' - A batch operation that removes a facet from an object.
--
-- 'addFacetToObject', 'batchWriteOperation_addFacetToObject' - A batch operation that adds a facet to an object.
--
-- 'detachObject', 'batchWriteOperation_detachObject' - Detaches an object from a Directory.
newBatchWriteOperation ::
  BatchWriteOperation
newBatchWriteOperation =
  BatchWriteOperation'
    { attachTypedLink =
        Core.Nothing,
      deleteObject = Core.Nothing,
      createObject = Core.Nothing,
      updateLinkAttributes = Core.Nothing,
      detachTypedLink = Core.Nothing,
      createIndex = Core.Nothing,
      detachPolicy = Core.Nothing,
      detachFromIndex = Core.Nothing,
      attachObject = Core.Nothing,
      attachToIndex = Core.Nothing,
      updateObjectAttributes = Core.Nothing,
      attachPolicy = Core.Nothing,
      removeFacetFromObject = Core.Nothing,
      addFacetToObject = Core.Nothing,
      detachObject = Core.Nothing
    }

-- | Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperation_attachTypedLink :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchAttachTypedLink)
batchWriteOperation_attachTypedLink = Lens.lens (\BatchWriteOperation' {attachTypedLink} -> attachTypedLink) (\s@BatchWriteOperation' {} a -> s {attachTypedLink = a} :: BatchWriteOperation)

-- | Deletes an object in a Directory.
batchWriteOperation_deleteObject :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchDeleteObject)
batchWriteOperation_deleteObject = Lens.lens (\BatchWriteOperation' {deleteObject} -> deleteObject) (\s@BatchWriteOperation' {} a -> s {deleteObject = a} :: BatchWriteOperation)

-- | Creates an object.
batchWriteOperation_createObject :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchCreateObject)
batchWriteOperation_createObject = Lens.lens (\BatchWriteOperation' {createObject} -> createObject) (\s@BatchWriteOperation' {} a -> s {createObject = a} :: BatchWriteOperation)

-- | Updates a given object\'s attributes.
batchWriteOperation_updateLinkAttributes :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchUpdateLinkAttributes)
batchWriteOperation_updateLinkAttributes = Lens.lens (\BatchWriteOperation' {updateLinkAttributes} -> updateLinkAttributes) (\s@BatchWriteOperation' {} a -> s {updateLinkAttributes = a} :: BatchWriteOperation)

-- | Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperation_detachTypedLink :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchDetachTypedLink)
batchWriteOperation_detachTypedLink = Lens.lens (\BatchWriteOperation' {detachTypedLink} -> detachTypedLink) (\s@BatchWriteOperation' {} a -> s {detachTypedLink = a} :: BatchWriteOperation)

-- | Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
batchWriteOperation_createIndex :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchCreateIndex)
batchWriteOperation_createIndex = Lens.lens (\BatchWriteOperation' {createIndex} -> createIndex) (\s@BatchWriteOperation' {} a -> s {createIndex = a} :: BatchWriteOperation)

-- | Detaches a policy from a Directory.
batchWriteOperation_detachPolicy :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchDetachPolicy)
batchWriteOperation_detachPolicy = Lens.lens (\BatchWriteOperation' {detachPolicy} -> detachPolicy) (\s@BatchWriteOperation' {} a -> s {detachPolicy = a} :: BatchWriteOperation)

-- | Detaches the specified object from the specified index.
batchWriteOperation_detachFromIndex :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchDetachFromIndex)
batchWriteOperation_detachFromIndex = Lens.lens (\BatchWriteOperation' {detachFromIndex} -> detachFromIndex) (\s@BatchWriteOperation' {} a -> s {detachFromIndex = a} :: BatchWriteOperation)

-- | Attaches an object to a Directory.
batchWriteOperation_attachObject :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchAttachObject)
batchWriteOperation_attachObject = Lens.lens (\BatchWriteOperation' {attachObject} -> attachObject) (\s@BatchWriteOperation' {} a -> s {attachObject = a} :: BatchWriteOperation)

-- | Attaches the specified object to the specified index.
batchWriteOperation_attachToIndex :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchAttachToIndex)
batchWriteOperation_attachToIndex = Lens.lens (\BatchWriteOperation' {attachToIndex} -> attachToIndex) (\s@BatchWriteOperation' {} a -> s {attachToIndex = a} :: BatchWriteOperation)

-- | Updates a given object\'s attributes.
batchWriteOperation_updateObjectAttributes :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchUpdateObjectAttributes)
batchWriteOperation_updateObjectAttributes = Lens.lens (\BatchWriteOperation' {updateObjectAttributes} -> updateObjectAttributes) (\s@BatchWriteOperation' {} a -> s {updateObjectAttributes = a} :: BatchWriteOperation)

-- | Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
batchWriteOperation_attachPolicy :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchAttachPolicy)
batchWriteOperation_attachPolicy = Lens.lens (\BatchWriteOperation' {attachPolicy} -> attachPolicy) (\s@BatchWriteOperation' {} a -> s {attachPolicy = a} :: BatchWriteOperation)

-- | A batch operation that removes a facet from an object.
batchWriteOperation_removeFacetFromObject :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchRemoveFacetFromObject)
batchWriteOperation_removeFacetFromObject = Lens.lens (\BatchWriteOperation' {removeFacetFromObject} -> removeFacetFromObject) (\s@BatchWriteOperation' {} a -> s {removeFacetFromObject = a} :: BatchWriteOperation)

-- | A batch operation that adds a facet to an object.
batchWriteOperation_addFacetToObject :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchAddFacetToObject)
batchWriteOperation_addFacetToObject = Lens.lens (\BatchWriteOperation' {addFacetToObject} -> addFacetToObject) (\s@BatchWriteOperation' {} a -> s {addFacetToObject = a} :: BatchWriteOperation)

-- | Detaches an object from a Directory.
batchWriteOperation_detachObject :: Lens.Lens' BatchWriteOperation (Core.Maybe BatchDetachObject)
batchWriteOperation_detachObject = Lens.lens (\BatchWriteOperation' {detachObject} -> detachObject) (\s@BatchWriteOperation' {} a -> s {detachObject = a} :: BatchWriteOperation)

instance Core.Hashable BatchWriteOperation

instance Core.NFData BatchWriteOperation

instance Core.ToJSON BatchWriteOperation where
  toJSON BatchWriteOperation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AttachTypedLink" Core..=)
              Core.<$> attachTypedLink,
            ("DeleteObject" Core..=) Core.<$> deleteObject,
            ("CreateObject" Core..=) Core.<$> createObject,
            ("UpdateLinkAttributes" Core..=)
              Core.<$> updateLinkAttributes,
            ("DetachTypedLink" Core..=) Core.<$> detachTypedLink,
            ("CreateIndex" Core..=) Core.<$> createIndex,
            ("DetachPolicy" Core..=) Core.<$> detachPolicy,
            ("DetachFromIndex" Core..=) Core.<$> detachFromIndex,
            ("AttachObject" Core..=) Core.<$> attachObject,
            ("AttachToIndex" Core..=) Core.<$> attachToIndex,
            ("UpdateObjectAttributes" Core..=)
              Core.<$> updateObjectAttributes,
            ("AttachPolicy" Core..=) Core.<$> attachPolicy,
            ("RemoveFacetFromObject" Core..=)
              Core.<$> removeFacetFromObject,
            ("AddFacetToObject" Core..=)
              Core.<$> addFacetToObject,
            ("DetachObject" Core..=) Core.<$> detachObject
          ]
      )
