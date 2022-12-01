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
-- Module      : Amazonka.CloudDirectory.Types.BatchWriteOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchWriteOperation where

import Amazonka.CloudDirectory.Types.BatchAddFacetToObject
import Amazonka.CloudDirectory.Types.BatchAttachObject
import Amazonka.CloudDirectory.Types.BatchAttachPolicy
import Amazonka.CloudDirectory.Types.BatchAttachToIndex
import Amazonka.CloudDirectory.Types.BatchAttachTypedLink
import Amazonka.CloudDirectory.Types.BatchCreateIndex
import Amazonka.CloudDirectory.Types.BatchCreateObject
import Amazonka.CloudDirectory.Types.BatchDeleteObject
import Amazonka.CloudDirectory.Types.BatchDetachFromIndex
import Amazonka.CloudDirectory.Types.BatchDetachObject
import Amazonka.CloudDirectory.Types.BatchDetachPolicy
import Amazonka.CloudDirectory.Types.BatchDetachTypedLink
import Amazonka.CloudDirectory.Types.BatchRemoveFacetFromObject
import Amazonka.CloudDirectory.Types.BatchUpdateLinkAttributes
import Amazonka.CloudDirectory.Types.BatchUpdateObjectAttributes
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a @BatchWrite@ operation.
--
-- /See:/ 'newBatchWriteOperation' smart constructor.
data BatchWriteOperation = BatchWriteOperation'
  { -- | Attaches the specified object to the specified index.
    attachToIndex :: Prelude.Maybe BatchAttachToIndex,
    -- | Updates a given object\'s attributes.
    updateObjectAttributes :: Prelude.Maybe BatchUpdateObjectAttributes,
    -- | Deletes an object in a Directory.
    deleteObject :: Prelude.Maybe BatchDeleteObject,
    -- | Detaches an object from a Directory.
    detachObject :: Prelude.Maybe BatchDetachObject,
    -- | Detaches a typed link from a specified source and target object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    detachTypedLink :: Prelude.Maybe BatchDetachTypedLink,
    -- | Updates a given object\'s attributes.
    updateLinkAttributes :: Prelude.Maybe BatchUpdateLinkAttributes,
    -- | Creates an object.
    createObject :: Prelude.Maybe BatchCreateObject,
    -- | A batch operation that adds a facet to an object.
    addFacetToObject :: Prelude.Maybe BatchAddFacetToObject,
    -- | Detaches the specified object from the specified index.
    detachFromIndex :: Prelude.Maybe BatchDetachFromIndex,
    -- | Attaches a policy object to a regular object. An object can have a
    -- limited number of attached policies.
    attachPolicy :: Prelude.Maybe BatchAttachPolicy,
    -- | Creates an index object. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
    -- for more information.
    createIndex :: Prelude.Maybe BatchCreateIndex,
    -- | Detaches a policy from a Directory.
    detachPolicy :: Prelude.Maybe BatchDetachPolicy,
    -- | A batch operation that removes a facet from an object.
    removeFacetFromObject :: Prelude.Maybe BatchRemoveFacetFromObject,
    -- | Attaches a typed link to a specified source and target object. For more
    -- information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    attachTypedLink :: Prelude.Maybe BatchAttachTypedLink,
    -- | Attaches an object to a Directory.
    attachObject :: Prelude.Maybe BatchAttachObject
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchWriteOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachToIndex', 'batchWriteOperation_attachToIndex' - Attaches the specified object to the specified index.
--
-- 'updateObjectAttributes', 'batchWriteOperation_updateObjectAttributes' - Updates a given object\'s attributes.
--
-- 'deleteObject', 'batchWriteOperation_deleteObject' - Deletes an object in a Directory.
--
-- 'detachObject', 'batchWriteOperation_detachObject' - Detaches an object from a Directory.
--
-- 'detachTypedLink', 'batchWriteOperation_detachTypedLink' - Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'updateLinkAttributes', 'batchWriteOperation_updateLinkAttributes' - Updates a given object\'s attributes.
--
-- 'createObject', 'batchWriteOperation_createObject' - Creates an object.
--
-- 'addFacetToObject', 'batchWriteOperation_addFacetToObject' - A batch operation that adds a facet to an object.
--
-- 'detachFromIndex', 'batchWriteOperation_detachFromIndex' - Detaches the specified object from the specified index.
--
-- 'attachPolicy', 'batchWriteOperation_attachPolicy' - Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
--
-- 'createIndex', 'batchWriteOperation_createIndex' - Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
--
-- 'detachPolicy', 'batchWriteOperation_detachPolicy' - Detaches a policy from a Directory.
--
-- 'removeFacetFromObject', 'batchWriteOperation_removeFacetFromObject' - A batch operation that removes a facet from an object.
--
-- 'attachTypedLink', 'batchWriteOperation_attachTypedLink' - Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'attachObject', 'batchWriteOperation_attachObject' - Attaches an object to a Directory.
newBatchWriteOperation ::
  BatchWriteOperation
newBatchWriteOperation =
  BatchWriteOperation'
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
batchWriteOperation_attachToIndex :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchAttachToIndex)
batchWriteOperation_attachToIndex = Lens.lens (\BatchWriteOperation' {attachToIndex} -> attachToIndex) (\s@BatchWriteOperation' {} a -> s {attachToIndex = a} :: BatchWriteOperation)

-- | Updates a given object\'s attributes.
batchWriteOperation_updateObjectAttributes :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchUpdateObjectAttributes)
batchWriteOperation_updateObjectAttributes = Lens.lens (\BatchWriteOperation' {updateObjectAttributes} -> updateObjectAttributes) (\s@BatchWriteOperation' {} a -> s {updateObjectAttributes = a} :: BatchWriteOperation)

-- | Deletes an object in a Directory.
batchWriteOperation_deleteObject :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchDeleteObject)
batchWriteOperation_deleteObject = Lens.lens (\BatchWriteOperation' {deleteObject} -> deleteObject) (\s@BatchWriteOperation' {} a -> s {deleteObject = a} :: BatchWriteOperation)

-- | Detaches an object from a Directory.
batchWriteOperation_detachObject :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchDetachObject)
batchWriteOperation_detachObject = Lens.lens (\BatchWriteOperation' {detachObject} -> detachObject) (\s@BatchWriteOperation' {} a -> s {detachObject = a} :: BatchWriteOperation)

-- | Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperation_detachTypedLink :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchDetachTypedLink)
batchWriteOperation_detachTypedLink = Lens.lens (\BatchWriteOperation' {detachTypedLink} -> detachTypedLink) (\s@BatchWriteOperation' {} a -> s {detachTypedLink = a} :: BatchWriteOperation)

-- | Updates a given object\'s attributes.
batchWriteOperation_updateLinkAttributes :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchUpdateLinkAttributes)
batchWriteOperation_updateLinkAttributes = Lens.lens (\BatchWriteOperation' {updateLinkAttributes} -> updateLinkAttributes) (\s@BatchWriteOperation' {} a -> s {updateLinkAttributes = a} :: BatchWriteOperation)

-- | Creates an object.
batchWriteOperation_createObject :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchCreateObject)
batchWriteOperation_createObject = Lens.lens (\BatchWriteOperation' {createObject} -> createObject) (\s@BatchWriteOperation' {} a -> s {createObject = a} :: BatchWriteOperation)

-- | A batch operation that adds a facet to an object.
batchWriteOperation_addFacetToObject :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchAddFacetToObject)
batchWriteOperation_addFacetToObject = Lens.lens (\BatchWriteOperation' {addFacetToObject} -> addFacetToObject) (\s@BatchWriteOperation' {} a -> s {addFacetToObject = a} :: BatchWriteOperation)

-- | Detaches the specified object from the specified index.
batchWriteOperation_detachFromIndex :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchDetachFromIndex)
batchWriteOperation_detachFromIndex = Lens.lens (\BatchWriteOperation' {detachFromIndex} -> detachFromIndex) (\s@BatchWriteOperation' {} a -> s {detachFromIndex = a} :: BatchWriteOperation)

-- | Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
batchWriteOperation_attachPolicy :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchAttachPolicy)
batchWriteOperation_attachPolicy = Lens.lens (\BatchWriteOperation' {attachPolicy} -> attachPolicy) (\s@BatchWriteOperation' {} a -> s {attachPolicy = a} :: BatchWriteOperation)

-- | Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search>
-- for more information.
batchWriteOperation_createIndex :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchCreateIndex)
batchWriteOperation_createIndex = Lens.lens (\BatchWriteOperation' {createIndex} -> createIndex) (\s@BatchWriteOperation' {} a -> s {createIndex = a} :: BatchWriteOperation)

-- | Detaches a policy from a Directory.
batchWriteOperation_detachPolicy :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchDetachPolicy)
batchWriteOperation_detachPolicy = Lens.lens (\BatchWriteOperation' {detachPolicy} -> detachPolicy) (\s@BatchWriteOperation' {} a -> s {detachPolicy = a} :: BatchWriteOperation)

-- | A batch operation that removes a facet from an object.
batchWriteOperation_removeFacetFromObject :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchRemoveFacetFromObject)
batchWriteOperation_removeFacetFromObject = Lens.lens (\BatchWriteOperation' {removeFacetFromObject} -> removeFacetFromObject) (\s@BatchWriteOperation' {} a -> s {removeFacetFromObject = a} :: BatchWriteOperation)

-- | Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchWriteOperation_attachTypedLink :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchAttachTypedLink)
batchWriteOperation_attachTypedLink = Lens.lens (\BatchWriteOperation' {attachTypedLink} -> attachTypedLink) (\s@BatchWriteOperation' {} a -> s {attachTypedLink = a} :: BatchWriteOperation)

-- | Attaches an object to a Directory.
batchWriteOperation_attachObject :: Lens.Lens' BatchWriteOperation (Prelude.Maybe BatchAttachObject)
batchWriteOperation_attachObject = Lens.lens (\BatchWriteOperation' {attachObject} -> attachObject) (\s@BatchWriteOperation' {} a -> s {attachObject = a} :: BatchWriteOperation)

instance Prelude.Hashable BatchWriteOperation where
  hashWithSalt _salt BatchWriteOperation' {..} =
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

instance Prelude.NFData BatchWriteOperation where
  rnf BatchWriteOperation' {..} =
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

instance Core.ToJSON BatchWriteOperation where
  toJSON BatchWriteOperation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AttachToIndex" Core..=) Prelude.<$> attachToIndex,
            ("UpdateObjectAttributes" Core..=)
              Prelude.<$> updateObjectAttributes,
            ("DeleteObject" Core..=) Prelude.<$> deleteObject,
            ("DetachObject" Core..=) Prelude.<$> detachObject,
            ("DetachTypedLink" Core..=)
              Prelude.<$> detachTypedLink,
            ("UpdateLinkAttributes" Core..=)
              Prelude.<$> updateLinkAttributes,
            ("CreateObject" Core..=) Prelude.<$> createObject,
            ("AddFacetToObject" Core..=)
              Prelude.<$> addFacetToObject,
            ("DetachFromIndex" Core..=)
              Prelude.<$> detachFromIndex,
            ("AttachPolicy" Core..=) Prelude.<$> attachPolicy,
            ("CreateIndex" Core..=) Prelude.<$> createIndex,
            ("DetachPolicy" Core..=) Prelude.<$> detachPolicy,
            ("RemoveFacetFromObject" Core..=)
              Prelude.<$> removeFacetFromObject,
            ("AttachTypedLink" Core..=)
              Prelude.<$> attachTypedLink,
            ("AttachObject" Core..=) Prelude.<$> attachObject
          ]
      )
