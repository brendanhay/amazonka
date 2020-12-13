{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse
  ( BatchWriteOperationResponse (..),

    -- * Smart constructor
    mkBatchWriteOperationResponse,

    -- * Lenses
    bwoDeleteObject,
    bwoDetachFromIndex,
    bwoRemoveFacetFromObject,
    bwoAttachObject,
    bwoCreateObject,
    bwoAttachTypedLink,
    bwoDetachPolicy,
    bwoCreateIndex,
    bwoDetachObject,
    bwoAddFacetToObject,
    bwoDetachTypedLink,
    bwoUpdateObjectAttributes,
    bwoAttachPolicy,
    bwoUpdateLinkAttributes,
    bwoAttachToIndex,
  )
where

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @BatchWrite@ response operation.
--
-- /See:/ 'mkBatchWriteOperationResponse' smart constructor.
data BatchWriteOperationResponse = BatchWriteOperationResponse'
  { -- | Deletes an object in a 'Directory' .
    deleteObject :: Lude.Maybe BatchDeleteObjectResponse,
    -- | Detaches the specified object from the specified index.
    detachFromIndex :: Lude.Maybe BatchDetachFromIndexResponse,
    -- | The result of a batch remove facet from object operation.
    removeFacetFromObject :: Lude.Maybe BatchRemoveFacetFromObjectResponse,
    -- | Attaches an object to a 'Directory' .
    attachObject :: Lude.Maybe BatchAttachObjectResponse,
    -- | Creates an object in a 'Directory' .
    createObject :: Lude.Maybe BatchCreateObjectResponse,
    -- | Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    attachTypedLink :: Lude.Maybe BatchAttachTypedLinkResponse,
    -- | Detaches a policy from a 'Directory' .
    detachPolicy :: Lude.Maybe BatchDetachPolicyResponse,
    -- | Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
    createIndex :: Lude.Maybe BatchCreateIndexResponse,
    -- | Detaches an object from a 'Directory' .
    detachObject :: Lude.Maybe BatchDetachObjectResponse,
    -- | The result of an add facet to object batch operation.
    addFacetToObject :: Lude.Maybe BatchAddFacetToObjectResponse,
    -- | Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    detachTypedLink :: Lude.Maybe BatchDetachTypedLinkResponse,
    -- | Updates a given object’s attributes.
    updateObjectAttributes :: Lude.Maybe BatchUpdateObjectAttributesResponse,
    -- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
    attachPolicy :: Lude.Maybe BatchAttachPolicyResponse,
    -- | Represents the output of a @BatchWrite@ response operation.
    updateLinkAttributes :: Lude.Maybe BatchUpdateLinkAttributesResponse,
    -- | Attaches the specified object to the specified index.
    attachToIndex :: Lude.Maybe BatchAttachToIndexResponse
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchWriteOperationResponse' with the minimum fields required to make a request.
--
-- * 'deleteObject' - Deletes an object in a 'Directory' .
-- * 'detachFromIndex' - Detaches the specified object from the specified index.
-- * 'removeFacetFromObject' - The result of a batch remove facet from object operation.
-- * 'attachObject' - Attaches an object to a 'Directory' .
-- * 'createObject' - Creates an object in a 'Directory' .
-- * 'attachTypedLink' - Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'detachPolicy' - Detaches a policy from a 'Directory' .
-- * 'createIndex' - Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
-- * 'detachObject' - Detaches an object from a 'Directory' .
-- * 'addFacetToObject' - The result of an add facet to object batch operation.
-- * 'detachTypedLink' - Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'updateObjectAttributes' - Updates a given object’s attributes.
-- * 'attachPolicy' - Attaches a policy object to a regular object. An object can have a limited number of attached policies.
-- * 'updateLinkAttributes' - Represents the output of a @BatchWrite@ response operation.
-- * 'attachToIndex' - Attaches the specified object to the specified index.
mkBatchWriteOperationResponse ::
  BatchWriteOperationResponse
mkBatchWriteOperationResponse =
  BatchWriteOperationResponse'
    { deleteObject = Lude.Nothing,
      detachFromIndex = Lude.Nothing,
      removeFacetFromObject = Lude.Nothing,
      attachObject = Lude.Nothing,
      createObject = Lude.Nothing,
      attachTypedLink = Lude.Nothing,
      detachPolicy = Lude.Nothing,
      createIndex = Lude.Nothing,
      detachObject = Lude.Nothing,
      addFacetToObject = Lude.Nothing,
      detachTypedLink = Lude.Nothing,
      updateObjectAttributes = Lude.Nothing,
      attachPolicy = Lude.Nothing,
      updateLinkAttributes = Lude.Nothing,
      attachToIndex = Lude.Nothing
    }

-- | Deletes an object in a 'Directory' .
--
-- /Note:/ Consider using 'deleteObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDeleteObject :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchDeleteObjectResponse)
bwoDeleteObject = Lens.lens (deleteObject :: BatchWriteOperationResponse -> Lude.Maybe BatchDeleteObjectResponse) (\s a -> s {deleteObject = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoDeleteObject "Use generic-lens or generic-optics with 'deleteObject' instead." #-}

-- | Detaches the specified object from the specified index.
--
-- /Note:/ Consider using 'detachFromIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDetachFromIndex :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchDetachFromIndexResponse)
bwoDetachFromIndex = Lens.lens (detachFromIndex :: BatchWriteOperationResponse -> Lude.Maybe BatchDetachFromIndexResponse) (\s a -> s {detachFromIndex = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoDetachFromIndex "Use generic-lens or generic-optics with 'detachFromIndex' instead." #-}

-- | The result of a batch remove facet from object operation.
--
-- /Note:/ Consider using 'removeFacetFromObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoRemoveFacetFromObject :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchRemoveFacetFromObjectResponse)
bwoRemoveFacetFromObject = Lens.lens (removeFacetFromObject :: BatchWriteOperationResponse -> Lude.Maybe BatchRemoveFacetFromObjectResponse) (\s a -> s {removeFacetFromObject = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoRemoveFacetFromObject "Use generic-lens or generic-optics with 'removeFacetFromObject' instead." #-}

-- | Attaches an object to a 'Directory' .
--
-- /Note:/ Consider using 'attachObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAttachObject :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchAttachObjectResponse)
bwoAttachObject = Lens.lens (attachObject :: BatchWriteOperationResponse -> Lude.Maybe BatchAttachObjectResponse) (\s a -> s {attachObject = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoAttachObject "Use generic-lens or generic-optics with 'attachObject' instead." #-}

-- | Creates an object in a 'Directory' .
--
-- /Note:/ Consider using 'createObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoCreateObject :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchCreateObjectResponse)
bwoCreateObject = Lens.lens (createObject :: BatchWriteOperationResponse -> Lude.Maybe BatchCreateObjectResponse) (\s a -> s {createObject = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoCreateObject "Use generic-lens or generic-optics with 'createObject' instead." #-}

-- | Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'attachTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAttachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchAttachTypedLinkResponse)
bwoAttachTypedLink = Lens.lens (attachTypedLink :: BatchWriteOperationResponse -> Lude.Maybe BatchAttachTypedLinkResponse) (\s a -> s {attachTypedLink = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoAttachTypedLink "Use generic-lens or generic-optics with 'attachTypedLink' instead." #-}

-- | Detaches a policy from a 'Directory' .
--
-- /Note:/ Consider using 'detachPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDetachPolicy :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchDetachPolicyResponse)
bwoDetachPolicy = Lens.lens (detachPolicy :: BatchWriteOperationResponse -> Lude.Maybe BatchDetachPolicyResponse) (\s a -> s {detachPolicy = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoDetachPolicy "Use generic-lens or generic-optics with 'detachPolicy' instead." #-}

-- | Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
--
-- /Note:/ Consider using 'createIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoCreateIndex :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchCreateIndexResponse)
bwoCreateIndex = Lens.lens (createIndex :: BatchWriteOperationResponse -> Lude.Maybe BatchCreateIndexResponse) (\s a -> s {createIndex = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoCreateIndex "Use generic-lens or generic-optics with 'createIndex' instead." #-}

-- | Detaches an object from a 'Directory' .
--
-- /Note:/ Consider using 'detachObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDetachObject :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchDetachObjectResponse)
bwoDetachObject = Lens.lens (detachObject :: BatchWriteOperationResponse -> Lude.Maybe BatchDetachObjectResponse) (\s a -> s {detachObject = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoDetachObject "Use generic-lens or generic-optics with 'detachObject' instead." #-}

-- | The result of an add facet to object batch operation.
--
-- /Note:/ Consider using 'addFacetToObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAddFacetToObject :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchAddFacetToObjectResponse)
bwoAddFacetToObject = Lens.lens (addFacetToObject :: BatchWriteOperationResponse -> Lude.Maybe BatchAddFacetToObjectResponse) (\s a -> s {addFacetToObject = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoAddFacetToObject "Use generic-lens or generic-optics with 'addFacetToObject' instead." #-}

-- | Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'detachTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDetachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchDetachTypedLinkResponse)
bwoDetachTypedLink = Lens.lens (detachTypedLink :: BatchWriteOperationResponse -> Lude.Maybe BatchDetachTypedLinkResponse) (\s a -> s {detachTypedLink = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoDetachTypedLink "Use generic-lens or generic-optics with 'detachTypedLink' instead." #-}

-- | Updates a given object’s attributes.
--
-- /Note:/ Consider using 'updateObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoUpdateObjectAttributes :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchUpdateObjectAttributesResponse)
bwoUpdateObjectAttributes = Lens.lens (updateObjectAttributes :: BatchWriteOperationResponse -> Lude.Maybe BatchUpdateObjectAttributesResponse) (\s a -> s {updateObjectAttributes = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoUpdateObjectAttributes "Use generic-lens or generic-optics with 'updateObjectAttributes' instead." #-}

-- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
-- /Note:/ Consider using 'attachPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAttachPolicy :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchAttachPolicyResponse)
bwoAttachPolicy = Lens.lens (attachPolicy :: BatchWriteOperationResponse -> Lude.Maybe BatchAttachPolicyResponse) (\s a -> s {attachPolicy = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoAttachPolicy "Use generic-lens or generic-optics with 'attachPolicy' instead." #-}

-- | Represents the output of a @BatchWrite@ response operation.
--
-- /Note:/ Consider using 'updateLinkAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoUpdateLinkAttributes :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchUpdateLinkAttributesResponse)
bwoUpdateLinkAttributes = Lens.lens (updateLinkAttributes :: BatchWriteOperationResponse -> Lude.Maybe BatchUpdateLinkAttributesResponse) (\s a -> s {updateLinkAttributes = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoUpdateLinkAttributes "Use generic-lens or generic-optics with 'updateLinkAttributes' instead." #-}

-- | Attaches the specified object to the specified index.
--
-- /Note:/ Consider using 'attachToIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAttachToIndex :: Lens.Lens' BatchWriteOperationResponse (Lude.Maybe BatchAttachToIndexResponse)
bwoAttachToIndex = Lens.lens (attachToIndex :: BatchWriteOperationResponse -> Lude.Maybe BatchAttachToIndexResponse) (\s a -> s {attachToIndex = a} :: BatchWriteOperationResponse)
{-# DEPRECATED bwoAttachToIndex "Use generic-lens or generic-optics with 'attachToIndex' instead." #-}

instance Lude.FromJSON BatchWriteOperationResponse where
  parseJSON =
    Lude.withObject
      "BatchWriteOperationResponse"
      ( \x ->
          BatchWriteOperationResponse'
            Lude.<$> (x Lude..:? "DeleteObject")
            Lude.<*> (x Lude..:? "DetachFromIndex")
            Lude.<*> (x Lude..:? "RemoveFacetFromObject")
            Lude.<*> (x Lude..:? "AttachObject")
            Lude.<*> (x Lude..:? "CreateObject")
            Lude.<*> (x Lude..:? "AttachTypedLink")
            Lude.<*> (x Lude..:? "DetachPolicy")
            Lude.<*> (x Lude..:? "CreateIndex")
            Lude.<*> (x Lude..:? "DetachObject")
            Lude.<*> (x Lude..:? "AddFacetToObject")
            Lude.<*> (x Lude..:? "DetachTypedLink")
            Lude.<*> (x Lude..:? "UpdateObjectAttributes")
            Lude.<*> (x Lude..:? "AttachPolicy")
            Lude.<*> (x Lude..:? "UpdateLinkAttributes")
            Lude.<*> (x Lude..:? "AttachToIndex")
      )
