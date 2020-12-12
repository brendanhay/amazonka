{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchWriteOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchWriteOperation
  ( BatchWriteOperation (..),

    -- * Smart constructor
    mkBatchWriteOperation,

    -- * Lenses
    bDeleteObject,
    bDetachFromIndex,
    bRemoveFacetFromObject,
    bAttachObject,
    bCreateObject,
    bAttachTypedLink,
    bDetachPolicy,
    bCreateIndex,
    bDetachObject,
    bAddFacetToObject,
    bDetachTypedLink,
    bUpdateObjectAttributes,
    bAttachPolicy,
    bUpdateLinkAttributes,
    bAttachToIndex,
  )
where

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @BatchWrite@ operation.
--
-- /See:/ 'mkBatchWriteOperation' smart constructor.
data BatchWriteOperation = BatchWriteOperation'
  { deleteObject ::
      Lude.Maybe BatchDeleteObject,
    detachFromIndex :: Lude.Maybe BatchDetachFromIndex,
    removeFacetFromObject ::
      Lude.Maybe BatchRemoveFacetFromObject,
    attachObject :: Lude.Maybe BatchAttachObject,
    createObject :: Lude.Maybe BatchCreateObject,
    attachTypedLink :: Lude.Maybe BatchAttachTypedLink,
    detachPolicy :: Lude.Maybe BatchDetachPolicy,
    createIndex :: Lude.Maybe BatchCreateIndex,
    detachObject :: Lude.Maybe BatchDetachObject,
    addFacetToObject ::
      Lude.Maybe BatchAddFacetToObject,
    detachTypedLink :: Lude.Maybe BatchDetachTypedLink,
    updateObjectAttributes ::
      Lude.Maybe BatchUpdateObjectAttributes,
    attachPolicy :: Lude.Maybe BatchAttachPolicy,
    updateLinkAttributes ::
      Lude.Maybe BatchUpdateLinkAttributes,
    attachToIndex :: Lude.Maybe BatchAttachToIndex
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchWriteOperation' with the minimum fields required to make a request.
--
-- * 'addFacetToObject' - A batch operation that adds a facet to an object.
-- * 'attachObject' - Attaches an object to a 'Directory' .
-- * 'attachPolicy' - Attaches a policy object to a regular object. An object can have a limited number of attached policies.
-- * 'attachToIndex' - Attaches the specified object to the specified index.
-- * 'attachTypedLink' - Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'createIndex' - Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
-- * 'createObject' - Creates an object.
-- * 'deleteObject' - Deletes an object in a 'Directory' .
-- * 'detachFromIndex' - Detaches the specified object from the specified index.
-- * 'detachObject' - Detaches an object from a 'Directory' .
-- * 'detachPolicy' - Detaches a policy from a 'Directory' .
-- * 'detachTypedLink' - Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'removeFacetFromObject' - A batch operation that removes a facet from an object.
-- * 'updateLinkAttributes' - Updates a given object's attributes.
-- * 'updateObjectAttributes' - Updates a given object's attributes.
mkBatchWriteOperation ::
  BatchWriteOperation
mkBatchWriteOperation =
  BatchWriteOperation'
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
bDeleteObject :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchDeleteObject)
bDeleteObject = Lens.lens (deleteObject :: BatchWriteOperation -> Lude.Maybe BatchDeleteObject) (\s a -> s {deleteObject = a} :: BatchWriteOperation)
{-# DEPRECATED bDeleteObject "Use generic-lens or generic-optics with 'deleteObject' instead." #-}

-- | Detaches the specified object from the specified index.
--
-- /Note:/ Consider using 'detachFromIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDetachFromIndex :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchDetachFromIndex)
bDetachFromIndex = Lens.lens (detachFromIndex :: BatchWriteOperation -> Lude.Maybe BatchDetachFromIndex) (\s a -> s {detachFromIndex = a} :: BatchWriteOperation)
{-# DEPRECATED bDetachFromIndex "Use generic-lens or generic-optics with 'detachFromIndex' instead." #-}

-- | A batch operation that removes a facet from an object.
--
-- /Note:/ Consider using 'removeFacetFromObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bRemoveFacetFromObject :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchRemoveFacetFromObject)
bRemoveFacetFromObject = Lens.lens (removeFacetFromObject :: BatchWriteOperation -> Lude.Maybe BatchRemoveFacetFromObject) (\s a -> s {removeFacetFromObject = a} :: BatchWriteOperation)
{-# DEPRECATED bRemoveFacetFromObject "Use generic-lens or generic-optics with 'removeFacetFromObject' instead." #-}

-- | Attaches an object to a 'Directory' .
--
-- /Note:/ Consider using 'attachObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bAttachObject :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchAttachObject)
bAttachObject = Lens.lens (attachObject :: BatchWriteOperation -> Lude.Maybe BatchAttachObject) (\s a -> s {attachObject = a} :: BatchWriteOperation)
{-# DEPRECATED bAttachObject "Use generic-lens or generic-optics with 'attachObject' instead." #-}

-- | Creates an object.
--
-- /Note:/ Consider using 'createObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreateObject :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchCreateObject)
bCreateObject = Lens.lens (createObject :: BatchWriteOperation -> Lude.Maybe BatchCreateObject) (\s a -> s {createObject = a} :: BatchWriteOperation)
{-# DEPRECATED bCreateObject "Use generic-lens or generic-optics with 'createObject' instead." #-}

-- | Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'attachTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bAttachTypedLink :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchAttachTypedLink)
bAttachTypedLink = Lens.lens (attachTypedLink :: BatchWriteOperation -> Lude.Maybe BatchAttachTypedLink) (\s a -> s {attachTypedLink = a} :: BatchWriteOperation)
{-# DEPRECATED bAttachTypedLink "Use generic-lens or generic-optics with 'attachTypedLink' instead." #-}

-- | Detaches a policy from a 'Directory' .
--
-- /Note:/ Consider using 'detachPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDetachPolicy :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchDetachPolicy)
bDetachPolicy = Lens.lens (detachPolicy :: BatchWriteOperation -> Lude.Maybe BatchDetachPolicy) (\s a -> s {detachPolicy = a} :: BatchWriteOperation)
{-# DEPRECATED bDetachPolicy "Use generic-lens or generic-optics with 'detachPolicy' instead." #-}

-- | Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
--
-- /Note:/ Consider using 'createIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreateIndex :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchCreateIndex)
bCreateIndex = Lens.lens (createIndex :: BatchWriteOperation -> Lude.Maybe BatchCreateIndex) (\s a -> s {createIndex = a} :: BatchWriteOperation)
{-# DEPRECATED bCreateIndex "Use generic-lens or generic-optics with 'createIndex' instead." #-}

-- | Detaches an object from a 'Directory' .
--
-- /Note:/ Consider using 'detachObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDetachObject :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchDetachObject)
bDetachObject = Lens.lens (detachObject :: BatchWriteOperation -> Lude.Maybe BatchDetachObject) (\s a -> s {detachObject = a} :: BatchWriteOperation)
{-# DEPRECATED bDetachObject "Use generic-lens or generic-optics with 'detachObject' instead." #-}

-- | A batch operation that adds a facet to an object.
--
-- /Note:/ Consider using 'addFacetToObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bAddFacetToObject :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchAddFacetToObject)
bAddFacetToObject = Lens.lens (addFacetToObject :: BatchWriteOperation -> Lude.Maybe BatchAddFacetToObject) (\s a -> s {addFacetToObject = a} :: BatchWriteOperation)
{-# DEPRECATED bAddFacetToObject "Use generic-lens or generic-optics with 'addFacetToObject' instead." #-}

-- | Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'detachTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDetachTypedLink :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchDetachTypedLink)
bDetachTypedLink = Lens.lens (detachTypedLink :: BatchWriteOperation -> Lude.Maybe BatchDetachTypedLink) (\s a -> s {detachTypedLink = a} :: BatchWriteOperation)
{-# DEPRECATED bDetachTypedLink "Use generic-lens or generic-optics with 'detachTypedLink' instead." #-}

-- | Updates a given object's attributes.
--
-- /Note:/ Consider using 'updateObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bUpdateObjectAttributes :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchUpdateObjectAttributes)
bUpdateObjectAttributes = Lens.lens (updateObjectAttributes :: BatchWriteOperation -> Lude.Maybe BatchUpdateObjectAttributes) (\s a -> s {updateObjectAttributes = a} :: BatchWriteOperation)
{-# DEPRECATED bUpdateObjectAttributes "Use generic-lens or generic-optics with 'updateObjectAttributes' instead." #-}

-- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
-- /Note:/ Consider using 'attachPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bAttachPolicy :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchAttachPolicy)
bAttachPolicy = Lens.lens (attachPolicy :: BatchWriteOperation -> Lude.Maybe BatchAttachPolicy) (\s a -> s {attachPolicy = a} :: BatchWriteOperation)
{-# DEPRECATED bAttachPolicy "Use generic-lens or generic-optics with 'attachPolicy' instead." #-}

-- | Updates a given object's attributes.
--
-- /Note:/ Consider using 'updateLinkAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bUpdateLinkAttributes :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchUpdateLinkAttributes)
bUpdateLinkAttributes = Lens.lens (updateLinkAttributes :: BatchWriteOperation -> Lude.Maybe BatchUpdateLinkAttributes) (\s a -> s {updateLinkAttributes = a} :: BatchWriteOperation)
{-# DEPRECATED bUpdateLinkAttributes "Use generic-lens or generic-optics with 'updateLinkAttributes' instead." #-}

-- | Attaches the specified object to the specified index.
--
-- /Note:/ Consider using 'attachToIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bAttachToIndex :: Lens.Lens' BatchWriteOperation (Lude.Maybe BatchAttachToIndex)
bAttachToIndex = Lens.lens (attachToIndex :: BatchWriteOperation -> Lude.Maybe BatchAttachToIndex) (\s a -> s {attachToIndex = a} :: BatchWriteOperation)
{-# DEPRECATED bAttachToIndex "Use generic-lens or generic-optics with 'attachToIndex' instead." #-}

instance Lude.ToJSON BatchWriteOperation where
  toJSON BatchWriteOperation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeleteObject" Lude..=) Lude.<$> deleteObject,
            ("DetachFromIndex" Lude..=) Lude.<$> detachFromIndex,
            ("RemoveFacetFromObject" Lude..=) Lude.<$> removeFacetFromObject,
            ("AttachObject" Lude..=) Lude.<$> attachObject,
            ("CreateObject" Lude..=) Lude.<$> createObject,
            ("AttachTypedLink" Lude..=) Lude.<$> attachTypedLink,
            ("DetachPolicy" Lude..=) Lude.<$> detachPolicy,
            ("CreateIndex" Lude..=) Lude.<$> createIndex,
            ("DetachObject" Lude..=) Lude.<$> detachObject,
            ("AddFacetToObject" Lude..=) Lude.<$> addFacetToObject,
            ("DetachTypedLink" Lude..=) Lude.<$> detachTypedLink,
            ("UpdateObjectAttributes" Lude..=) Lude.<$> updateObjectAttributes,
            ("AttachPolicy" Lude..=) Lude.<$> attachPolicy,
            ("UpdateLinkAttributes" Lude..=) Lude.<$> updateLinkAttributes,
            ("AttachToIndex" Lude..=) Lude.<$> attachToIndex
          ]
      )
