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
    bwoAddFacetToObject,
    bwoAttachObject,
    bwoAttachPolicy,
    bwoAttachToIndex,
    bwoAttachTypedLink,
    bwoCreateIndex,
    bwoCreateObject,
    bwoDeleteObject,
    bwoDetachFromIndex,
    bwoDetachObject,
    bwoDetachPolicy,
    bwoDetachTypedLink,
    bwoRemoveFacetFromObject,
    bwoUpdateLinkAttributes,
    bwoUpdateObjectAttributes,
  )
where

import qualified Network.AWS.CloudDirectory.Types.BatchAddFacetToObject as Types
import qualified Network.AWS.CloudDirectory.Types.BatchAttachObject as Types
import qualified Network.AWS.CloudDirectory.Types.BatchAttachPolicy as Types
import qualified Network.AWS.CloudDirectory.Types.BatchAttachToIndex as Types
import qualified Network.AWS.CloudDirectory.Types.BatchAttachTypedLink as Types
import qualified Network.AWS.CloudDirectory.Types.BatchCreateIndex as Types
import qualified Network.AWS.CloudDirectory.Types.BatchCreateObject as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDeleteObject as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDetachFromIndex as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDetachObject as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDetachPolicy as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDetachTypedLink as Types
import qualified Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject as Types
import qualified Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes as Types
import qualified Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @BatchWrite@ operation.
--
-- /See:/ 'mkBatchWriteOperation' smart constructor.
data BatchWriteOperation = BatchWriteOperation'
  { -- | A batch operation that adds a facet to an object.
    addFacetToObject :: Core.Maybe Types.BatchAddFacetToObject,
    -- | Attaches an object to a 'Directory' .
    attachObject :: Core.Maybe Types.BatchAttachObject,
    -- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
    attachPolicy :: Core.Maybe Types.BatchAttachPolicy,
    -- | Attaches the specified object to the specified index.
    attachToIndex :: Core.Maybe Types.BatchAttachToIndex,
    -- | Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    attachTypedLink :: Core.Maybe Types.BatchAttachTypedLink,
    -- | Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
    createIndex :: Core.Maybe Types.BatchCreateIndex,
    -- | Creates an object.
    createObject :: Core.Maybe Types.BatchCreateObject,
    -- | Deletes an object in a 'Directory' .
    deleteObject :: Core.Maybe Types.BatchDeleteObject,
    -- | Detaches the specified object from the specified index.
    detachFromIndex :: Core.Maybe Types.BatchDetachFromIndex,
    -- | Detaches an object from a 'Directory' .
    detachObject :: Core.Maybe Types.BatchDetachObject,
    -- | Detaches a policy from a 'Directory' .
    detachPolicy :: Core.Maybe Types.BatchDetachPolicy,
    -- | Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    detachTypedLink :: Core.Maybe Types.BatchDetachTypedLink,
    -- | A batch operation that removes a facet from an object.
    removeFacetFromObject :: Core.Maybe Types.BatchRemoveFacetFromObject,
    -- | Updates a given object's attributes.
    updateLinkAttributes :: Core.Maybe Types.BatchUpdateLinkAttributes,
    -- | Updates a given object's attributes.
    updateObjectAttributes :: Core.Maybe Types.BatchUpdateObjectAttributes
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchWriteOperation' value with any optional fields omitted.
mkBatchWriteOperation ::
  BatchWriteOperation
mkBatchWriteOperation =
  BatchWriteOperation'
    { addFacetToObject = Core.Nothing,
      attachObject = Core.Nothing,
      attachPolicy = Core.Nothing,
      attachToIndex = Core.Nothing,
      attachTypedLink = Core.Nothing,
      createIndex = Core.Nothing,
      createObject = Core.Nothing,
      deleteObject = Core.Nothing,
      detachFromIndex = Core.Nothing,
      detachObject = Core.Nothing,
      detachPolicy = Core.Nothing,
      detachTypedLink = Core.Nothing,
      removeFacetFromObject = Core.Nothing,
      updateLinkAttributes = Core.Nothing,
      updateObjectAttributes = Core.Nothing
    }

-- | A batch operation that adds a facet to an object.
--
-- /Note:/ Consider using 'addFacetToObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAddFacetToObject :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchAddFacetToObject)
bwoAddFacetToObject = Lens.field @"addFacetToObject"
{-# DEPRECATED bwoAddFacetToObject "Use generic-lens or generic-optics with 'addFacetToObject' instead." #-}

-- | Attaches an object to a 'Directory' .
--
-- /Note:/ Consider using 'attachObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAttachObject :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchAttachObject)
bwoAttachObject = Lens.field @"attachObject"
{-# DEPRECATED bwoAttachObject "Use generic-lens or generic-optics with 'attachObject' instead." #-}

-- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
-- /Note:/ Consider using 'attachPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAttachPolicy :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchAttachPolicy)
bwoAttachPolicy = Lens.field @"attachPolicy"
{-# DEPRECATED bwoAttachPolicy "Use generic-lens or generic-optics with 'attachPolicy' instead." #-}

-- | Attaches the specified object to the specified index.
--
-- /Note:/ Consider using 'attachToIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAttachToIndex :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchAttachToIndex)
bwoAttachToIndex = Lens.field @"attachToIndex"
{-# DEPRECATED bwoAttachToIndex "Use generic-lens or generic-optics with 'attachToIndex' instead." #-}

-- | Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'attachTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoAttachTypedLink :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchAttachTypedLink)
bwoAttachTypedLink = Lens.field @"attachTypedLink"
{-# DEPRECATED bwoAttachTypedLink "Use generic-lens or generic-optics with 'attachTypedLink' instead." #-}

-- | Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
--
-- /Note:/ Consider using 'createIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoCreateIndex :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchCreateIndex)
bwoCreateIndex = Lens.field @"createIndex"
{-# DEPRECATED bwoCreateIndex "Use generic-lens or generic-optics with 'createIndex' instead." #-}

-- | Creates an object.
--
-- /Note:/ Consider using 'createObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoCreateObject :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchCreateObject)
bwoCreateObject = Lens.field @"createObject"
{-# DEPRECATED bwoCreateObject "Use generic-lens or generic-optics with 'createObject' instead." #-}

-- | Deletes an object in a 'Directory' .
--
-- /Note:/ Consider using 'deleteObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDeleteObject :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchDeleteObject)
bwoDeleteObject = Lens.field @"deleteObject"
{-# DEPRECATED bwoDeleteObject "Use generic-lens or generic-optics with 'deleteObject' instead." #-}

-- | Detaches the specified object from the specified index.
--
-- /Note:/ Consider using 'detachFromIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDetachFromIndex :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchDetachFromIndex)
bwoDetachFromIndex = Lens.field @"detachFromIndex"
{-# DEPRECATED bwoDetachFromIndex "Use generic-lens or generic-optics with 'detachFromIndex' instead." #-}

-- | Detaches an object from a 'Directory' .
--
-- /Note:/ Consider using 'detachObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDetachObject :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchDetachObject)
bwoDetachObject = Lens.field @"detachObject"
{-# DEPRECATED bwoDetachObject "Use generic-lens or generic-optics with 'detachObject' instead." #-}

-- | Detaches a policy from a 'Directory' .
--
-- /Note:/ Consider using 'detachPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDetachPolicy :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchDetachPolicy)
bwoDetachPolicy = Lens.field @"detachPolicy"
{-# DEPRECATED bwoDetachPolicy "Use generic-lens or generic-optics with 'detachPolicy' instead." #-}

-- | Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'detachTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoDetachTypedLink :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchDetachTypedLink)
bwoDetachTypedLink = Lens.field @"detachTypedLink"
{-# DEPRECATED bwoDetachTypedLink "Use generic-lens or generic-optics with 'detachTypedLink' instead." #-}

-- | A batch operation that removes a facet from an object.
--
-- /Note:/ Consider using 'removeFacetFromObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoRemoveFacetFromObject :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchRemoveFacetFromObject)
bwoRemoveFacetFromObject = Lens.field @"removeFacetFromObject"
{-# DEPRECATED bwoRemoveFacetFromObject "Use generic-lens or generic-optics with 'removeFacetFromObject' instead." #-}

-- | Updates a given object's attributes.
--
-- /Note:/ Consider using 'updateLinkAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoUpdateLinkAttributes :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchUpdateLinkAttributes)
bwoUpdateLinkAttributes = Lens.field @"updateLinkAttributes"
{-# DEPRECATED bwoUpdateLinkAttributes "Use generic-lens or generic-optics with 'updateLinkAttributes' instead." #-}

-- | Updates a given object's attributes.
--
-- /Note:/ Consider using 'updateObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwoUpdateObjectAttributes :: Lens.Lens' BatchWriteOperation (Core.Maybe Types.BatchUpdateObjectAttributes)
bwoUpdateObjectAttributes = Lens.field @"updateObjectAttributes"
{-# DEPRECATED bwoUpdateObjectAttributes "Use generic-lens or generic-optics with 'updateObjectAttributes' instead." #-}

instance Core.FromJSON BatchWriteOperation where
  toJSON BatchWriteOperation {..} =
    Core.object
      ( Core.catMaybes
          [ ("AddFacetToObject" Core..=) Core.<$> addFacetToObject,
            ("AttachObject" Core..=) Core.<$> attachObject,
            ("AttachPolicy" Core..=) Core.<$> attachPolicy,
            ("AttachToIndex" Core..=) Core.<$> attachToIndex,
            ("AttachTypedLink" Core..=) Core.<$> attachTypedLink,
            ("CreateIndex" Core..=) Core.<$> createIndex,
            ("CreateObject" Core..=) Core.<$> createObject,
            ("DeleteObject" Core..=) Core.<$> deleteObject,
            ("DetachFromIndex" Core..=) Core.<$> detachFromIndex,
            ("DetachObject" Core..=) Core.<$> detachObject,
            ("DetachPolicy" Core..=) Core.<$> detachPolicy,
            ("DetachTypedLink" Core..=) Core.<$> detachTypedLink,
            ("RemoveFacetFromObject" Core..=) Core.<$> removeFacetFromObject,
            ("UpdateLinkAttributes" Core..=) Core.<$> updateLinkAttributes,
            ("UpdateObjectAttributes" Core..=)
              Core.<$> updateObjectAttributes
          ]
      )
