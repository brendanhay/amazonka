{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse
  ( BatchWriteOperationResponse (..)
  -- * Smart constructor
  , mkBatchWriteOperationResponse
  -- * Lenses
  , bworAddFacetToObject
  , bworAttachObject
  , bworAttachPolicy
  , bworAttachToIndex
  , bworAttachTypedLink
  , bworCreateIndex
  , bworCreateObject
  , bworDeleteObject
  , bworDetachFromIndex
  , bworDetachObject
  , bworDetachPolicy
  , bworDetachTypedLink
  , bworRemoveFacetFromObject
  , bworUpdateLinkAttributes
  , bworUpdateObjectAttributes
  ) where

import qualified Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchAttachPolicyResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDeleteObjectResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDetachPolicyResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @BatchWrite@ response operation.
--
-- /See:/ 'mkBatchWriteOperationResponse' smart constructor.
data BatchWriteOperationResponse = BatchWriteOperationResponse'
  { addFacetToObject :: Core.Maybe Types.BatchAddFacetToObjectResponse
    -- ^ The result of an add facet to object batch operation.
  , attachObject :: Core.Maybe Types.BatchAttachObjectResponse
    -- ^ Attaches an object to a 'Directory' .
  , attachPolicy :: Core.Maybe Types.BatchAttachPolicyResponse
    -- ^ Attaches a policy object to a regular object. An object can have a limited number of attached policies.
  , attachToIndex :: Core.Maybe Types.BatchAttachToIndexResponse
    -- ^ Attaches the specified object to the specified index.
  , attachTypedLink :: Core.Maybe Types.BatchAttachTypedLinkResponse
    -- ^ Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
  , createIndex :: Core.Maybe Types.BatchCreateIndexResponse
    -- ^ Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
  , createObject :: Core.Maybe Types.BatchCreateObjectResponse
    -- ^ Creates an object in a 'Directory' .
  , deleteObject :: Core.Maybe Types.BatchDeleteObjectResponse
    -- ^ Deletes an object in a 'Directory' .
  , detachFromIndex :: Core.Maybe Types.BatchDetachFromIndexResponse
    -- ^ Detaches the specified object from the specified index.
  , detachObject :: Core.Maybe Types.BatchDetachObjectResponse
    -- ^ Detaches an object from a 'Directory' .
  , detachPolicy :: Core.Maybe Types.BatchDetachPolicyResponse
    -- ^ Detaches a policy from a 'Directory' .
  , detachTypedLink :: Core.Maybe Types.BatchDetachTypedLinkResponse
    -- ^ Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
  , removeFacetFromObject :: Core.Maybe Types.BatchRemoveFacetFromObjectResponse
    -- ^ The result of a batch remove facet from object operation.
  , updateLinkAttributes :: Core.Maybe Types.BatchUpdateLinkAttributesResponse
    -- ^ Represents the output of a @BatchWrite@ response operation.
  , updateObjectAttributes :: Core.Maybe Types.BatchUpdateObjectAttributesResponse
    -- ^ Updates a given object’s attributes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchWriteOperationResponse' value with any optional fields omitted.
mkBatchWriteOperationResponse
    :: BatchWriteOperationResponse
mkBatchWriteOperationResponse
  = BatchWriteOperationResponse'{addFacetToObject = Core.Nothing,
                                 attachObject = Core.Nothing, attachPolicy = Core.Nothing,
                                 attachToIndex = Core.Nothing, attachTypedLink = Core.Nothing,
                                 createIndex = Core.Nothing, createObject = Core.Nothing,
                                 deleteObject = Core.Nothing, detachFromIndex = Core.Nothing,
                                 detachObject = Core.Nothing, detachPolicy = Core.Nothing,
                                 detachTypedLink = Core.Nothing,
                                 removeFacetFromObject = Core.Nothing,
                                 updateLinkAttributes = Core.Nothing,
                                 updateObjectAttributes = Core.Nothing}

-- | The result of an add facet to object batch operation.
--
-- /Note:/ Consider using 'addFacetToObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworAddFacetToObject :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchAddFacetToObjectResponse)
bworAddFacetToObject = Lens.field @"addFacetToObject"
{-# INLINEABLE bworAddFacetToObject #-}
{-# DEPRECATED addFacetToObject "Use generic-lens or generic-optics with 'addFacetToObject' instead"  #-}

-- | Attaches an object to a 'Directory' .
--
-- /Note:/ Consider using 'attachObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworAttachObject :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchAttachObjectResponse)
bworAttachObject = Lens.field @"attachObject"
{-# INLINEABLE bworAttachObject #-}
{-# DEPRECATED attachObject "Use generic-lens or generic-optics with 'attachObject' instead"  #-}

-- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
-- /Note:/ Consider using 'attachPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworAttachPolicy :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchAttachPolicyResponse)
bworAttachPolicy = Lens.field @"attachPolicy"
{-# INLINEABLE bworAttachPolicy #-}
{-# DEPRECATED attachPolicy "Use generic-lens or generic-optics with 'attachPolicy' instead"  #-}

-- | Attaches the specified object to the specified index.
--
-- /Note:/ Consider using 'attachToIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworAttachToIndex :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchAttachToIndexResponse)
bworAttachToIndex = Lens.field @"attachToIndex"
{-# INLINEABLE bworAttachToIndex #-}
{-# DEPRECATED attachToIndex "Use generic-lens or generic-optics with 'attachToIndex' instead"  #-}

-- | Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'attachTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworAttachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchAttachTypedLinkResponse)
bworAttachTypedLink = Lens.field @"attachTypedLink"
{-# INLINEABLE bworAttachTypedLink #-}
{-# DEPRECATED attachTypedLink "Use generic-lens or generic-optics with 'attachTypedLink' instead"  #-}

-- | Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
--
-- /Note:/ Consider using 'createIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworCreateIndex :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchCreateIndexResponse)
bworCreateIndex = Lens.field @"createIndex"
{-# INLINEABLE bworCreateIndex #-}
{-# DEPRECATED createIndex "Use generic-lens or generic-optics with 'createIndex' instead"  #-}

-- | Creates an object in a 'Directory' .
--
-- /Note:/ Consider using 'createObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworCreateObject :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchCreateObjectResponse)
bworCreateObject = Lens.field @"createObject"
{-# INLINEABLE bworCreateObject #-}
{-# DEPRECATED createObject "Use generic-lens or generic-optics with 'createObject' instead"  #-}

-- | Deletes an object in a 'Directory' .
--
-- /Note:/ Consider using 'deleteObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworDeleteObject :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchDeleteObjectResponse)
bworDeleteObject = Lens.field @"deleteObject"
{-# INLINEABLE bworDeleteObject #-}
{-# DEPRECATED deleteObject "Use generic-lens or generic-optics with 'deleteObject' instead"  #-}

-- | Detaches the specified object from the specified index.
--
-- /Note:/ Consider using 'detachFromIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworDetachFromIndex :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchDetachFromIndexResponse)
bworDetachFromIndex = Lens.field @"detachFromIndex"
{-# INLINEABLE bworDetachFromIndex #-}
{-# DEPRECATED detachFromIndex "Use generic-lens or generic-optics with 'detachFromIndex' instead"  #-}

-- | Detaches an object from a 'Directory' .
--
-- /Note:/ Consider using 'detachObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworDetachObject :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchDetachObjectResponse)
bworDetachObject = Lens.field @"detachObject"
{-# INLINEABLE bworDetachObject #-}
{-# DEPRECATED detachObject "Use generic-lens or generic-optics with 'detachObject' instead"  #-}

-- | Detaches a policy from a 'Directory' .
--
-- /Note:/ Consider using 'detachPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworDetachPolicy :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchDetachPolicyResponse)
bworDetachPolicy = Lens.field @"detachPolicy"
{-# INLINEABLE bworDetachPolicy #-}
{-# DEPRECATED detachPolicy "Use generic-lens or generic-optics with 'detachPolicy' instead"  #-}

-- | Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'detachTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworDetachTypedLink :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchDetachTypedLinkResponse)
bworDetachTypedLink = Lens.field @"detachTypedLink"
{-# INLINEABLE bworDetachTypedLink #-}
{-# DEPRECATED detachTypedLink "Use generic-lens or generic-optics with 'detachTypedLink' instead"  #-}

-- | The result of a batch remove facet from object operation.
--
-- /Note:/ Consider using 'removeFacetFromObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworRemoveFacetFromObject :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchRemoveFacetFromObjectResponse)
bworRemoveFacetFromObject = Lens.field @"removeFacetFromObject"
{-# INLINEABLE bworRemoveFacetFromObject #-}
{-# DEPRECATED removeFacetFromObject "Use generic-lens or generic-optics with 'removeFacetFromObject' instead"  #-}

-- | Represents the output of a @BatchWrite@ response operation.
--
-- /Note:/ Consider using 'updateLinkAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworUpdateLinkAttributes :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchUpdateLinkAttributesResponse)
bworUpdateLinkAttributes = Lens.field @"updateLinkAttributes"
{-# INLINEABLE bworUpdateLinkAttributes #-}
{-# DEPRECATED updateLinkAttributes "Use generic-lens or generic-optics with 'updateLinkAttributes' instead"  #-}

-- | Updates a given object’s attributes.
--
-- /Note:/ Consider using 'updateObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bworUpdateObjectAttributes :: Lens.Lens' BatchWriteOperationResponse (Core.Maybe Types.BatchUpdateObjectAttributesResponse)
bworUpdateObjectAttributes = Lens.field @"updateObjectAttributes"
{-# INLINEABLE bworUpdateObjectAttributes #-}
{-# DEPRECATED updateObjectAttributes "Use generic-lens or generic-optics with 'updateObjectAttributes' instead"  #-}

instance Core.FromJSON BatchWriteOperationResponse where
        parseJSON
          = Core.withObject "BatchWriteOperationResponse" Core.$
              \ x ->
                BatchWriteOperationResponse' Core.<$>
                  (x Core..:? "AddFacetToObject") Core.<*> x Core..:? "AttachObject"
                    Core.<*> x Core..:? "AttachPolicy"
                    Core.<*> x Core..:? "AttachToIndex"
                    Core.<*> x Core..:? "AttachTypedLink"
                    Core.<*> x Core..:? "CreateIndex"
                    Core.<*> x Core..:? "CreateObject"
                    Core.<*> x Core..:? "DeleteObject"
                    Core.<*> x Core..:? "DetachFromIndex"
                    Core.<*> x Core..:? "DetachObject"
                    Core.<*> x Core..:? "DetachPolicy"
                    Core.<*> x Core..:? "DetachTypedLink"
                    Core.<*> x Core..:? "RemoveFacetFromObject"
                    Core.<*> x Core..:? "UpdateLinkAttributes"
                    Core.<*> x Core..:? "UpdateObjectAttributes"
