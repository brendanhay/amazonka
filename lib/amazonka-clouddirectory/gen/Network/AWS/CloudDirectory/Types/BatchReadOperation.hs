{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchReadOperation
  ( BatchReadOperation (..)
  -- * Smart constructor
  , mkBatchReadOperation
  -- * Lenses
  , broGetLinkAttributes
  , broGetObjectAttributes
  , broGetObjectInformation
  , broListAttachedIndices
  , broListIncomingTypedLinks
  , broListIndex
  , broListObjectAttributes
  , broListObjectChildren
  , broListObjectParentPaths
  , broListObjectParents
  , broListObjectPolicies
  , broListOutgoingTypedLinks
  , broListPolicyAttachments
  , broLookupPolicy
  ) where

import qualified Network.AWS.CloudDirectory.Types.BatchGetLinkAttributes as Types
import qualified Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes as Types
import qualified Network.AWS.CloudDirectory.Types.BatchGetObjectInformation as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListAttachedIndices as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinks as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListIndex as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectAttributes as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectChildren as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectParents as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectPolicies as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments as Types
import qualified Network.AWS.CloudDirectory.Types.BatchLookupPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @BatchRead@ operation.
--
-- /See:/ 'mkBatchReadOperation' smart constructor.
data BatchReadOperation = BatchReadOperation'
  { getLinkAttributes :: Core.Maybe Types.BatchGetLinkAttributes
    -- ^ Retrieves attributes that are associated with a typed link.
  , getObjectAttributes :: Core.Maybe Types.BatchGetObjectAttributes
    -- ^ Retrieves attributes within a facet that are associated with an object.
  , getObjectInformation :: Core.Maybe Types.BatchGetObjectInformation
    -- ^ Retrieves metadata about an object.
  , listAttachedIndices :: Core.Maybe Types.BatchListAttachedIndices
    -- ^ Lists indices attached to an object.
  , listIncomingTypedLinks :: Core.Maybe Types.BatchListIncomingTypedLinks
    -- ^ Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
  , listIndex :: Core.Maybe Types.BatchListIndex
    -- ^ Lists objects attached to the specified index.
  , listObjectAttributes :: Core.Maybe Types.BatchListObjectAttributes
    -- ^ Lists all attributes that are associated with an object.
  , listObjectChildren :: Core.Maybe Types.BatchListObjectChildren
    -- ^ Returns a paginated list of child objects that are associated with a given object.
  , listObjectParentPaths :: Core.Maybe Types.BatchListObjectParentPaths
    -- ^ Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
  , listObjectParents :: Core.Maybe Types.BatchListObjectParents
  , listObjectPolicies :: Core.Maybe Types.BatchListObjectPolicies
    -- ^ Returns policies attached to an object in pagination fashion.
  , listOutgoingTypedLinks :: Core.Maybe Types.BatchListOutgoingTypedLinks
    -- ^ Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
  , listPolicyAttachments :: Core.Maybe Types.BatchListPolicyAttachments
    -- ^ Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
  , lookupPolicy :: Core.Maybe Types.BatchLookupPolicy
    -- ^ Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchReadOperation' value with any optional fields omitted.
mkBatchReadOperation
    :: BatchReadOperation
mkBatchReadOperation
  = BatchReadOperation'{getLinkAttributes = Core.Nothing,
                        getObjectAttributes = Core.Nothing,
                        getObjectInformation = Core.Nothing,
                        listAttachedIndices = Core.Nothing,
                        listIncomingTypedLinks = Core.Nothing, listIndex = Core.Nothing,
                        listObjectAttributes = Core.Nothing,
                        listObjectChildren = Core.Nothing,
                        listObjectParentPaths = Core.Nothing,
                        listObjectParents = Core.Nothing,
                        listObjectPolicies = Core.Nothing,
                        listOutgoingTypedLinks = Core.Nothing,
                        listPolicyAttachments = Core.Nothing, lookupPolicy = Core.Nothing}

-- | Retrieves attributes that are associated with a typed link.
--
-- /Note:/ Consider using 'getLinkAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broGetLinkAttributes :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchGetLinkAttributes)
broGetLinkAttributes = Lens.field @"getLinkAttributes"
{-# INLINEABLE broGetLinkAttributes #-}
{-# DEPRECATED getLinkAttributes "Use generic-lens or generic-optics with 'getLinkAttributes' instead"  #-}

-- | Retrieves attributes within a facet that are associated with an object.
--
-- /Note:/ Consider using 'getObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broGetObjectAttributes :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchGetObjectAttributes)
broGetObjectAttributes = Lens.field @"getObjectAttributes"
{-# INLINEABLE broGetObjectAttributes #-}
{-# DEPRECATED getObjectAttributes "Use generic-lens or generic-optics with 'getObjectAttributes' instead"  #-}

-- | Retrieves metadata about an object.
--
-- /Note:/ Consider using 'getObjectInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broGetObjectInformation :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchGetObjectInformation)
broGetObjectInformation = Lens.field @"getObjectInformation"
{-# INLINEABLE broGetObjectInformation #-}
{-# DEPRECATED getObjectInformation "Use generic-lens or generic-optics with 'getObjectInformation' instead"  #-}

-- | Lists indices attached to an object.
--
-- /Note:/ Consider using 'listAttachedIndices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListAttachedIndices :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListAttachedIndices)
broListAttachedIndices = Lens.field @"listAttachedIndices"
{-# INLINEABLE broListAttachedIndices #-}
{-# DEPRECATED listAttachedIndices "Use generic-lens or generic-optics with 'listAttachedIndices' instead"  #-}

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'listIncomingTypedLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListIncomingTypedLinks :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListIncomingTypedLinks)
broListIncomingTypedLinks = Lens.field @"listIncomingTypedLinks"
{-# INLINEABLE broListIncomingTypedLinks #-}
{-# DEPRECATED listIncomingTypedLinks "Use generic-lens or generic-optics with 'listIncomingTypedLinks' instead"  #-}

-- | Lists objects attached to the specified index.
--
-- /Note:/ Consider using 'listIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListIndex :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListIndex)
broListIndex = Lens.field @"listIndex"
{-# INLINEABLE broListIndex #-}
{-# DEPRECATED listIndex "Use generic-lens or generic-optics with 'listIndex' instead"  #-}

-- | Lists all attributes that are associated with an object.
--
-- /Note:/ Consider using 'listObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectAttributes :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListObjectAttributes)
broListObjectAttributes = Lens.field @"listObjectAttributes"
{-# INLINEABLE broListObjectAttributes #-}
{-# DEPRECATED listObjectAttributes "Use generic-lens or generic-optics with 'listObjectAttributes' instead"  #-}

-- | Returns a paginated list of child objects that are associated with a given object.
--
-- /Note:/ Consider using 'listObjectChildren' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectChildren :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListObjectChildren)
broListObjectChildren = Lens.field @"listObjectChildren"
{-# INLINEABLE broListObjectChildren #-}
{-# DEPRECATED listObjectChildren "Use generic-lens or generic-optics with 'listObjectChildren' instead"  #-}

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
--
-- /Note:/ Consider using 'listObjectParentPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectParentPaths :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListObjectParentPaths)
broListObjectParentPaths = Lens.field @"listObjectParentPaths"
{-# INLINEABLE broListObjectParentPaths #-}
{-# DEPRECATED listObjectParentPaths "Use generic-lens or generic-optics with 'listObjectParentPaths' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'listObjectParents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectParents :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListObjectParents)
broListObjectParents = Lens.field @"listObjectParents"
{-# INLINEABLE broListObjectParents #-}
{-# DEPRECATED listObjectParents "Use generic-lens or generic-optics with 'listObjectParents' instead"  #-}

-- | Returns policies attached to an object in pagination fashion.
--
-- /Note:/ Consider using 'listObjectPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectPolicies :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListObjectPolicies)
broListObjectPolicies = Lens.field @"listObjectPolicies"
{-# INLINEABLE broListObjectPolicies #-}
{-# DEPRECATED listObjectPolicies "Use generic-lens or generic-optics with 'listObjectPolicies' instead"  #-}

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'listOutgoingTypedLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListOutgoingTypedLinks :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListOutgoingTypedLinks)
broListOutgoingTypedLinks = Lens.field @"listOutgoingTypedLinks"
{-# INLINEABLE broListOutgoingTypedLinks #-}
{-# DEPRECATED listOutgoingTypedLinks "Use generic-lens or generic-optics with 'listOutgoingTypedLinks' instead"  #-}

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- /Note:/ Consider using 'listPolicyAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListPolicyAttachments :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchListPolicyAttachments)
broListPolicyAttachments = Lens.field @"listPolicyAttachments"
{-# INLINEABLE broListPolicyAttachments #-}
{-# DEPRECATED listPolicyAttachments "Use generic-lens or generic-optics with 'listPolicyAttachments' instead"  #-}

-- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /Note:/ Consider using 'lookupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broLookupPolicy :: Lens.Lens' BatchReadOperation (Core.Maybe Types.BatchLookupPolicy)
broLookupPolicy = Lens.field @"lookupPolicy"
{-# INLINEABLE broLookupPolicy #-}
{-# DEPRECATED lookupPolicy "Use generic-lens or generic-optics with 'lookupPolicy' instead"  #-}

instance Core.FromJSON BatchReadOperation where
        toJSON BatchReadOperation{..}
          = Core.object
              (Core.catMaybes
                 [("GetLinkAttributes" Core..=) Core.<$> getLinkAttributes,
                  ("GetObjectAttributes" Core..=) Core.<$> getObjectAttributes,
                  ("GetObjectInformation" Core..=) Core.<$> getObjectInformation,
                  ("ListAttachedIndices" Core..=) Core.<$> listAttachedIndices,
                  ("ListIncomingTypedLinks" Core..=) Core.<$> listIncomingTypedLinks,
                  ("ListIndex" Core..=) Core.<$> listIndex,
                  ("ListObjectAttributes" Core..=) Core.<$> listObjectAttributes,
                  ("ListObjectChildren" Core..=) Core.<$> listObjectChildren,
                  ("ListObjectParentPaths" Core..=) Core.<$> listObjectParentPaths,
                  ("ListObjectParents" Core..=) Core.<$> listObjectParents,
                  ("ListObjectPolicies" Core..=) Core.<$> listObjectPolicies,
                  ("ListOutgoingTypedLinks" Core..=) Core.<$> listOutgoingTypedLinks,
                  ("ListPolicyAttachments" Core..=) Core.<$> listPolicyAttachments,
                  ("LookupPolicy" Core..=) Core.<$> lookupPolicy])
