{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadOperation
  ( BatchReadOperation (..),

    -- * Smart constructor
    mkBatchReadOperation,

    -- * Lenses
    broListIndex,
    broGetObjectInformation,
    broListAttachedIndices,
    broLookupPolicy,
    broListObjectParentPaths,
    broListObjectAttributes,
    broListIncomingTypedLinks,
    broGetLinkAttributes,
    broGetObjectAttributes,
    broListObjectChildren,
    broListObjectParents,
    broListPolicyAttachments,
    broListOutgoingTypedLinks,
    broListObjectPolicies,
  )
where

import Network.AWS.CloudDirectory.Types.BatchGetLinkAttributes
import Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes
import Network.AWS.CloudDirectory.Types.BatchGetObjectInformation
import Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
import Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinks
import Network.AWS.CloudDirectory.Types.BatchListIndex
import Network.AWS.CloudDirectory.Types.BatchListObjectAttributes
import Network.AWS.CloudDirectory.Types.BatchListObjectChildren
import Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths
import Network.AWS.CloudDirectory.Types.BatchListObjectParents
import Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
import Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
import Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
import Network.AWS.CloudDirectory.Types.BatchLookupPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @BatchRead@ operation.
--
-- /See:/ 'mkBatchReadOperation' smart constructor.
data BatchReadOperation = BatchReadOperation'
  { listIndex ::
      Lude.Maybe BatchListIndex,
    getObjectInformation ::
      Lude.Maybe BatchGetObjectInformation,
    listAttachedIndices ::
      Lude.Maybe BatchListAttachedIndices,
    lookupPolicy :: Lude.Maybe BatchLookupPolicy,
    listObjectParentPaths ::
      Lude.Maybe BatchListObjectParentPaths,
    listObjectAttributes ::
      Lude.Maybe BatchListObjectAttributes,
    listIncomingTypedLinks ::
      Lude.Maybe BatchListIncomingTypedLinks,
    getLinkAttributes ::
      Lude.Maybe BatchGetLinkAttributes,
    getObjectAttributes ::
      Lude.Maybe BatchGetObjectAttributes,
    listObjectChildren ::
      Lude.Maybe BatchListObjectChildren,
    listObjectParents ::
      Lude.Maybe BatchListObjectParents,
    listPolicyAttachments ::
      Lude.Maybe BatchListPolicyAttachments,
    listOutgoingTypedLinks ::
      Lude.Maybe BatchListOutgoingTypedLinks,
    listObjectPolicies ::
      Lude.Maybe BatchListObjectPolicies
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchReadOperation' with the minimum fields required to make a request.
--
-- * 'getLinkAttributes' - Retrieves attributes that are associated with a typed link.
-- * 'getObjectAttributes' - Retrieves attributes within a facet that are associated with an object.
-- * 'getObjectInformation' - Retrieves metadata about an object.
-- * 'listAttachedIndices' - Lists indices attached to an object.
-- * 'listIncomingTypedLinks' - Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'listIndex' - Lists objects attached to the specified index.
-- * 'listObjectAttributes' - Lists all attributes that are associated with an object.
-- * 'listObjectChildren' - Returns a paginated list of child objects that are associated with a given object.
-- * 'listObjectParentPaths' - Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
-- * 'listObjectParents' - Undocumented field.
-- * 'listObjectPolicies' - Returns policies attached to an object in pagination fashion.
-- * 'listOutgoingTypedLinks' - Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'listPolicyAttachments' - Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
-- * 'lookupPolicy' - Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
mkBatchReadOperation ::
  BatchReadOperation
mkBatchReadOperation =
  BatchReadOperation'
    { listIndex = Lude.Nothing,
      getObjectInformation = Lude.Nothing,
      listAttachedIndices = Lude.Nothing,
      lookupPolicy = Lude.Nothing,
      listObjectParentPaths = Lude.Nothing,
      listObjectAttributes = Lude.Nothing,
      listIncomingTypedLinks = Lude.Nothing,
      getLinkAttributes = Lude.Nothing,
      getObjectAttributes = Lude.Nothing,
      listObjectChildren = Lude.Nothing,
      listObjectParents = Lude.Nothing,
      listPolicyAttachments = Lude.Nothing,
      listOutgoingTypedLinks = Lude.Nothing,
      listObjectPolicies = Lude.Nothing
    }

-- | Lists objects attached to the specified index.
--
-- /Note:/ Consider using 'listIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListIndex :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListIndex)
broListIndex = Lens.lens (listIndex :: BatchReadOperation -> Lude.Maybe BatchListIndex) (\s a -> s {listIndex = a} :: BatchReadOperation)
{-# DEPRECATED broListIndex "Use generic-lens or generic-optics with 'listIndex' instead." #-}

-- | Retrieves metadata about an object.
--
-- /Note:/ Consider using 'getObjectInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broGetObjectInformation :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchGetObjectInformation)
broGetObjectInformation = Lens.lens (getObjectInformation :: BatchReadOperation -> Lude.Maybe BatchGetObjectInformation) (\s a -> s {getObjectInformation = a} :: BatchReadOperation)
{-# DEPRECATED broGetObjectInformation "Use generic-lens or generic-optics with 'getObjectInformation' instead." #-}

-- | Lists indices attached to an object.
--
-- /Note:/ Consider using 'listAttachedIndices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListAttachedIndices :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListAttachedIndices)
broListAttachedIndices = Lens.lens (listAttachedIndices :: BatchReadOperation -> Lude.Maybe BatchListAttachedIndices) (\s a -> s {listAttachedIndices = a} :: BatchReadOperation)
{-# DEPRECATED broListAttachedIndices "Use generic-lens or generic-optics with 'listAttachedIndices' instead." #-}

-- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /Note:/ Consider using 'lookupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broLookupPolicy :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchLookupPolicy)
broLookupPolicy = Lens.lens (lookupPolicy :: BatchReadOperation -> Lude.Maybe BatchLookupPolicy) (\s a -> s {lookupPolicy = a} :: BatchReadOperation)
{-# DEPRECATED broLookupPolicy "Use generic-lens or generic-optics with 'lookupPolicy' instead." #-}

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
--
-- /Note:/ Consider using 'listObjectParentPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectParentPaths :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListObjectParentPaths)
broListObjectParentPaths = Lens.lens (listObjectParentPaths :: BatchReadOperation -> Lude.Maybe BatchListObjectParentPaths) (\s a -> s {listObjectParentPaths = a} :: BatchReadOperation)
{-# DEPRECATED broListObjectParentPaths "Use generic-lens or generic-optics with 'listObjectParentPaths' instead." #-}

-- | Lists all attributes that are associated with an object.
--
-- /Note:/ Consider using 'listObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectAttributes :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListObjectAttributes)
broListObjectAttributes = Lens.lens (listObjectAttributes :: BatchReadOperation -> Lude.Maybe BatchListObjectAttributes) (\s a -> s {listObjectAttributes = a} :: BatchReadOperation)
{-# DEPRECATED broListObjectAttributes "Use generic-lens or generic-optics with 'listObjectAttributes' instead." #-}

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'listIncomingTypedLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListIncomingTypedLinks :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListIncomingTypedLinks)
broListIncomingTypedLinks = Lens.lens (listIncomingTypedLinks :: BatchReadOperation -> Lude.Maybe BatchListIncomingTypedLinks) (\s a -> s {listIncomingTypedLinks = a} :: BatchReadOperation)
{-# DEPRECATED broListIncomingTypedLinks "Use generic-lens or generic-optics with 'listIncomingTypedLinks' instead." #-}

-- | Retrieves attributes that are associated with a typed link.
--
-- /Note:/ Consider using 'getLinkAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broGetLinkAttributes :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchGetLinkAttributes)
broGetLinkAttributes = Lens.lens (getLinkAttributes :: BatchReadOperation -> Lude.Maybe BatchGetLinkAttributes) (\s a -> s {getLinkAttributes = a} :: BatchReadOperation)
{-# DEPRECATED broGetLinkAttributes "Use generic-lens or generic-optics with 'getLinkAttributes' instead." #-}

-- | Retrieves attributes within a facet that are associated with an object.
--
-- /Note:/ Consider using 'getObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broGetObjectAttributes :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchGetObjectAttributes)
broGetObjectAttributes = Lens.lens (getObjectAttributes :: BatchReadOperation -> Lude.Maybe BatchGetObjectAttributes) (\s a -> s {getObjectAttributes = a} :: BatchReadOperation)
{-# DEPRECATED broGetObjectAttributes "Use generic-lens or generic-optics with 'getObjectAttributes' instead." #-}

-- | Returns a paginated list of child objects that are associated with a given object.
--
-- /Note:/ Consider using 'listObjectChildren' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectChildren :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListObjectChildren)
broListObjectChildren = Lens.lens (listObjectChildren :: BatchReadOperation -> Lude.Maybe BatchListObjectChildren) (\s a -> s {listObjectChildren = a} :: BatchReadOperation)
{-# DEPRECATED broListObjectChildren "Use generic-lens or generic-optics with 'listObjectChildren' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'listObjectParents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectParents :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListObjectParents)
broListObjectParents = Lens.lens (listObjectParents :: BatchReadOperation -> Lude.Maybe BatchListObjectParents) (\s a -> s {listObjectParents = a} :: BatchReadOperation)
{-# DEPRECATED broListObjectParents "Use generic-lens or generic-optics with 'listObjectParents' instead." #-}

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- /Note:/ Consider using 'listPolicyAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListPolicyAttachments :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListPolicyAttachments)
broListPolicyAttachments = Lens.lens (listPolicyAttachments :: BatchReadOperation -> Lude.Maybe BatchListPolicyAttachments) (\s a -> s {listPolicyAttachments = a} :: BatchReadOperation)
{-# DEPRECATED broListPolicyAttachments "Use generic-lens or generic-optics with 'listPolicyAttachments' instead." #-}

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'listOutgoingTypedLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListOutgoingTypedLinks :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListOutgoingTypedLinks)
broListOutgoingTypedLinks = Lens.lens (listOutgoingTypedLinks :: BatchReadOperation -> Lude.Maybe BatchListOutgoingTypedLinks) (\s a -> s {listOutgoingTypedLinks = a} :: BatchReadOperation)
{-# DEPRECATED broListOutgoingTypedLinks "Use generic-lens or generic-optics with 'listOutgoingTypedLinks' instead." #-}

-- | Returns policies attached to an object in pagination fashion.
--
-- /Note:/ Consider using 'listObjectPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broListObjectPolicies :: Lens.Lens' BatchReadOperation (Lude.Maybe BatchListObjectPolicies)
broListObjectPolicies = Lens.lens (listObjectPolicies :: BatchReadOperation -> Lude.Maybe BatchListObjectPolicies) (\s a -> s {listObjectPolicies = a} :: BatchReadOperation)
{-# DEPRECATED broListObjectPolicies "Use generic-lens or generic-optics with 'listObjectPolicies' instead." #-}

instance Lude.ToJSON BatchReadOperation where
  toJSON BatchReadOperation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ListIndex" Lude..=) Lude.<$> listIndex,
            ("GetObjectInformation" Lude..=) Lude.<$> getObjectInformation,
            ("ListAttachedIndices" Lude..=) Lude.<$> listAttachedIndices,
            ("LookupPolicy" Lude..=) Lude.<$> lookupPolicy,
            ("ListObjectParentPaths" Lude..=) Lude.<$> listObjectParentPaths,
            ("ListObjectAttributes" Lude..=) Lude.<$> listObjectAttributes,
            ("ListIncomingTypedLinks" Lude..=) Lude.<$> listIncomingTypedLinks,
            ("GetLinkAttributes" Lude..=) Lude.<$> getLinkAttributes,
            ("GetObjectAttributes" Lude..=) Lude.<$> getObjectAttributes,
            ("ListObjectChildren" Lude..=) Lude.<$> listObjectChildren,
            ("ListObjectParents" Lude..=) Lude.<$> listObjectParents,
            ("ListPolicyAttachments" Lude..=) Lude.<$> listPolicyAttachments,
            ("ListOutgoingTypedLinks" Lude..=) Lude.<$> listOutgoingTypedLinks,
            ("ListObjectPolicies" Lude..=) Lude.<$> listObjectPolicies
          ]
      )
