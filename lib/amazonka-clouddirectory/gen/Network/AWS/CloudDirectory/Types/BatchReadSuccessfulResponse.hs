-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
  ( BatchReadSuccessfulResponse (..),

    -- * Smart constructor
    mkBatchReadSuccessfulResponse,

    -- * Lenses
    brsListIndex,
    brsGetObjectInformation,
    brsListAttachedIndices,
    brsLookupPolicy,
    brsListObjectParentPaths,
    brsListObjectAttributes,
    brsListIncomingTypedLinks,
    brsGetLinkAttributes,
    brsGetObjectAttributes,
    brsListObjectChildren,
    brsListObjectParents,
    brsListPolicyAttachments,
    brsListOutgoingTypedLinks,
    brsListObjectPolicies,
  )
where

import Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
import Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
import Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
import Network.AWS.CloudDirectory.Types.BatchListIndexResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
import Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
import Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
import Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @BatchRead@ success response operation.
--
-- /See:/ 'mkBatchReadSuccessfulResponse' smart constructor.
data BatchReadSuccessfulResponse = BatchReadSuccessfulResponse'
  { listIndex ::
      Lude.Maybe BatchListIndexResponse,
    getObjectInformation ::
      Lude.Maybe
        BatchGetObjectInformationResponse,
    listAttachedIndices ::
      Lude.Maybe
        BatchListAttachedIndicesResponse,
    lookupPolicy ::
      Lude.Maybe
        BatchLookupPolicyResponse,
    listObjectParentPaths ::
      Lude.Maybe
        BatchListObjectParentPathsResponse,
    listObjectAttributes ::
      Lude.Maybe
        BatchListObjectAttributesResponse,
    listIncomingTypedLinks ::
      Lude.Maybe
        BatchListIncomingTypedLinksResponse,
    getLinkAttributes ::
      Lude.Maybe
        BatchGetLinkAttributesResponse,
    getObjectAttributes ::
      Lude.Maybe
        BatchGetObjectAttributesResponse,
    listObjectChildren ::
      Lude.Maybe
        BatchListObjectChildrenResponse,
    listObjectParents ::
      Lude.Maybe
        BatchListObjectParentsResponse,
    listPolicyAttachments ::
      Lude.Maybe
        BatchListPolicyAttachmentsResponse,
    listOutgoingTypedLinks ::
      Lude.Maybe
        BatchListOutgoingTypedLinksResponse,
    listObjectPolicies ::
      Lude.Maybe
        BatchListObjectPoliciesResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchReadSuccessfulResponse' with the minimum fields required to make a request.
--
-- * 'getLinkAttributes' - The list of attributes to retrieve from the typed link.
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
mkBatchReadSuccessfulResponse ::
  BatchReadSuccessfulResponse
mkBatchReadSuccessfulResponse =
  BatchReadSuccessfulResponse'
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
brsListIndex :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListIndexResponse)
brsListIndex = Lens.lens (listIndex :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListIndexResponse) (\s a -> s {listIndex = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListIndex "Use generic-lens or generic-optics with 'listIndex' instead." #-}

-- | Retrieves metadata about an object.
--
-- /Note:/ Consider using 'getObjectInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsGetObjectInformation :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchGetObjectInformationResponse)
brsGetObjectInformation = Lens.lens (getObjectInformation :: BatchReadSuccessfulResponse -> Lude.Maybe BatchGetObjectInformationResponse) (\s a -> s {getObjectInformation = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsGetObjectInformation "Use generic-lens or generic-optics with 'getObjectInformation' instead." #-}

-- | Lists indices attached to an object.
--
-- /Note:/ Consider using 'listAttachedIndices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListAttachedIndices :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListAttachedIndicesResponse)
brsListAttachedIndices = Lens.lens (listAttachedIndices :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListAttachedIndicesResponse) (\s a -> s {listAttachedIndices = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListAttachedIndices "Use generic-lens or generic-optics with 'listAttachedIndices' instead." #-}

-- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /Note:/ Consider using 'lookupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsLookupPolicy :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchLookupPolicyResponse)
brsLookupPolicy = Lens.lens (lookupPolicy :: BatchReadSuccessfulResponse -> Lude.Maybe BatchLookupPolicyResponse) (\s a -> s {lookupPolicy = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsLookupPolicy "Use generic-lens or generic-optics with 'lookupPolicy' instead." #-}

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
--
-- /Note:/ Consider using 'listObjectParentPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListObjectParentPaths :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListObjectParentPathsResponse)
brsListObjectParentPaths = Lens.lens (listObjectParentPaths :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListObjectParentPathsResponse) (\s a -> s {listObjectParentPaths = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListObjectParentPaths "Use generic-lens or generic-optics with 'listObjectParentPaths' instead." #-}

-- | Lists all attributes that are associated with an object.
--
-- /Note:/ Consider using 'listObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListObjectAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListObjectAttributesResponse)
brsListObjectAttributes = Lens.lens (listObjectAttributes :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListObjectAttributesResponse) (\s a -> s {listObjectAttributes = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListObjectAttributes "Use generic-lens or generic-optics with 'listObjectAttributes' instead." #-}

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'listIncomingTypedLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListIncomingTypedLinks :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListIncomingTypedLinksResponse)
brsListIncomingTypedLinks = Lens.lens (listIncomingTypedLinks :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListIncomingTypedLinksResponse) (\s a -> s {listIncomingTypedLinks = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListIncomingTypedLinks "Use generic-lens or generic-optics with 'listIncomingTypedLinks' instead." #-}

-- | The list of attributes to retrieve from the typed link.
--
-- /Note:/ Consider using 'getLinkAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsGetLinkAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchGetLinkAttributesResponse)
brsGetLinkAttributes = Lens.lens (getLinkAttributes :: BatchReadSuccessfulResponse -> Lude.Maybe BatchGetLinkAttributesResponse) (\s a -> s {getLinkAttributes = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsGetLinkAttributes "Use generic-lens or generic-optics with 'getLinkAttributes' instead." #-}

-- | Retrieves attributes within a facet that are associated with an object.
--
-- /Note:/ Consider using 'getObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsGetObjectAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchGetObjectAttributesResponse)
brsGetObjectAttributes = Lens.lens (getObjectAttributes :: BatchReadSuccessfulResponse -> Lude.Maybe BatchGetObjectAttributesResponse) (\s a -> s {getObjectAttributes = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsGetObjectAttributes "Use generic-lens or generic-optics with 'getObjectAttributes' instead." #-}

-- | Returns a paginated list of child objects that are associated with a given object.
--
-- /Note:/ Consider using 'listObjectChildren' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListObjectChildren :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListObjectChildrenResponse)
brsListObjectChildren = Lens.lens (listObjectChildren :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListObjectChildrenResponse) (\s a -> s {listObjectChildren = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListObjectChildren "Use generic-lens or generic-optics with 'listObjectChildren' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'listObjectParents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListObjectParents :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListObjectParentsResponse)
brsListObjectParents = Lens.lens (listObjectParents :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListObjectParentsResponse) (\s a -> s {listObjectParents = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListObjectParents "Use generic-lens or generic-optics with 'listObjectParents' instead." #-}

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- /Note:/ Consider using 'listPolicyAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListPolicyAttachments :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListPolicyAttachmentsResponse)
brsListPolicyAttachments = Lens.lens (listPolicyAttachments :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListPolicyAttachmentsResponse) (\s a -> s {listPolicyAttachments = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListPolicyAttachments "Use generic-lens or generic-optics with 'listPolicyAttachments' instead." #-}

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'listOutgoingTypedLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListOutgoingTypedLinks :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListOutgoingTypedLinksResponse)
brsListOutgoingTypedLinks = Lens.lens (listOutgoingTypedLinks :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListOutgoingTypedLinksResponse) (\s a -> s {listOutgoingTypedLinks = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListOutgoingTypedLinks "Use generic-lens or generic-optics with 'listOutgoingTypedLinks' instead." #-}

-- | Returns policies attached to an object in pagination fashion.
--
-- /Note:/ Consider using 'listObjectPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsListObjectPolicies :: Lens.Lens' BatchReadSuccessfulResponse (Lude.Maybe BatchListObjectPoliciesResponse)
brsListObjectPolicies = Lens.lens (listObjectPolicies :: BatchReadSuccessfulResponse -> Lude.Maybe BatchListObjectPoliciesResponse) (\s a -> s {listObjectPolicies = a} :: BatchReadSuccessfulResponse)
{-# DEPRECATED brsListObjectPolicies "Use generic-lens or generic-optics with 'listObjectPolicies' instead." #-}

instance Lude.FromJSON BatchReadSuccessfulResponse where
  parseJSON =
    Lude.withObject
      "BatchReadSuccessfulResponse"
      ( \x ->
          BatchReadSuccessfulResponse'
            Lude.<$> (x Lude..:? "ListIndex")
            Lude.<*> (x Lude..:? "GetObjectInformation")
            Lude.<*> (x Lude..:? "ListAttachedIndices")
            Lude.<*> (x Lude..:? "LookupPolicy")
            Lude.<*> (x Lude..:? "ListObjectParentPaths")
            Lude.<*> (x Lude..:? "ListObjectAttributes")
            Lude.<*> (x Lude..:? "ListIncomingTypedLinks")
            Lude.<*> (x Lude..:? "GetLinkAttributes")
            Lude.<*> (x Lude..:? "GetObjectAttributes")
            Lude.<*> (x Lude..:? "ListObjectChildren")
            Lude.<*> (x Lude..:? "ListObjectParents")
            Lude.<*> (x Lude..:? "ListPolicyAttachments")
            Lude.<*> (x Lude..:? "ListOutgoingTypedLinks")
            Lude.<*> (x Lude..:? "ListObjectPolicies")
      )
