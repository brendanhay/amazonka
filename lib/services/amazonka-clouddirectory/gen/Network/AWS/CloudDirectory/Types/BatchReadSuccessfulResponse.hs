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
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse where

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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a @BatchRead@ success response operation.
--
-- /See:/ 'newBatchReadSuccessfulResponse' smart constructor.
data BatchReadSuccessfulResponse = BatchReadSuccessfulResponse'
  { -- | Lists objects attached to the specified index.
    listIndex :: Prelude.Maybe BatchListIndexResponse,
    -- | Retrieves metadata about an object.
    getObjectInformation :: Prelude.Maybe BatchGetObjectInformationResponse,
    -- | Lists indices attached to an object.
    listAttachedIndices :: Prelude.Maybe BatchListAttachedIndicesResponse,
    -- | Lists all policies from the root of the Directory to the object
    -- specified. If there are no policies present, an empty list is returned.
    -- If policies are present, and if some objects don\'t have the policies
    -- attached, it returns the @ObjectIdentifier@ for such objects. If
    -- policies are present, it returns @ObjectIdentifier@, @policyId@, and
    -- @policyType@. Paths that don\'t lead to the root from the target object
    -- are ignored. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
    lookupPolicy :: Prelude.Maybe BatchLookupPolicyResponse,
    -- | Retrieves all available parent paths for any object type such as node,
    -- leaf node, policy node, and index node objects. For more information
    -- about objects, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure>.
    listObjectParentPaths :: Prelude.Maybe BatchListObjectParentPathsResponse,
    -- | Lists all attributes that are associated with an object.
    listObjectAttributes :: Prelude.Maybe BatchListObjectAttributesResponse,
    -- | Returns a paginated list of all the incoming TypedLinkSpecifier
    -- information for an object. It also supports filtering by typed link
    -- facet and identity attributes. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    listIncomingTypedLinks :: Prelude.Maybe BatchListIncomingTypedLinksResponse,
    -- | The list of attributes to retrieve from the typed link.
    getLinkAttributes :: Prelude.Maybe BatchGetLinkAttributesResponse,
    -- | Retrieves attributes within a facet that are associated with an object.
    getObjectAttributes :: Prelude.Maybe BatchGetObjectAttributesResponse,
    -- | Returns a paginated list of child objects that are associated with a
    -- given object.
    listObjectChildren :: Prelude.Maybe BatchListObjectChildrenResponse,
    -- | The list of parent objects to retrieve.
    listObjectParents :: Prelude.Maybe BatchListObjectParentsResponse,
    -- | Returns all of the @ObjectIdentifiers@ to which a given policy is
    -- attached.
    listPolicyAttachments :: Prelude.Maybe BatchListPolicyAttachmentsResponse,
    -- | Returns a paginated list of all the outgoing TypedLinkSpecifier
    -- information for an object. It also supports filtering by typed link
    -- facet and identity attributes. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    listOutgoingTypedLinks :: Prelude.Maybe BatchListOutgoingTypedLinksResponse,
    -- | Returns policies attached to an object in pagination fashion.
    listObjectPolicies :: Prelude.Maybe BatchListObjectPoliciesResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchReadSuccessfulResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listIndex', 'batchReadSuccessfulResponse_listIndex' - Lists objects attached to the specified index.
--
-- 'getObjectInformation', 'batchReadSuccessfulResponse_getObjectInformation' - Retrieves metadata about an object.
--
-- 'listAttachedIndices', 'batchReadSuccessfulResponse_listAttachedIndices' - Lists indices attached to an object.
--
-- 'lookupPolicy', 'batchReadSuccessfulResponse_lookupPolicy' - Lists all policies from the root of the Directory to the object
-- specified. If there are no policies present, an empty list is returned.
-- If policies are present, and if some objects don\'t have the policies
-- attached, it returns the @ObjectIdentifier@ for such objects. If
-- policies are present, it returns @ObjectIdentifier@, @policyId@, and
-- @policyType@. Paths that don\'t lead to the root from the target object
-- are ignored. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
--
-- 'listObjectParentPaths', 'batchReadSuccessfulResponse_listObjectParentPaths' - Retrieves all available parent paths for any object type such as node,
-- leaf node, policy node, and index node objects. For more information
-- about objects, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure>.
--
-- 'listObjectAttributes', 'batchReadSuccessfulResponse_listObjectAttributes' - Lists all attributes that are associated with an object.
--
-- 'listIncomingTypedLinks', 'batchReadSuccessfulResponse_listIncomingTypedLinks' - Returns a paginated list of all the incoming TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'getLinkAttributes', 'batchReadSuccessfulResponse_getLinkAttributes' - The list of attributes to retrieve from the typed link.
--
-- 'getObjectAttributes', 'batchReadSuccessfulResponse_getObjectAttributes' - Retrieves attributes within a facet that are associated with an object.
--
-- 'listObjectChildren', 'batchReadSuccessfulResponse_listObjectChildren' - Returns a paginated list of child objects that are associated with a
-- given object.
--
-- 'listObjectParents', 'batchReadSuccessfulResponse_listObjectParents' - The list of parent objects to retrieve.
--
-- 'listPolicyAttachments', 'batchReadSuccessfulResponse_listPolicyAttachments' - Returns all of the @ObjectIdentifiers@ to which a given policy is
-- attached.
--
-- 'listOutgoingTypedLinks', 'batchReadSuccessfulResponse_listOutgoingTypedLinks' - Returns a paginated list of all the outgoing TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'listObjectPolicies', 'batchReadSuccessfulResponse_listObjectPolicies' - Returns policies attached to an object in pagination fashion.
newBatchReadSuccessfulResponse ::
  BatchReadSuccessfulResponse
newBatchReadSuccessfulResponse =
  BatchReadSuccessfulResponse'
    { listIndex =
        Prelude.Nothing,
      getObjectInformation = Prelude.Nothing,
      listAttachedIndices = Prelude.Nothing,
      lookupPolicy = Prelude.Nothing,
      listObjectParentPaths = Prelude.Nothing,
      listObjectAttributes = Prelude.Nothing,
      listIncomingTypedLinks = Prelude.Nothing,
      getLinkAttributes = Prelude.Nothing,
      getObjectAttributes = Prelude.Nothing,
      listObjectChildren = Prelude.Nothing,
      listObjectParents = Prelude.Nothing,
      listPolicyAttachments = Prelude.Nothing,
      listOutgoingTypedLinks = Prelude.Nothing,
      listObjectPolicies = Prelude.Nothing
    }

-- | Lists objects attached to the specified index.
batchReadSuccessfulResponse_listIndex :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListIndexResponse)
batchReadSuccessfulResponse_listIndex = Lens.lens (\BatchReadSuccessfulResponse' {listIndex} -> listIndex) (\s@BatchReadSuccessfulResponse' {} a -> s {listIndex = a} :: BatchReadSuccessfulResponse)

-- | Retrieves metadata about an object.
batchReadSuccessfulResponse_getObjectInformation :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchGetObjectInformationResponse)
batchReadSuccessfulResponse_getObjectInformation = Lens.lens (\BatchReadSuccessfulResponse' {getObjectInformation} -> getObjectInformation) (\s@BatchReadSuccessfulResponse' {} a -> s {getObjectInformation = a} :: BatchReadSuccessfulResponse)

-- | Lists indices attached to an object.
batchReadSuccessfulResponse_listAttachedIndices :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListAttachedIndicesResponse)
batchReadSuccessfulResponse_listAttachedIndices = Lens.lens (\BatchReadSuccessfulResponse' {listAttachedIndices} -> listAttachedIndices) (\s@BatchReadSuccessfulResponse' {} a -> s {listAttachedIndices = a} :: BatchReadSuccessfulResponse)

-- | Lists all policies from the root of the Directory to the object
-- specified. If there are no policies present, an empty list is returned.
-- If policies are present, and if some objects don\'t have the policies
-- attached, it returns the @ObjectIdentifier@ for such objects. If
-- policies are present, it returns @ObjectIdentifier@, @policyId@, and
-- @policyType@. Paths that don\'t lead to the root from the target object
-- are ignored. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
batchReadSuccessfulResponse_lookupPolicy :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchLookupPolicyResponse)
batchReadSuccessfulResponse_lookupPolicy = Lens.lens (\BatchReadSuccessfulResponse' {lookupPolicy} -> lookupPolicy) (\s@BatchReadSuccessfulResponse' {} a -> s {lookupPolicy = a} :: BatchReadSuccessfulResponse)

-- | Retrieves all available parent paths for any object type such as node,
-- leaf node, policy node, and index node objects. For more information
-- about objects, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure>.
batchReadSuccessfulResponse_listObjectParentPaths :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListObjectParentPathsResponse)
batchReadSuccessfulResponse_listObjectParentPaths = Lens.lens (\BatchReadSuccessfulResponse' {listObjectParentPaths} -> listObjectParentPaths) (\s@BatchReadSuccessfulResponse' {} a -> s {listObjectParentPaths = a} :: BatchReadSuccessfulResponse)

-- | Lists all attributes that are associated with an object.
batchReadSuccessfulResponse_listObjectAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListObjectAttributesResponse)
batchReadSuccessfulResponse_listObjectAttributes = Lens.lens (\BatchReadSuccessfulResponse' {listObjectAttributes} -> listObjectAttributes) (\s@BatchReadSuccessfulResponse' {} a -> s {listObjectAttributes = a} :: BatchReadSuccessfulResponse)

-- | Returns a paginated list of all the incoming TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchReadSuccessfulResponse_listIncomingTypedLinks :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListIncomingTypedLinksResponse)
batchReadSuccessfulResponse_listIncomingTypedLinks = Lens.lens (\BatchReadSuccessfulResponse' {listIncomingTypedLinks} -> listIncomingTypedLinks) (\s@BatchReadSuccessfulResponse' {} a -> s {listIncomingTypedLinks = a} :: BatchReadSuccessfulResponse)

-- | The list of attributes to retrieve from the typed link.
batchReadSuccessfulResponse_getLinkAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchGetLinkAttributesResponse)
batchReadSuccessfulResponse_getLinkAttributes = Lens.lens (\BatchReadSuccessfulResponse' {getLinkAttributes} -> getLinkAttributes) (\s@BatchReadSuccessfulResponse' {} a -> s {getLinkAttributes = a} :: BatchReadSuccessfulResponse)

-- | Retrieves attributes within a facet that are associated with an object.
batchReadSuccessfulResponse_getObjectAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchGetObjectAttributesResponse)
batchReadSuccessfulResponse_getObjectAttributes = Lens.lens (\BatchReadSuccessfulResponse' {getObjectAttributes} -> getObjectAttributes) (\s@BatchReadSuccessfulResponse' {} a -> s {getObjectAttributes = a} :: BatchReadSuccessfulResponse)

-- | Returns a paginated list of child objects that are associated with a
-- given object.
batchReadSuccessfulResponse_listObjectChildren :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListObjectChildrenResponse)
batchReadSuccessfulResponse_listObjectChildren = Lens.lens (\BatchReadSuccessfulResponse' {listObjectChildren} -> listObjectChildren) (\s@BatchReadSuccessfulResponse' {} a -> s {listObjectChildren = a} :: BatchReadSuccessfulResponse)

-- | The list of parent objects to retrieve.
batchReadSuccessfulResponse_listObjectParents :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListObjectParentsResponse)
batchReadSuccessfulResponse_listObjectParents = Lens.lens (\BatchReadSuccessfulResponse' {listObjectParents} -> listObjectParents) (\s@BatchReadSuccessfulResponse' {} a -> s {listObjectParents = a} :: BatchReadSuccessfulResponse)

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is
-- attached.
batchReadSuccessfulResponse_listPolicyAttachments :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListPolicyAttachmentsResponse)
batchReadSuccessfulResponse_listPolicyAttachments = Lens.lens (\BatchReadSuccessfulResponse' {listPolicyAttachments} -> listPolicyAttachments) (\s@BatchReadSuccessfulResponse' {} a -> s {listPolicyAttachments = a} :: BatchReadSuccessfulResponse)

-- | Returns a paginated list of all the outgoing TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchReadSuccessfulResponse_listOutgoingTypedLinks :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListOutgoingTypedLinksResponse)
batchReadSuccessfulResponse_listOutgoingTypedLinks = Lens.lens (\BatchReadSuccessfulResponse' {listOutgoingTypedLinks} -> listOutgoingTypedLinks) (\s@BatchReadSuccessfulResponse' {} a -> s {listOutgoingTypedLinks = a} :: BatchReadSuccessfulResponse)

-- | Returns policies attached to an object in pagination fashion.
batchReadSuccessfulResponse_listObjectPolicies :: Lens.Lens' BatchReadSuccessfulResponse (Prelude.Maybe BatchListObjectPoliciesResponse)
batchReadSuccessfulResponse_listObjectPolicies = Lens.lens (\BatchReadSuccessfulResponse' {listObjectPolicies} -> listObjectPolicies) (\s@BatchReadSuccessfulResponse' {} a -> s {listObjectPolicies = a} :: BatchReadSuccessfulResponse)

instance Core.FromJSON BatchReadSuccessfulResponse where
  parseJSON =
    Core.withObject
      "BatchReadSuccessfulResponse"
      ( \x ->
          BatchReadSuccessfulResponse'
            Prelude.<$> (x Core..:? "ListIndex")
            Prelude.<*> (x Core..:? "GetObjectInformation")
            Prelude.<*> (x Core..:? "ListAttachedIndices")
            Prelude.<*> (x Core..:? "LookupPolicy")
            Prelude.<*> (x Core..:? "ListObjectParentPaths")
            Prelude.<*> (x Core..:? "ListObjectAttributes")
            Prelude.<*> (x Core..:? "ListIncomingTypedLinks")
            Prelude.<*> (x Core..:? "GetLinkAttributes")
            Prelude.<*> (x Core..:? "GetObjectAttributes")
            Prelude.<*> (x Core..:? "ListObjectChildren")
            Prelude.<*> (x Core..:? "ListObjectParents")
            Prelude.<*> (x Core..:? "ListPolicyAttachments")
            Prelude.<*> (x Core..:? "ListOutgoingTypedLinks")
            Prelude.<*> (x Core..:? "ListObjectPolicies")
      )

instance Prelude.Hashable BatchReadSuccessfulResponse

instance Prelude.NFData BatchReadSuccessfulResponse
