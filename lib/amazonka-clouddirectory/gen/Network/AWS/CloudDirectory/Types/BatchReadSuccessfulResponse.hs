{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @BatchRead@ success response operation.
--
--
--
-- /See:/ 'batchReadSuccessfulResponse' smart constructor.
data BatchReadSuccessfulResponse = BatchReadSuccessfulResponse'
  { _brsListIndex ::
      !(Maybe BatchListIndexResponse),
    _brsGetObjectInformation ::
      !( Maybe
           BatchGetObjectInformationResponse
       ),
    _brsListAttachedIndices ::
      !( Maybe
           BatchListAttachedIndicesResponse
       ),
    _brsLookupPolicy ::
      !(Maybe BatchLookupPolicyResponse),
    _brsListObjectParentPaths ::
      !( Maybe
           BatchListObjectParentPathsResponse
       ),
    _brsListObjectAttributes ::
      !( Maybe
           BatchListObjectAttributesResponse
       ),
    _brsListIncomingTypedLinks ::
      !( Maybe
           BatchListIncomingTypedLinksResponse
       ),
    _brsGetLinkAttributes ::
      !( Maybe
           BatchGetLinkAttributesResponse
       ),
    _brsGetObjectAttributes ::
      !( Maybe
           BatchGetObjectAttributesResponse
       ),
    _brsListObjectChildren ::
      !( Maybe
           BatchListObjectChildrenResponse
       ),
    _brsListObjectParents ::
      !( Maybe
           BatchListObjectParentsResponse
       ),
    _brsListPolicyAttachments ::
      !( Maybe
           BatchListPolicyAttachmentsResponse
       ),
    _brsListOutgoingTypedLinks ::
      !( Maybe
           BatchListOutgoingTypedLinksResponse
       ),
    _brsListObjectPolicies ::
      !( Maybe
           BatchListObjectPoliciesResponse
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchReadSuccessfulResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsListIndex' - Lists objects attached to the specified index.
--
-- * 'brsGetObjectInformation' - Retrieves metadata about an object.
--
-- * 'brsListAttachedIndices' - Lists indices attached to an object.
--
-- * 'brsLookupPolicy' - Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- * 'brsListObjectParentPaths' - Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
--
-- * 'brsListObjectAttributes' - Lists all attributes that are associated with an object.
--
-- * 'brsListIncomingTypedLinks' - Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'brsGetLinkAttributes' - The list of attributes to retrieve from the typed link.
--
-- * 'brsGetObjectAttributes' - Retrieves attributes within a facet that are associated with an object.
--
-- * 'brsListObjectChildren' - Returns a paginated list of child objects that are associated with a given object.
--
-- * 'brsListObjectParents' - Undocumented member.
--
-- * 'brsListPolicyAttachments' - Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- * 'brsListOutgoingTypedLinks' - Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'brsListObjectPolicies' - Returns policies attached to an object in pagination fashion.
batchReadSuccessfulResponse ::
  BatchReadSuccessfulResponse
batchReadSuccessfulResponse =
  BatchReadSuccessfulResponse'
    { _brsListIndex = Nothing,
      _brsGetObjectInformation = Nothing,
      _brsListAttachedIndices = Nothing,
      _brsLookupPolicy = Nothing,
      _brsListObjectParentPaths = Nothing,
      _brsListObjectAttributes = Nothing,
      _brsListIncomingTypedLinks = Nothing,
      _brsGetLinkAttributes = Nothing,
      _brsGetObjectAttributes = Nothing,
      _brsListObjectChildren = Nothing,
      _brsListObjectParents = Nothing,
      _brsListPolicyAttachments = Nothing,
      _brsListOutgoingTypedLinks = Nothing,
      _brsListObjectPolicies = Nothing
    }

-- | Lists objects attached to the specified index.
brsListIndex :: Lens' BatchReadSuccessfulResponse (Maybe BatchListIndexResponse)
brsListIndex = lens _brsListIndex (\s a -> s {_brsListIndex = a})

-- | Retrieves metadata about an object.
brsGetObjectInformation :: Lens' BatchReadSuccessfulResponse (Maybe BatchGetObjectInformationResponse)
brsGetObjectInformation = lens _brsGetObjectInformation (\s a -> s {_brsGetObjectInformation = a})

-- | Lists indices attached to an object.
brsListAttachedIndices :: Lens' BatchReadSuccessfulResponse (Maybe BatchListAttachedIndicesResponse)
brsListAttachedIndices = lens _brsListAttachedIndices (\s a -> s {_brsListAttachedIndices = a})

-- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
brsLookupPolicy :: Lens' BatchReadSuccessfulResponse (Maybe BatchLookupPolicyResponse)
brsLookupPolicy = lens _brsLookupPolicy (\s a -> s {_brsLookupPolicy = a})

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
brsListObjectParentPaths :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectParentPathsResponse)
brsListObjectParentPaths = lens _brsListObjectParentPaths (\s a -> s {_brsListObjectParentPaths = a})

-- | Lists all attributes that are associated with an object.
brsListObjectAttributes :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectAttributesResponse)
brsListObjectAttributes = lens _brsListObjectAttributes (\s a -> s {_brsListObjectAttributes = a})

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
brsListIncomingTypedLinks :: Lens' BatchReadSuccessfulResponse (Maybe BatchListIncomingTypedLinksResponse)
brsListIncomingTypedLinks = lens _brsListIncomingTypedLinks (\s a -> s {_brsListIncomingTypedLinks = a})

-- | The list of attributes to retrieve from the typed link.
brsGetLinkAttributes :: Lens' BatchReadSuccessfulResponse (Maybe BatchGetLinkAttributesResponse)
brsGetLinkAttributes = lens _brsGetLinkAttributes (\s a -> s {_brsGetLinkAttributes = a})

-- | Retrieves attributes within a facet that are associated with an object.
brsGetObjectAttributes :: Lens' BatchReadSuccessfulResponse (Maybe BatchGetObjectAttributesResponse)
brsGetObjectAttributes = lens _brsGetObjectAttributes (\s a -> s {_brsGetObjectAttributes = a})

-- | Returns a paginated list of child objects that are associated with a given object.
brsListObjectChildren :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectChildrenResponse)
brsListObjectChildren = lens _brsListObjectChildren (\s a -> s {_brsListObjectChildren = a})

-- | Undocumented member.
brsListObjectParents :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectParentsResponse)
brsListObjectParents = lens _brsListObjectParents (\s a -> s {_brsListObjectParents = a})

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
brsListPolicyAttachments :: Lens' BatchReadSuccessfulResponse (Maybe BatchListPolicyAttachmentsResponse)
brsListPolicyAttachments = lens _brsListPolicyAttachments (\s a -> s {_brsListPolicyAttachments = a})

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
brsListOutgoingTypedLinks :: Lens' BatchReadSuccessfulResponse (Maybe BatchListOutgoingTypedLinksResponse)
brsListOutgoingTypedLinks = lens _brsListOutgoingTypedLinks (\s a -> s {_brsListOutgoingTypedLinks = a})

-- | Returns policies attached to an object in pagination fashion.
brsListObjectPolicies :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectPoliciesResponse)
brsListObjectPolicies = lens _brsListObjectPolicies (\s a -> s {_brsListObjectPolicies = a})

instance FromJSON BatchReadSuccessfulResponse where
  parseJSON =
    withObject
      "BatchReadSuccessfulResponse"
      ( \x ->
          BatchReadSuccessfulResponse'
            <$> (x .:? "ListIndex")
            <*> (x .:? "GetObjectInformation")
            <*> (x .:? "ListAttachedIndices")
            <*> (x .:? "LookupPolicy")
            <*> (x .:? "ListObjectParentPaths")
            <*> (x .:? "ListObjectAttributes")
            <*> (x .:? "ListIncomingTypedLinks")
            <*> (x .:? "GetLinkAttributes")
            <*> (x .:? "GetObjectAttributes")
            <*> (x .:? "ListObjectChildren")
            <*> (x .:? "ListObjectParents")
            <*> (x .:? "ListPolicyAttachments")
            <*> (x .:? "ListOutgoingTypedLinks")
            <*> (x .:? "ListObjectPolicies")
      )

instance Hashable BatchReadSuccessfulResponse

instance NFData BatchReadSuccessfulResponse
