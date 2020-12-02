{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadOperation where

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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @BatchRead@ operation.
--
--
--
-- /See:/ 'batchReadOperation' smart constructor.
data BatchReadOperation = BatchReadOperation'
  { _broListIndex ::
      !(Maybe BatchListIndex),
    _broGetObjectInformation ::
      !(Maybe BatchGetObjectInformation),
    _broListAttachedIndices ::
      !(Maybe BatchListAttachedIndices),
    _broLookupPolicy :: !(Maybe BatchLookupPolicy),
    _broListObjectParentPaths ::
      !(Maybe BatchListObjectParentPaths),
    _broListObjectAttributes ::
      !(Maybe BatchListObjectAttributes),
    _broListIncomingTypedLinks ::
      !(Maybe BatchListIncomingTypedLinks),
    _broGetLinkAttributes ::
      !(Maybe BatchGetLinkAttributes),
    _broGetObjectAttributes ::
      !(Maybe BatchGetObjectAttributes),
    _broListObjectChildren ::
      !(Maybe BatchListObjectChildren),
    _broListObjectParents ::
      !(Maybe BatchListObjectParents),
    _broListPolicyAttachments ::
      !(Maybe BatchListPolicyAttachments),
    _broListOutgoingTypedLinks ::
      !(Maybe BatchListOutgoingTypedLinks),
    _broListObjectPolicies ::
      !(Maybe BatchListObjectPolicies)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchReadOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'broListIndex' - Lists objects attached to the specified index.
--
-- * 'broGetObjectInformation' - Retrieves metadata about an object.
--
-- * 'broListAttachedIndices' - Lists indices attached to an object.
--
-- * 'broLookupPolicy' - Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- * 'broListObjectParentPaths' - Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
--
-- * 'broListObjectAttributes' - Lists all attributes that are associated with an object.
--
-- * 'broListIncomingTypedLinks' - Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'broGetLinkAttributes' - Retrieves attributes that are associated with a typed link.
--
-- * 'broGetObjectAttributes' - Retrieves attributes within a facet that are associated with an object.
--
-- * 'broListObjectChildren' - Returns a paginated list of child objects that are associated with a given object.
--
-- * 'broListObjectParents' - Undocumented member.
--
-- * 'broListPolicyAttachments' - Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- * 'broListOutgoingTypedLinks' - Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'broListObjectPolicies' - Returns policies attached to an object in pagination fashion.
batchReadOperation ::
  BatchReadOperation
batchReadOperation =
  BatchReadOperation'
    { _broListIndex = Nothing,
      _broGetObjectInformation = Nothing,
      _broListAttachedIndices = Nothing,
      _broLookupPolicy = Nothing,
      _broListObjectParentPaths = Nothing,
      _broListObjectAttributes = Nothing,
      _broListIncomingTypedLinks = Nothing,
      _broGetLinkAttributes = Nothing,
      _broGetObjectAttributes = Nothing,
      _broListObjectChildren = Nothing,
      _broListObjectParents = Nothing,
      _broListPolicyAttachments = Nothing,
      _broListOutgoingTypedLinks = Nothing,
      _broListObjectPolicies = Nothing
    }

-- | Lists objects attached to the specified index.
broListIndex :: Lens' BatchReadOperation (Maybe BatchListIndex)
broListIndex = lens _broListIndex (\s a -> s {_broListIndex = a})

-- | Retrieves metadata about an object.
broGetObjectInformation :: Lens' BatchReadOperation (Maybe BatchGetObjectInformation)
broGetObjectInformation = lens _broGetObjectInformation (\s a -> s {_broGetObjectInformation = a})

-- | Lists indices attached to an object.
broListAttachedIndices :: Lens' BatchReadOperation (Maybe BatchListAttachedIndices)
broListAttachedIndices = lens _broListAttachedIndices (\s a -> s {_broListAttachedIndices = a})

-- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
broLookupPolicy :: Lens' BatchReadOperation (Maybe BatchLookupPolicy)
broLookupPolicy = lens _broLookupPolicy (\s a -> s {_broLookupPolicy = a})

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
broListObjectParentPaths :: Lens' BatchReadOperation (Maybe BatchListObjectParentPaths)
broListObjectParentPaths = lens _broListObjectParentPaths (\s a -> s {_broListObjectParentPaths = a})

-- | Lists all attributes that are associated with an object.
broListObjectAttributes :: Lens' BatchReadOperation (Maybe BatchListObjectAttributes)
broListObjectAttributes = lens _broListObjectAttributes (\s a -> s {_broListObjectAttributes = a})

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
broListIncomingTypedLinks :: Lens' BatchReadOperation (Maybe BatchListIncomingTypedLinks)
broListIncomingTypedLinks = lens _broListIncomingTypedLinks (\s a -> s {_broListIncomingTypedLinks = a})

-- | Retrieves attributes that are associated with a typed link.
broGetLinkAttributes :: Lens' BatchReadOperation (Maybe BatchGetLinkAttributes)
broGetLinkAttributes = lens _broGetLinkAttributes (\s a -> s {_broGetLinkAttributes = a})

-- | Retrieves attributes within a facet that are associated with an object.
broGetObjectAttributes :: Lens' BatchReadOperation (Maybe BatchGetObjectAttributes)
broGetObjectAttributes = lens _broGetObjectAttributes (\s a -> s {_broGetObjectAttributes = a})

-- | Returns a paginated list of child objects that are associated with a given object.
broListObjectChildren :: Lens' BatchReadOperation (Maybe BatchListObjectChildren)
broListObjectChildren = lens _broListObjectChildren (\s a -> s {_broListObjectChildren = a})

-- | Undocumented member.
broListObjectParents :: Lens' BatchReadOperation (Maybe BatchListObjectParents)
broListObjectParents = lens _broListObjectParents (\s a -> s {_broListObjectParents = a})

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
broListPolicyAttachments :: Lens' BatchReadOperation (Maybe BatchListPolicyAttachments)
broListPolicyAttachments = lens _broListPolicyAttachments (\s a -> s {_broListPolicyAttachments = a})

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
broListOutgoingTypedLinks :: Lens' BatchReadOperation (Maybe BatchListOutgoingTypedLinks)
broListOutgoingTypedLinks = lens _broListOutgoingTypedLinks (\s a -> s {_broListOutgoingTypedLinks = a})

-- | Returns policies attached to an object in pagination fashion.
broListObjectPolicies :: Lens' BatchReadOperation (Maybe BatchListObjectPolicies)
broListObjectPolicies = lens _broListObjectPolicies (\s a -> s {_broListObjectPolicies = a})

instance Hashable BatchReadOperation

instance NFData BatchReadOperation

instance ToJSON BatchReadOperation where
  toJSON BatchReadOperation' {..} =
    object
      ( catMaybes
          [ ("ListIndex" .=) <$> _broListIndex,
            ("GetObjectInformation" .=) <$> _broGetObjectInformation,
            ("ListAttachedIndices" .=) <$> _broListAttachedIndices,
            ("LookupPolicy" .=) <$> _broLookupPolicy,
            ("ListObjectParentPaths" .=) <$> _broListObjectParentPaths,
            ("ListObjectAttributes" .=) <$> _broListObjectAttributes,
            ("ListIncomingTypedLinks" .=) <$> _broListIncomingTypedLinks,
            ("GetLinkAttributes" .=) <$> _broGetLinkAttributes,
            ("GetObjectAttributes" .=) <$> _broGetObjectAttributes,
            ("ListObjectChildren" .=) <$> _broListObjectChildren,
            ("ListObjectParents" .=) <$> _broListObjectParents,
            ("ListPolicyAttachments" .=) <$> _broListPolicyAttachments,
            ("ListOutgoingTypedLinks" .=) <$> _broListOutgoingTypedLinks,
            ("ListObjectPolicies" .=) <$> _broListObjectPolicies
          ]
      )
