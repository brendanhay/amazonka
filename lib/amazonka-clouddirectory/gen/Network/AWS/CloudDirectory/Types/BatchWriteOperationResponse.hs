{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse where

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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @BatchWrite@ response operation.
--
--
--
-- /See:/ 'batchWriteOperationResponse' smart constructor.
data BatchWriteOperationResponse = BatchWriteOperationResponse'
  { _bwoDeleteObject ::
      !(Maybe BatchDeleteObjectResponse),
    _bwoDetachFromIndex ::
      !( Maybe
           BatchDetachFromIndexResponse
       ),
    _bwoRemoveFacetFromObject ::
      !( Maybe
           BatchRemoveFacetFromObjectResponse
       ),
    _bwoAttachObject ::
      !(Maybe BatchAttachObjectResponse),
    _bwoCreateObject ::
      !(Maybe BatchCreateObjectResponse),
    _bwoAttachTypedLink ::
      !( Maybe
           BatchAttachTypedLinkResponse
       ),
    _bwoDetachPolicy ::
      !(Maybe BatchDetachPolicyResponse),
    _bwoCreateIndex ::
      !(Maybe BatchCreateIndexResponse),
    _bwoDetachObject ::
      !(Maybe BatchDetachObjectResponse),
    _bwoAddFacetToObject ::
      !( Maybe
           BatchAddFacetToObjectResponse
       ),
    _bwoDetachTypedLink ::
      !( Maybe
           BatchDetachTypedLinkResponse
       ),
    _bwoUpdateObjectAttributes ::
      !( Maybe
           BatchUpdateObjectAttributesResponse
       ),
    _bwoAttachPolicy ::
      !(Maybe BatchAttachPolicyResponse),
    _bwoUpdateLinkAttributes ::
      !( Maybe
           BatchUpdateLinkAttributesResponse
       ),
    _bwoAttachToIndex ::
      !(Maybe BatchAttachToIndexResponse)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchWriteOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bwoDeleteObject' - Deletes an object in a 'Directory' .
--
-- * 'bwoDetachFromIndex' - Detaches the specified object from the specified index.
--
-- * 'bwoRemoveFacetFromObject' - The result of a batch remove facet from object operation.
--
-- * 'bwoAttachObject' - Attaches an object to a 'Directory' .
--
-- * 'bwoCreateObject' - Creates an object in a 'Directory' .
--
-- * 'bwoAttachTypedLink' - Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'bwoDetachPolicy' - Detaches a policy from a 'Directory' .
--
-- * 'bwoCreateIndex' - Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
--
-- * 'bwoDetachObject' - Detaches an object from a 'Directory' .
--
-- * 'bwoAddFacetToObject' - The result of an add facet to object batch operation.
--
-- * 'bwoDetachTypedLink' - Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'bwoUpdateObjectAttributes' - Updates a given object’s attributes.
--
-- * 'bwoAttachPolicy' - Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
-- * 'bwoUpdateLinkAttributes' - Represents the output of a @BatchWrite@ response operation.
--
-- * 'bwoAttachToIndex' - Attaches the specified object to the specified index.
batchWriteOperationResponse ::
  BatchWriteOperationResponse
batchWriteOperationResponse =
  BatchWriteOperationResponse'
    { _bwoDeleteObject = Nothing,
      _bwoDetachFromIndex = Nothing,
      _bwoRemoveFacetFromObject = Nothing,
      _bwoAttachObject = Nothing,
      _bwoCreateObject = Nothing,
      _bwoAttachTypedLink = Nothing,
      _bwoDetachPolicy = Nothing,
      _bwoCreateIndex = Nothing,
      _bwoDetachObject = Nothing,
      _bwoAddFacetToObject = Nothing,
      _bwoDetachTypedLink = Nothing,
      _bwoUpdateObjectAttributes = Nothing,
      _bwoAttachPolicy = Nothing,
      _bwoUpdateLinkAttributes = Nothing,
      _bwoAttachToIndex = Nothing
    }

-- | Deletes an object in a 'Directory' .
bwoDeleteObject :: Lens' BatchWriteOperationResponse (Maybe BatchDeleteObjectResponse)
bwoDeleteObject = lens _bwoDeleteObject (\s a -> s {_bwoDeleteObject = a})

-- | Detaches the specified object from the specified index.
bwoDetachFromIndex :: Lens' BatchWriteOperationResponse (Maybe BatchDetachFromIndexResponse)
bwoDetachFromIndex = lens _bwoDetachFromIndex (\s a -> s {_bwoDetachFromIndex = a})

-- | The result of a batch remove facet from object operation.
bwoRemoveFacetFromObject :: Lens' BatchWriteOperationResponse (Maybe BatchRemoveFacetFromObjectResponse)
bwoRemoveFacetFromObject = lens _bwoRemoveFacetFromObject (\s a -> s {_bwoRemoveFacetFromObject = a})

-- | Attaches an object to a 'Directory' .
bwoAttachObject :: Lens' BatchWriteOperationResponse (Maybe BatchAttachObjectResponse)
bwoAttachObject = lens _bwoAttachObject (\s a -> s {_bwoAttachObject = a})

-- | Creates an object in a 'Directory' .
bwoCreateObject :: Lens' BatchWriteOperationResponse (Maybe BatchCreateObjectResponse)
bwoCreateObject = lens _bwoCreateObject (\s a -> s {_bwoCreateObject = a})

-- | Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
bwoAttachTypedLink :: Lens' BatchWriteOperationResponse (Maybe BatchAttachTypedLinkResponse)
bwoAttachTypedLink = lens _bwoAttachTypedLink (\s a -> s {_bwoAttachTypedLink = a})

-- | Detaches a policy from a 'Directory' .
bwoDetachPolicy :: Lens' BatchWriteOperationResponse (Maybe BatchDetachPolicyResponse)
bwoDetachPolicy = lens _bwoDetachPolicy (\s a -> s {_bwoDetachPolicy = a})

-- | Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
bwoCreateIndex :: Lens' BatchWriteOperationResponse (Maybe BatchCreateIndexResponse)
bwoCreateIndex = lens _bwoCreateIndex (\s a -> s {_bwoCreateIndex = a})

-- | Detaches an object from a 'Directory' .
bwoDetachObject :: Lens' BatchWriteOperationResponse (Maybe BatchDetachObjectResponse)
bwoDetachObject = lens _bwoDetachObject (\s a -> s {_bwoDetachObject = a})

-- | The result of an add facet to object batch operation.
bwoAddFacetToObject :: Lens' BatchWriteOperationResponse (Maybe BatchAddFacetToObjectResponse)
bwoAddFacetToObject = lens _bwoAddFacetToObject (\s a -> s {_bwoAddFacetToObject = a})

-- | Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
bwoDetachTypedLink :: Lens' BatchWriteOperationResponse (Maybe BatchDetachTypedLinkResponse)
bwoDetachTypedLink = lens _bwoDetachTypedLink (\s a -> s {_bwoDetachTypedLink = a})

-- | Updates a given object’s attributes.
bwoUpdateObjectAttributes :: Lens' BatchWriteOperationResponse (Maybe BatchUpdateObjectAttributesResponse)
bwoUpdateObjectAttributes = lens _bwoUpdateObjectAttributes (\s a -> s {_bwoUpdateObjectAttributes = a})

-- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
bwoAttachPolicy :: Lens' BatchWriteOperationResponse (Maybe BatchAttachPolicyResponse)
bwoAttachPolicy = lens _bwoAttachPolicy (\s a -> s {_bwoAttachPolicy = a})

-- | Represents the output of a @BatchWrite@ response operation.
bwoUpdateLinkAttributes :: Lens' BatchWriteOperationResponse (Maybe BatchUpdateLinkAttributesResponse)
bwoUpdateLinkAttributes = lens _bwoUpdateLinkAttributes (\s a -> s {_bwoUpdateLinkAttributes = a})

-- | Attaches the specified object to the specified index.
bwoAttachToIndex :: Lens' BatchWriteOperationResponse (Maybe BatchAttachToIndexResponse)
bwoAttachToIndex = lens _bwoAttachToIndex (\s a -> s {_bwoAttachToIndex = a})

instance FromJSON BatchWriteOperationResponse where
  parseJSON =
    withObject
      "BatchWriteOperationResponse"
      ( \x ->
          BatchWriteOperationResponse'
            <$> (x .:? "DeleteObject")
            <*> (x .:? "DetachFromIndex")
            <*> (x .:? "RemoveFacetFromObject")
            <*> (x .:? "AttachObject")
            <*> (x .:? "CreateObject")
            <*> (x .:? "AttachTypedLink")
            <*> (x .:? "DetachPolicy")
            <*> (x .:? "CreateIndex")
            <*> (x .:? "DetachObject")
            <*> (x .:? "AddFacetToObject")
            <*> (x .:? "DetachTypedLink")
            <*> (x .:? "UpdateObjectAttributes")
            <*> (x .:? "AttachPolicy")
            <*> (x .:? "UpdateLinkAttributes")
            <*> (x .:? "AttachToIndex")
      )

instance Hashable BatchWriteOperationResponse

instance NFData BatchWriteOperationResponse
