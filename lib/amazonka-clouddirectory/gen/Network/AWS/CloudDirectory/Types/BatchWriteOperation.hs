{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchWriteOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchWriteOperation where

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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @BatchWrite@ operation.
--
--
--
-- /See:/ 'batchWriteOperation' smart constructor.
data BatchWriteOperation = BatchWriteOperation'
  { _bDeleteObject ::
      !(Maybe BatchDeleteObject),
    _bDetachFromIndex :: !(Maybe BatchDetachFromIndex),
    _bRemoveFacetFromObject ::
      !(Maybe BatchRemoveFacetFromObject),
    _bAttachObject :: !(Maybe BatchAttachObject),
    _bCreateObject :: !(Maybe BatchCreateObject),
    _bAttachTypedLink :: !(Maybe BatchAttachTypedLink),
    _bDetachPolicy :: !(Maybe BatchDetachPolicy),
    _bCreateIndex :: !(Maybe BatchCreateIndex),
    _bDetachObject :: !(Maybe BatchDetachObject),
    _bAddFacetToObject ::
      !(Maybe BatchAddFacetToObject),
    _bDetachTypedLink :: !(Maybe BatchDetachTypedLink),
    _bUpdateObjectAttributes ::
      !(Maybe BatchUpdateObjectAttributes),
    _bAttachPolicy :: !(Maybe BatchAttachPolicy),
    _bUpdateLinkAttributes ::
      !(Maybe BatchUpdateLinkAttributes),
    _bAttachToIndex :: !(Maybe BatchAttachToIndex)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchWriteOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bDeleteObject' - Deletes an object in a 'Directory' .
--
-- * 'bDetachFromIndex' - Detaches the specified object from the specified index.
--
-- * 'bRemoveFacetFromObject' - A batch operation that removes a facet from an object.
--
-- * 'bAttachObject' - Attaches an object to a 'Directory' .
--
-- * 'bCreateObject' - Creates an object.
--
-- * 'bAttachTypedLink' - Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'bDetachPolicy' - Detaches a policy from a 'Directory' .
--
-- * 'bCreateIndex' - Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
--
-- * 'bDetachObject' - Detaches an object from a 'Directory' .
--
-- * 'bAddFacetToObject' - A batch operation that adds a facet to an object.
--
-- * 'bDetachTypedLink' - Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'bUpdateObjectAttributes' - Updates a given object's attributes.
--
-- * 'bAttachPolicy' - Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
-- * 'bUpdateLinkAttributes' - Updates a given object's attributes.
--
-- * 'bAttachToIndex' - Attaches the specified object to the specified index.
batchWriteOperation ::
  BatchWriteOperation
batchWriteOperation =
  BatchWriteOperation'
    { _bDeleteObject = Nothing,
      _bDetachFromIndex = Nothing,
      _bRemoveFacetFromObject = Nothing,
      _bAttachObject = Nothing,
      _bCreateObject = Nothing,
      _bAttachTypedLink = Nothing,
      _bDetachPolicy = Nothing,
      _bCreateIndex = Nothing,
      _bDetachObject = Nothing,
      _bAddFacetToObject = Nothing,
      _bDetachTypedLink = Nothing,
      _bUpdateObjectAttributes = Nothing,
      _bAttachPolicy = Nothing,
      _bUpdateLinkAttributes = Nothing,
      _bAttachToIndex = Nothing
    }

-- | Deletes an object in a 'Directory' .
bDeleteObject :: Lens' BatchWriteOperation (Maybe BatchDeleteObject)
bDeleteObject = lens _bDeleteObject (\s a -> s {_bDeleteObject = a})

-- | Detaches the specified object from the specified index.
bDetachFromIndex :: Lens' BatchWriteOperation (Maybe BatchDetachFromIndex)
bDetachFromIndex = lens _bDetachFromIndex (\s a -> s {_bDetachFromIndex = a})

-- | A batch operation that removes a facet from an object.
bRemoveFacetFromObject :: Lens' BatchWriteOperation (Maybe BatchRemoveFacetFromObject)
bRemoveFacetFromObject = lens _bRemoveFacetFromObject (\s a -> s {_bRemoveFacetFromObject = a})

-- | Attaches an object to a 'Directory' .
bAttachObject :: Lens' BatchWriteOperation (Maybe BatchAttachObject)
bAttachObject = lens _bAttachObject (\s a -> s {_bAttachObject = a})

-- | Creates an object.
bCreateObject :: Lens' BatchWriteOperation (Maybe BatchCreateObject)
bCreateObject = lens _bCreateObject (\s a -> s {_bCreateObject = a})

-- | Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
bAttachTypedLink :: Lens' BatchWriteOperation (Maybe BatchAttachTypedLink)
bAttachTypedLink = lens _bAttachTypedLink (\s a -> s {_bAttachTypedLink = a})

-- | Detaches a policy from a 'Directory' .
bDetachPolicy :: Lens' BatchWriteOperation (Maybe BatchDetachPolicy)
bDetachPolicy = lens _bDetachPolicy (\s a -> s {_bDetachPolicy = a})

-- | Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.htm Indexing and search> for more information.
bCreateIndex :: Lens' BatchWriteOperation (Maybe BatchCreateIndex)
bCreateIndex = lens _bCreateIndex (\s a -> s {_bCreateIndex = a})

-- | Detaches an object from a 'Directory' .
bDetachObject :: Lens' BatchWriteOperation (Maybe BatchDetachObject)
bDetachObject = lens _bDetachObject (\s a -> s {_bDetachObject = a})

-- | A batch operation that adds a facet to an object.
bAddFacetToObject :: Lens' BatchWriteOperation (Maybe BatchAddFacetToObject)
bAddFacetToObject = lens _bAddFacetToObject (\s a -> s {_bAddFacetToObject = a})

-- | Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
bDetachTypedLink :: Lens' BatchWriteOperation (Maybe BatchDetachTypedLink)
bDetachTypedLink = lens _bDetachTypedLink (\s a -> s {_bDetachTypedLink = a})

-- | Updates a given object's attributes.
bUpdateObjectAttributes :: Lens' BatchWriteOperation (Maybe BatchUpdateObjectAttributes)
bUpdateObjectAttributes = lens _bUpdateObjectAttributes (\s a -> s {_bUpdateObjectAttributes = a})

-- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
bAttachPolicy :: Lens' BatchWriteOperation (Maybe BatchAttachPolicy)
bAttachPolicy = lens _bAttachPolicy (\s a -> s {_bAttachPolicy = a})

-- | Updates a given object's attributes.
bUpdateLinkAttributes :: Lens' BatchWriteOperation (Maybe BatchUpdateLinkAttributes)
bUpdateLinkAttributes = lens _bUpdateLinkAttributes (\s a -> s {_bUpdateLinkAttributes = a})

-- | Attaches the specified object to the specified index.
bAttachToIndex :: Lens' BatchWriteOperation (Maybe BatchAttachToIndex)
bAttachToIndex = lens _bAttachToIndex (\s a -> s {_bAttachToIndex = a})

instance Hashable BatchWriteOperation

instance NFData BatchWriteOperation

instance ToJSON BatchWriteOperation where
  toJSON BatchWriteOperation' {..} =
    object
      ( catMaybes
          [ ("DeleteObject" .=) <$> _bDeleteObject,
            ("DetachFromIndex" .=) <$> _bDetachFromIndex,
            ("RemoveFacetFromObject" .=) <$> _bRemoveFacetFromObject,
            ("AttachObject" .=) <$> _bAttachObject,
            ("CreateObject" .=) <$> _bCreateObject,
            ("AttachTypedLink" .=) <$> _bAttachTypedLink,
            ("DetachPolicy" .=) <$> _bDetachPolicy,
            ("CreateIndex" .=) <$> _bCreateIndex,
            ("DetachObject" .=) <$> _bDetachObject,
            ("AddFacetToObject" .=) <$> _bAddFacetToObject,
            ("DetachTypedLink" .=) <$> _bDetachTypedLink,
            ("UpdateObjectAttributes" .=) <$> _bUpdateObjectAttributes,
            ("AttachPolicy" .=) <$> _bAttachPolicy,
            ("UpdateLinkAttributes" .=) <$> _bUpdateLinkAttributes,
            ("AttachToIndex" .=) <$> _bAttachToIndex
          ]
      )
