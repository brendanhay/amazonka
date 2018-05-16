{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AddFacetToObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Facet' to an object. An object can have more than one facet applied on it.
--
--
module Network.AWS.CloudDirectory.AddFacetToObject
    (
    -- * Creating a Request
      addFacetToObject
    , AddFacetToObject
    -- * Request Lenses
    , aftoObjectAttributeList
    , aftoDirectoryARN
    , aftoSchemaFacet
    , aftoObjectReference

    -- * Destructuring the Response
    , addFacetToObjectResponse
    , AddFacetToObjectResponse
    -- * Response Lenses
    , aftorsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addFacetToObject' smart constructor.
data AddFacetToObject = AddFacetToObject'
  { _aftoObjectAttributeList :: !(Maybe [AttributeKeyAndValue])
  , _aftoDirectoryARN        :: !Text
  , _aftoSchemaFacet         :: !SchemaFacet
  , _aftoObjectReference     :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddFacetToObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aftoObjectAttributeList' - Attributes on the facet that you are adding to the object.
--
-- * 'aftoDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- * 'aftoSchemaFacet' - Identifiers for the facet that you are adding to the object. See 'SchemaFacet' for details.
--
-- * 'aftoObjectReference' - A reference to the object you are adding the specified facet to.
addFacetToObject
    :: Text -- ^ 'aftoDirectoryARN'
    -> SchemaFacet -- ^ 'aftoSchemaFacet'
    -> ObjectReference -- ^ 'aftoObjectReference'
    -> AddFacetToObject
addFacetToObject pDirectoryARN_ pSchemaFacet_ pObjectReference_ =
  AddFacetToObject'
    { _aftoObjectAttributeList = Nothing
    , _aftoDirectoryARN = pDirectoryARN_
    , _aftoSchemaFacet = pSchemaFacet_
    , _aftoObjectReference = pObjectReference_
    }


-- | Attributes on the facet that you are adding to the object.
aftoObjectAttributeList :: Lens' AddFacetToObject [AttributeKeyAndValue]
aftoObjectAttributeList = lens _aftoObjectAttributeList (\ s a -> s{_aftoObjectAttributeList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
aftoDirectoryARN :: Lens' AddFacetToObject Text
aftoDirectoryARN = lens _aftoDirectoryARN (\ s a -> s{_aftoDirectoryARN = a})

-- | Identifiers for the facet that you are adding to the object. See 'SchemaFacet' for details.
aftoSchemaFacet :: Lens' AddFacetToObject SchemaFacet
aftoSchemaFacet = lens _aftoSchemaFacet (\ s a -> s{_aftoSchemaFacet = a})

-- | A reference to the object you are adding the specified facet to.
aftoObjectReference :: Lens' AddFacetToObject ObjectReference
aftoObjectReference = lens _aftoObjectReference (\ s a -> s{_aftoObjectReference = a})

instance AWSRequest AddFacetToObject where
        type Rs AddFacetToObject = AddFacetToObjectResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 AddFacetToObjectResponse' <$> (pure (fromEnum s)))

instance Hashable AddFacetToObject where

instance NFData AddFacetToObject where

instance ToHeaders AddFacetToObject where
        toHeaders AddFacetToObject'{..}
          = mconcat
              ["x-amz-data-partition" =# _aftoDirectoryARN]

instance ToJSON AddFacetToObject where
        toJSON AddFacetToObject'{..}
          = object
              (catMaybes
                 [("ObjectAttributeList" .=) <$>
                    _aftoObjectAttributeList,
                  Just ("SchemaFacet" .= _aftoSchemaFacet),
                  Just ("ObjectReference" .= _aftoObjectReference)])

instance ToPath AddFacetToObject where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/facets"

instance ToQuery AddFacetToObject where
        toQuery = const mempty

-- | /See:/ 'addFacetToObjectResponse' smart constructor.
newtype AddFacetToObjectResponse = AddFacetToObjectResponse'
  { _aftorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddFacetToObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aftorsResponseStatus' - -- | The response status code.
addFacetToObjectResponse
    :: Int -- ^ 'aftorsResponseStatus'
    -> AddFacetToObjectResponse
addFacetToObjectResponse pResponseStatus_ =
  AddFacetToObjectResponse' {_aftorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aftorsResponseStatus :: Lens' AddFacetToObjectResponse Int
aftorsResponseStatus = lens _aftorsResponseStatus (\ s a -> s{_aftorsResponseStatus = a})

instance NFData AddFacetToObjectResponse where
