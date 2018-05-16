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
-- Module      : Network.AWS.CloudDirectory.GetObjectAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes within a facet that are associated with an object.
--
--
module Network.AWS.CloudDirectory.GetObjectAttributes
    (
    -- * Creating a Request
      getObjectAttributes
    , GetObjectAttributes
    -- * Request Lenses
    , goaConsistencyLevel
    , goaDirectoryARN
    , goaObjectReference
    , goaSchemaFacet
    , goaAttributeNames

    -- * Destructuring the Response
    , getObjectAttributesResponse
    , GetObjectAttributesResponse
    -- * Response Lenses
    , goarsAttributes
    , goarsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getObjectAttributes' smart constructor.
data GetObjectAttributes = GetObjectAttributes'
  { _goaConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _goaDirectoryARN     :: !Text
  , _goaObjectReference  :: !ObjectReference
  , _goaSchemaFacet      :: !SchemaFacet
  , _goaAttributeNames   :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goaConsistencyLevel' - The consistency level at which to retrieve the attributes on an object.
--
-- * 'goaDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides.
--
-- * 'goaObjectReference' - Reference that identifies the object whose attributes will be retrieved.
--
-- * 'goaSchemaFacet' - Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
--
-- * 'goaAttributeNames' - List of attribute names whose values will be retrieved.
getObjectAttributes
    :: Text -- ^ 'goaDirectoryARN'
    -> ObjectReference -- ^ 'goaObjectReference'
    -> SchemaFacet -- ^ 'goaSchemaFacet'
    -> GetObjectAttributes
getObjectAttributes pDirectoryARN_ pObjectReference_ pSchemaFacet_ =
  GetObjectAttributes'
    { _goaConsistencyLevel = Nothing
    , _goaDirectoryARN = pDirectoryARN_
    , _goaObjectReference = pObjectReference_
    , _goaSchemaFacet = pSchemaFacet_
    , _goaAttributeNames = mempty
    }


-- | The consistency level at which to retrieve the attributes on an object.
goaConsistencyLevel :: Lens' GetObjectAttributes (Maybe ConsistencyLevel)
goaConsistencyLevel = lens _goaConsistencyLevel (\ s a -> s{_goaConsistencyLevel = a})

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides.
goaDirectoryARN :: Lens' GetObjectAttributes Text
goaDirectoryARN = lens _goaDirectoryARN (\ s a -> s{_goaDirectoryARN = a})

-- | Reference that identifies the object whose attributes will be retrieved.
goaObjectReference :: Lens' GetObjectAttributes ObjectReference
goaObjectReference = lens _goaObjectReference (\ s a -> s{_goaObjectReference = a})

-- | Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
goaSchemaFacet :: Lens' GetObjectAttributes SchemaFacet
goaSchemaFacet = lens _goaSchemaFacet (\ s a -> s{_goaSchemaFacet = a})

-- | List of attribute names whose values will be retrieved.
goaAttributeNames :: Lens' GetObjectAttributes [Text]
goaAttributeNames = lens _goaAttributeNames (\ s a -> s{_goaAttributeNames = a}) . _Coerce

instance AWSRequest GetObjectAttributes where
        type Rs GetObjectAttributes =
             GetObjectAttributesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 GetObjectAttributesResponse' <$>
                   (x .?> "Attributes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetObjectAttributes where

instance NFData GetObjectAttributes where

instance ToHeaders GetObjectAttributes where
        toHeaders GetObjectAttributes'{..}
          = mconcat
              ["x-amz-consistency-level" =# _goaConsistencyLevel,
               "x-amz-data-partition" =# _goaDirectoryARN]

instance ToJSON GetObjectAttributes where
        toJSON GetObjectAttributes'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _goaObjectReference),
                  Just ("SchemaFacet" .= _goaSchemaFacet),
                  Just ("AttributeNames" .= _goaAttributeNames)])

instance ToPath GetObjectAttributes where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/attributes/get"

instance ToQuery GetObjectAttributes where
        toQuery = const mempty

-- | /See:/ 'getObjectAttributesResponse' smart constructor.
data GetObjectAttributesResponse = GetObjectAttributesResponse'
  { _goarsAttributes     :: !(Maybe [AttributeKeyAndValue])
  , _goarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goarsAttributes' - The attributes that are associated with the object.
--
-- * 'goarsResponseStatus' - -- | The response status code.
getObjectAttributesResponse
    :: Int -- ^ 'goarsResponseStatus'
    -> GetObjectAttributesResponse
getObjectAttributesResponse pResponseStatus_ =
  GetObjectAttributesResponse'
    {_goarsAttributes = Nothing, _goarsResponseStatus = pResponseStatus_}


-- | The attributes that are associated with the object.
goarsAttributes :: Lens' GetObjectAttributesResponse [AttributeKeyAndValue]
goarsAttributes = lens _goarsAttributes (\ s a -> s{_goarsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
goarsResponseStatus :: Lens' GetObjectAttributesResponse Int
goarsResponseStatus = lens _goarsResponseStatus (\ s a -> s{_goarsResponseStatus = a})

instance NFData GetObjectAttributesResponse where
