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
-- Module      : Network.AWS.CloudDirectory.ListObjectAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all attributes that are associated with an object.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectAttributes
    (
    -- * Creating a Request
      listObjectAttributes
    , ListObjectAttributes
    -- * Request Lenses
    , loaFacetFilter
    , loaConsistencyLevel
    , loaNextToken
    , loaMaxResults
    , loaDirectoryARN
    , loaObjectReference

    -- * Destructuring the Response
    , listObjectAttributesResponse
    , ListObjectAttributesResponse
    -- * Response Lenses
    , loarsNextToken
    , loarsAttributes
    , loarsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listObjectAttributes' smart constructor.
data ListObjectAttributes = ListObjectAttributes'
  { _loaFacetFilter      :: !(Maybe SchemaFacet)
  , _loaConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _loaNextToken        :: !(Maybe Text)
  , _loaMaxResults       :: !(Maybe Nat)
  , _loaDirectoryARN     :: !Text
  , _loaObjectReference  :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loaFacetFilter' - Used to filter the list of object attributes that are associated with a certain facet.
--
-- * 'loaConsistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- * 'loaNextToken' - The pagination token.
--
-- * 'loaMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'loaDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- * 'loaObjectReference' - The reference that identifies the object whose attributes will be listed.
listObjectAttributes
    :: Text -- ^ 'loaDirectoryARN'
    -> ObjectReference -- ^ 'loaObjectReference'
    -> ListObjectAttributes
listObjectAttributes pDirectoryARN_ pObjectReference_ =
  ListObjectAttributes'
    { _loaFacetFilter = Nothing
    , _loaConsistencyLevel = Nothing
    , _loaNextToken = Nothing
    , _loaMaxResults = Nothing
    , _loaDirectoryARN = pDirectoryARN_
    , _loaObjectReference = pObjectReference_
    }


-- | Used to filter the list of object attributes that are associated with a certain facet.
loaFacetFilter :: Lens' ListObjectAttributes (Maybe SchemaFacet)
loaFacetFilter = lens _loaFacetFilter (\ s a -> s{_loaFacetFilter = a})

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
loaConsistencyLevel :: Lens' ListObjectAttributes (Maybe ConsistencyLevel)
loaConsistencyLevel = lens _loaConsistencyLevel (\ s a -> s{_loaConsistencyLevel = a})

-- | The pagination token.
loaNextToken :: Lens' ListObjectAttributes (Maybe Text)
loaNextToken = lens _loaNextToken (\ s a -> s{_loaNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
loaMaxResults :: Lens' ListObjectAttributes (Maybe Natural)
loaMaxResults = lens _loaMaxResults (\ s a -> s{_loaMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
loaDirectoryARN :: Lens' ListObjectAttributes Text
loaDirectoryARN = lens _loaDirectoryARN (\ s a -> s{_loaDirectoryARN = a})

-- | The reference that identifies the object whose attributes will be listed.
loaObjectReference :: Lens' ListObjectAttributes ObjectReference
loaObjectReference = lens _loaObjectReference (\ s a -> s{_loaObjectReference = a})

instance AWSPager ListObjectAttributes where
        page rq rs
          | stop (rs ^. loarsNextToken) = Nothing
          | stop (rs ^. loarsAttributes) = Nothing
          | otherwise =
            Just $ rq & loaNextToken .~ rs ^. loarsNextToken

instance AWSRequest ListObjectAttributes where
        type Rs ListObjectAttributes =
             ListObjectAttributesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListObjectAttributesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Attributes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListObjectAttributes where

instance NFData ListObjectAttributes where

instance ToHeaders ListObjectAttributes where
        toHeaders ListObjectAttributes'{..}
          = mconcat
              ["x-amz-consistency-level" =# _loaConsistencyLevel,
               "x-amz-data-partition" =# _loaDirectoryARN]

instance ToJSON ListObjectAttributes where
        toJSON ListObjectAttributes'{..}
          = object
              (catMaybes
                 [("FacetFilter" .=) <$> _loaFacetFilter,
                  ("NextToken" .=) <$> _loaNextToken,
                  ("MaxResults" .=) <$> _loaMaxResults,
                  Just ("ObjectReference" .= _loaObjectReference)])

instance ToPath ListObjectAttributes where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/attributes"

instance ToQuery ListObjectAttributes where
        toQuery = const mempty

-- | /See:/ 'listObjectAttributesResponse' smart constructor.
data ListObjectAttributesResponse = ListObjectAttributesResponse'
  { _loarsNextToken      :: !(Maybe Text)
  , _loarsAttributes     :: !(Maybe [AttributeKeyAndValue])
  , _loarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loarsNextToken' - The pagination token.
--
-- * 'loarsAttributes' - Attributes map that is associated with the object. @AttributeArn@ is the key, and attribute value is the value.
--
-- * 'loarsResponseStatus' - -- | The response status code.
listObjectAttributesResponse
    :: Int -- ^ 'loarsResponseStatus'
    -> ListObjectAttributesResponse
listObjectAttributesResponse pResponseStatus_ =
  ListObjectAttributesResponse'
    { _loarsNextToken = Nothing
    , _loarsAttributes = Nothing
    , _loarsResponseStatus = pResponseStatus_
    }


-- | The pagination token.
loarsNextToken :: Lens' ListObjectAttributesResponse (Maybe Text)
loarsNextToken = lens _loarsNextToken (\ s a -> s{_loarsNextToken = a})

-- | Attributes map that is associated with the object. @AttributeArn@ is the key, and attribute value is the value.
loarsAttributes :: Lens' ListObjectAttributesResponse [AttributeKeyAndValue]
loarsAttributes = lens _loarsAttributes (\ s a -> s{_loarsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
loarsResponseStatus :: Lens' ListObjectAttributesResponse Int
loarsResponseStatus = lens _loarsResponseStatus (\ s a -> s{_loarsResponseStatus = a})

instance NFData ListObjectAttributesResponse where
