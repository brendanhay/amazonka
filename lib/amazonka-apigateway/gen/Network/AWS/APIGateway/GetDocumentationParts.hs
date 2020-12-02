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
-- Module      : Network.AWS.APIGateway.GetDocumentationParts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetDocumentationParts
    (
    -- * Creating a Request
      getDocumentationParts
    , GetDocumentationParts
    -- * Request Lenses
    , gdpPath
    , gdpLocationStatus
    , gdpNameQuery
    , gdpLimit
    , gdpType
    , gdpPosition
    , gdpRestAPIId

    -- * Destructuring the Response
    , getDocumentationPartsResponse
    , GetDocumentationPartsResponse
    -- * Response Lenses
    , gdprsItems
    , gdprsPosition
    , gdprsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Gets the documentation parts of an API. The result may be filtered by the type, name, or path of API entities (targets).
--
--
--
-- /See:/ 'getDocumentationParts' smart constructor.
data GetDocumentationParts = GetDocumentationParts'
  { _gdpPath           :: !(Maybe Text)
  , _gdpLocationStatus :: !(Maybe LocationStatusType)
  , _gdpNameQuery      :: !(Maybe Text)
  , _gdpLimit          :: !(Maybe Int)
  , _gdpType           :: !(Maybe DocumentationPartType)
  , _gdpPosition       :: !(Maybe Text)
  , _gdpRestAPIId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentationParts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdpPath' - The path of API entities of the to-be-retrieved documentation parts.
--
-- * 'gdpLocationStatus' - The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
--
-- * 'gdpNameQuery' - The name of API entities of the to-be-retrieved documentation parts.
--
-- * 'gdpLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gdpType' - The type of API entities of the to-be-retrieved documentation parts.
--
-- * 'gdpPosition' - The current pagination position in the paged result set.
--
-- * 'gdpRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
getDocumentationParts
    :: Text -- ^ 'gdpRestAPIId'
    -> GetDocumentationParts
getDocumentationParts pRestAPIId_ =
  GetDocumentationParts'
    { _gdpPath = Nothing
    , _gdpLocationStatus = Nothing
    , _gdpNameQuery = Nothing
    , _gdpLimit = Nothing
    , _gdpType = Nothing
    , _gdpPosition = Nothing
    , _gdpRestAPIId = pRestAPIId_
    }


-- | The path of API entities of the to-be-retrieved documentation parts.
gdpPath :: Lens' GetDocumentationParts (Maybe Text)
gdpPath = lens _gdpPath (\ s a -> s{_gdpPath = a})

-- | The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
gdpLocationStatus :: Lens' GetDocumentationParts (Maybe LocationStatusType)
gdpLocationStatus = lens _gdpLocationStatus (\ s a -> s{_gdpLocationStatus = a})

-- | The name of API entities of the to-be-retrieved documentation parts.
gdpNameQuery :: Lens' GetDocumentationParts (Maybe Text)
gdpNameQuery = lens _gdpNameQuery (\ s a -> s{_gdpNameQuery = a})

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gdpLimit :: Lens' GetDocumentationParts (Maybe Int)
gdpLimit = lens _gdpLimit (\ s a -> s{_gdpLimit = a})

-- | The type of API entities of the to-be-retrieved documentation parts.
gdpType :: Lens' GetDocumentationParts (Maybe DocumentationPartType)
gdpType = lens _gdpType (\ s a -> s{_gdpType = a})

-- | The current pagination position in the paged result set.
gdpPosition :: Lens' GetDocumentationParts (Maybe Text)
gdpPosition = lens _gdpPosition (\ s a -> s{_gdpPosition = a})

-- | [Required] The string identifier of the associated 'RestApi' .
gdpRestAPIId :: Lens' GetDocumentationParts Text
gdpRestAPIId = lens _gdpRestAPIId (\ s a -> s{_gdpRestAPIId = a})

instance AWSRequest GetDocumentationParts where
        type Rs GetDocumentationParts =
             GetDocumentationPartsResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentationPartsResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetDocumentationParts where

instance NFData GetDocumentationParts where

instance ToHeaders GetDocumentationParts where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetDocumentationParts where
        toPath GetDocumentationParts'{..}
          = mconcat
              ["/restapis/", toBS _gdpRestAPIId,
               "/documentation/parts"]

instance ToQuery GetDocumentationParts where
        toQuery GetDocumentationParts'{..}
          = mconcat
              ["path" =: _gdpPath,
               "locationStatus" =: _gdpLocationStatus,
               "name" =: _gdpNameQuery, "limit" =: _gdpLimit,
               "type" =: _gdpType, "position" =: _gdpPosition]

-- | The collection of documentation parts of an API.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart'
--
-- /See:/ 'getDocumentationPartsResponse' smart constructor.
data GetDocumentationPartsResponse = GetDocumentationPartsResponse'
  { _gdprsItems          :: !(Maybe [DocumentationPart])
  , _gdprsPosition       :: !(Maybe Text)
  , _gdprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentationPartsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdprsItems' - The current page of elements from this collection.
--
-- * 'gdprsPosition' - Undocumented member.
--
-- * 'gdprsResponseStatus' - -- | The response status code.
getDocumentationPartsResponse
    :: Int -- ^ 'gdprsResponseStatus'
    -> GetDocumentationPartsResponse
getDocumentationPartsResponse pResponseStatus_ =
  GetDocumentationPartsResponse'
    { _gdprsItems = Nothing
    , _gdprsPosition = Nothing
    , _gdprsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
gdprsItems :: Lens' GetDocumentationPartsResponse [DocumentationPart]
gdprsItems = lens _gdprsItems (\ s a -> s{_gdprsItems = a}) . _Default . _Coerce

-- | Undocumented member.
gdprsPosition :: Lens' GetDocumentationPartsResponse (Maybe Text)
gdprsPosition = lens _gdprsPosition (\ s a -> s{_gdprsPosition = a})

-- | -- | The response status code.
gdprsResponseStatus :: Lens' GetDocumentationPartsResponse Int
gdprsResponseStatus = lens _gdprsResponseStatus (\ s a -> s{_gdprsResponseStatus = a})

instance NFData GetDocumentationPartsResponse where
