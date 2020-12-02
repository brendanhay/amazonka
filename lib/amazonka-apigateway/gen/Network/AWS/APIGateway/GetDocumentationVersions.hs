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
-- Module      : Network.AWS.APIGateway.GetDocumentationVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetDocumentationVersions
    (
    -- * Creating a Request
      getDocumentationVersions
    , GetDocumentationVersions
    -- * Request Lenses
    , gdvLimit
    , gdvPosition
    , gdvRestAPIId

    -- * Destructuring the Response
    , getDocumentationVersionsResponse
    , GetDocumentationVersionsResponse
    -- * Response Lenses
    , gdvrsItems
    , gdvrsPosition
    , gdvrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Gets the documentation versions of an API.
--
--
--
-- /See:/ 'getDocumentationVersions' smart constructor.
data GetDocumentationVersions = GetDocumentationVersions'
  { _gdvLimit     :: !(Maybe Int)
  , _gdvPosition  :: !(Maybe Text)
  , _gdvRestAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentationVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdvLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gdvPosition' - The current pagination position in the paged result set.
--
-- * 'gdvRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
getDocumentationVersions
    :: Text -- ^ 'gdvRestAPIId'
    -> GetDocumentationVersions
getDocumentationVersions pRestAPIId_ =
  GetDocumentationVersions'
    {_gdvLimit = Nothing, _gdvPosition = Nothing, _gdvRestAPIId = pRestAPIId_}


-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gdvLimit :: Lens' GetDocumentationVersions (Maybe Int)
gdvLimit = lens _gdvLimit (\ s a -> s{_gdvLimit = a})

-- | The current pagination position in the paged result set.
gdvPosition :: Lens' GetDocumentationVersions (Maybe Text)
gdvPosition = lens _gdvPosition (\ s a -> s{_gdvPosition = a})

-- | [Required] The string identifier of the associated 'RestApi' .
gdvRestAPIId :: Lens' GetDocumentationVersions Text
gdvRestAPIId = lens _gdvRestAPIId (\ s a -> s{_gdvRestAPIId = a})

instance AWSRequest GetDocumentationVersions where
        type Rs GetDocumentationVersions =
             GetDocumentationVersionsResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentationVersionsResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetDocumentationVersions where

instance NFData GetDocumentationVersions where

instance ToHeaders GetDocumentationVersions where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetDocumentationVersions where
        toPath GetDocumentationVersions'{..}
          = mconcat
              ["/restapis/", toBS _gdvRestAPIId,
               "/documentation/versions"]

instance ToQuery GetDocumentationVersions where
        toQuery GetDocumentationVersions'{..}
          = mconcat
              ["limit" =: _gdvLimit, "position" =: _gdvPosition]

-- | The collection of documentation snapshots of an API.
--
--
-- Use the 'DocumentationVersions' to manage documentation snapshots associated with various API stages.
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart' , 'DocumentationVersion'
--
-- /See:/ 'getDocumentationVersionsResponse' smart constructor.
data GetDocumentationVersionsResponse = GetDocumentationVersionsResponse'
  { _gdvrsItems          :: !(Maybe [DocumentationVersion])
  , _gdvrsPosition       :: !(Maybe Text)
  , _gdvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentationVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdvrsItems' - The current page of elements from this collection.
--
-- * 'gdvrsPosition' - Undocumented member.
--
-- * 'gdvrsResponseStatus' - -- | The response status code.
getDocumentationVersionsResponse
    :: Int -- ^ 'gdvrsResponseStatus'
    -> GetDocumentationVersionsResponse
getDocumentationVersionsResponse pResponseStatus_ =
  GetDocumentationVersionsResponse'
    { _gdvrsItems = Nothing
    , _gdvrsPosition = Nothing
    , _gdvrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
gdvrsItems :: Lens' GetDocumentationVersionsResponse [DocumentationVersion]
gdvrsItems = lens _gdvrsItems (\ s a -> s{_gdvrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
gdvrsPosition :: Lens' GetDocumentationVersionsResponse (Maybe Text)
gdvrsPosition = lens _gdvrsPosition (\ s a -> s{_gdvrsPosition = a})

-- | -- | The response status code.
gdvrsResponseStatus :: Lens' GetDocumentationVersionsResponse Int
gdvrsResponseStatus = lens _gdvrsResponseStatus (\ s a -> s{_gdvrsResponseStatus = a})

instance NFData GetDocumentationVersionsResponse
         where
