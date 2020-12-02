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
-- Module      : Network.AWS.APIGateway.GetResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a collection of 'Resource' resources.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetResources
    (
    -- * Creating a Request
      getResources
    , GetResources
    -- * Request Lenses
    , grsEmbed
    , grsLimit
    , grsPosition
    , grsRestAPIId

    -- * Destructuring the Response
    , getResourcesResponse
    , GetResourcesResponse
    -- * Response Lenses
    , grrsItems
    , grrsPosition
    , grrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to list information about a collection of resources.
--
--
--
-- /See:/ 'getResources' smart constructor.
data GetResources = GetResources'
  { _grsEmbed     :: !(Maybe [Text])
  , _grsLimit     :: !(Maybe Int)
  , _grsPosition  :: !(Maybe Text)
  , _grsRestAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsEmbed' - A query parameter used to retrieve the specified resources embedded in the returned 'Resources' resource in the response. This @embed@ parameter value is a list of comma-separated strings. Currently, the request supports only retrieval of the embedded 'Method' resources this way. The query parameter value must be a single-valued list and contain the @"methods"@ string. For example, @GET /restapis/{restapi_id}/resources?embed=methods@ .
--
-- * 'grsLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'grsPosition' - The current pagination position in the paged result set.
--
-- * 'grsRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
getResources
    :: Text -- ^ 'grsRestAPIId'
    -> GetResources
getResources pRestAPIId_ =
  GetResources'
    { _grsEmbed = Nothing
    , _grsLimit = Nothing
    , _grsPosition = Nothing
    , _grsRestAPIId = pRestAPIId_
    }


-- | A query parameter used to retrieve the specified resources embedded in the returned 'Resources' resource in the response. This @embed@ parameter value is a list of comma-separated strings. Currently, the request supports only retrieval of the embedded 'Method' resources this way. The query parameter value must be a single-valued list and contain the @"methods"@ string. For example, @GET /restapis/{restapi_id}/resources?embed=methods@ .
grsEmbed :: Lens' GetResources [Text]
grsEmbed = lens _grsEmbed (\ s a -> s{_grsEmbed = a}) . _Default . _Coerce

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
grsLimit :: Lens' GetResources (Maybe Int)
grsLimit = lens _grsLimit (\ s a -> s{_grsLimit = a})

-- | The current pagination position in the paged result set.
grsPosition :: Lens' GetResources (Maybe Text)
grsPosition = lens _grsPosition (\ s a -> s{_grsPosition = a})

-- | [Required] The string identifier of the associated 'RestApi' .
grsRestAPIId :: Lens' GetResources Text
grsRestAPIId = lens _grsRestAPIId (\ s a -> s{_grsRestAPIId = a})

instance AWSPager GetResources where
        page rq rs
          | stop (rs ^. grrsPosition) = Nothing
          | stop (rs ^. grrsItems) = Nothing
          | otherwise =
            Just $ rq & grsPosition .~ rs ^. grrsPosition

instance AWSRequest GetResources where
        type Rs GetResources = GetResourcesResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetResourcesResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetResources where

instance NFData GetResources where

instance ToHeaders GetResources where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetResources where
        toPath GetResources'{..}
          = mconcat
              ["/restapis/", toBS _grsRestAPIId, "/resources"]

instance ToQuery GetResources where
        toQuery GetResources'{..}
          = mconcat
              ["embed" =:
                 toQuery (toQueryList "member" <$> _grsEmbed),
               "limit" =: _grsLimit, "position" =: _grsPosition]

-- | Represents a collection of 'Resource' resources.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'getResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { _grrsItems          :: !(Maybe [Resource])
  , _grrsPosition       :: !(Maybe Text)
  , _grrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsItems' - The current page of elements from this collection.
--
-- * 'grrsPosition' - Undocumented member.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getResourcesResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetResourcesResponse
getResourcesResponse pResponseStatus_ =
  GetResourcesResponse'
    { _grrsItems = Nothing
    , _grrsPosition = Nothing
    , _grrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
grrsItems :: Lens' GetResourcesResponse [Resource]
grrsItems = lens _grrsItems (\ s a -> s{_grrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
grrsPosition :: Lens' GetResourcesResponse (Maybe Text)
grrsPosition = lens _grrsPosition (\ s a -> s{_grrsPosition = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetResourcesResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetResourcesResponse where
