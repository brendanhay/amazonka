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
-- Module      : Network.AWS.APIGateway.GetModels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes existing 'Models' defined for a 'RestApi' resource.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetModels
    (
    -- * Creating a Request
      getModels
    , GetModels
    -- * Request Lenses
    , gmsLimit
    , gmsPosition
    , gmsRestAPIId

    -- * Destructuring the Response
    , getModelsResponse
    , GetModelsResponse
    -- * Response Lenses
    , gmrsItems
    , gmrsPosition
    , gmrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to list existing 'Models' defined for a 'RestApi' resource.
--
--
--
-- /See:/ 'getModels' smart constructor.
data GetModels = GetModels'
  { _gmsLimit     :: !(Maybe Int)
  , _gmsPosition  :: !(Maybe Text)
  , _gmsRestAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gmsPosition' - The current pagination position in the paged result set.
--
-- * 'gmsRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
getModels
    :: Text -- ^ 'gmsRestAPIId'
    -> GetModels
getModels pRestAPIId_ =
  GetModels'
    {_gmsLimit = Nothing, _gmsPosition = Nothing, _gmsRestAPIId = pRestAPIId_}


-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gmsLimit :: Lens' GetModels (Maybe Int)
gmsLimit = lens _gmsLimit (\ s a -> s{_gmsLimit = a})

-- | The current pagination position in the paged result set.
gmsPosition :: Lens' GetModels (Maybe Text)
gmsPosition = lens _gmsPosition (\ s a -> s{_gmsPosition = a})

-- | [Required] The string identifier of the associated 'RestApi' .
gmsRestAPIId :: Lens' GetModels Text
gmsRestAPIId = lens _gmsRestAPIId (\ s a -> s{_gmsRestAPIId = a})

instance AWSPager GetModels where
        page rq rs
          | stop (rs ^. gmrsPosition) = Nothing
          | stop (rs ^. gmrsItems) = Nothing
          | otherwise =
            Just $ rq & gmsPosition .~ rs ^. gmrsPosition

instance AWSRequest GetModels where
        type Rs GetModels = GetModelsResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetModelsResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetModels where

instance NFData GetModels where

instance ToHeaders GetModels where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetModels where
        toPath GetModels'{..}
          = mconcat
              ["/restapis/", toBS _gmsRestAPIId, "/models"]

instance ToQuery GetModels where
        toQuery GetModels'{..}
          = mconcat
              ["limit" =: _gmsLimit, "position" =: _gmsPosition]

-- | Represents a collection of 'Model' resources.
--
--
-- 'Method' , 'MethodResponse' , <http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html Models and Mappings>
--
-- /See:/ 'getModelsResponse' smart constructor.
data GetModelsResponse = GetModelsResponse'
  { _gmrsItems          :: !(Maybe [Model])
  , _gmrsPosition       :: !(Maybe Text)
  , _gmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmrsItems' - The current page of elements from this collection.
--
-- * 'gmrsPosition' - Undocumented member.
--
-- * 'gmrsResponseStatus' - -- | The response status code.
getModelsResponse
    :: Int -- ^ 'gmrsResponseStatus'
    -> GetModelsResponse
getModelsResponse pResponseStatus_ =
  GetModelsResponse'
    { _gmrsItems = Nothing
    , _gmrsPosition = Nothing
    , _gmrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
gmrsItems :: Lens' GetModelsResponse [Model]
gmrsItems = lens _gmrsItems (\ s a -> s{_gmrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
gmrsPosition :: Lens' GetModelsResponse (Maybe Text)
gmrsPosition = lens _gmrsPosition (\ s a -> s{_gmrsPosition = a})

-- | -- | The response status code.
gmrsResponseStatus :: Lens' GetModelsResponse Int
gmrsResponseStatus = lens _gmrsResponseStatus (\ s a -> s{_gmrsResponseStatus = a})

instance NFData GetModelsResponse where
