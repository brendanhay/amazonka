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
-- Module      : Network.AWS.APIGateway.GetDeployments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Deployments' collection.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDeployments
    (
    -- * Creating a Request
      getDeployments
    , GetDeployments
    -- * Request Lenses
    , gdLimit
    , gdPosition
    , gdRestAPIId

    -- * Destructuring the Response
    , getDeploymentsResponse
    , GetDeploymentsResponse
    -- * Response Lenses
    , gdrsItems
    , gdrsPosition
    , gdrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests API Gateway to get information about a 'Deployments' collection.
--
--
--
-- /See:/ 'getDeployments' smart constructor.
data GetDeployments = GetDeployments'
  { _gdLimit     :: !(Maybe Int)
  , _gdPosition  :: !(Maybe Text)
  , _gdRestAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gdPosition' - The current pagination position in the paged result set.
--
-- * 'gdRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
getDeployments
    :: Text -- ^ 'gdRestAPIId'
    -> GetDeployments
getDeployments pRestAPIId_ =
  GetDeployments'
    {_gdLimit = Nothing, _gdPosition = Nothing, _gdRestAPIId = pRestAPIId_}


-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gdLimit :: Lens' GetDeployments (Maybe Int)
gdLimit = lens _gdLimit (\ s a -> s{_gdLimit = a})

-- | The current pagination position in the paged result set.
gdPosition :: Lens' GetDeployments (Maybe Text)
gdPosition = lens _gdPosition (\ s a -> s{_gdPosition = a})

-- | [Required] The string identifier of the associated 'RestApi' .
gdRestAPIId :: Lens' GetDeployments Text
gdRestAPIId = lens _gdRestAPIId (\ s a -> s{_gdRestAPIId = a})

instance AWSPager GetDeployments where
        page rq rs
          | stop (rs ^. gdrsPosition) = Nothing
          | stop (rs ^. gdrsItems) = Nothing
          | otherwise =
            Just $ rq & gdPosition .~ rs ^. gdrsPosition

instance AWSRequest GetDeployments where
        type Rs GetDeployments = GetDeploymentsResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentsResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetDeployments where

instance NFData GetDeployments where

instance ToHeaders GetDeployments where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetDeployments where
        toPath GetDeployments'{..}
          = mconcat
              ["/restapis/", toBS _gdRestAPIId, "/deployments"]

instance ToQuery GetDeployments where
        toQuery GetDeployments'{..}
          = mconcat
              ["limit" =: _gdLimit, "position" =: _gdPosition]

-- | Represents a collection resource that contains zero or more references to your existing deployments, and links that guide you on how to interact with your collection. The collection offers a paginated view of the contained deployments.
--
--
-- To create a new deployment of a 'RestApi' , make a @POST@ request against this resource. To view, update, or delete an existing deployment, make a @GET@ , @PATCH@ , or @DELETE@ request, respectively, on a specified 'Deployment' resource.<http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploying an API> , <http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI> , <https://aws.amazon.com/tools/ AWS SDKs>
--
-- /See:/ 'getDeploymentsResponse' smart constructor.
data GetDeploymentsResponse = GetDeploymentsResponse'
  { _gdrsItems          :: !(Maybe [Deployment])
  , _gdrsPosition       :: !(Maybe Text)
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeploymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsItems' - The current page of elements from this collection.
--
-- * 'gdrsPosition' - Undocumented member.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDeploymentsResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDeploymentsResponse
getDeploymentsResponse pResponseStatus_ =
  GetDeploymentsResponse'
    { _gdrsItems = Nothing
    , _gdrsPosition = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
gdrsItems :: Lens' GetDeploymentsResponse [Deployment]
gdrsItems = lens _gdrsItems (\ s a -> s{_gdrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
gdrsPosition :: Lens' GetDeploymentsResponse (Maybe Text)
gdrsPosition = lens _gdrsPosition (\ s a -> s{_gdrsPosition = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDeploymentsResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDeploymentsResponse where
