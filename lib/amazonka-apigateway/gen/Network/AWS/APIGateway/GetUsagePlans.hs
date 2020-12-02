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
-- Module      : Network.AWS.APIGateway.GetUsagePlans
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the usage plans of the caller's account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetUsagePlans
    (
    -- * Creating a Request
      getUsagePlans
    , GetUsagePlans
    -- * Request Lenses
    , gupKeyId
    , gupLimit
    , gupPosition

    -- * Destructuring the Response
    , getUsagePlansResponse
    , GetUsagePlansResponse
    -- * Response Lenses
    , guprsItems
    , guprsPosition
    , guprsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The GET request to get all the usage plans of the caller's account.
--
--
--
-- /See:/ 'getUsagePlans' smart constructor.
data GetUsagePlans = GetUsagePlans'
  { _gupKeyId    :: !(Maybe Text)
  , _gupLimit    :: !(Maybe Int)
  , _gupPosition :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUsagePlans' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gupKeyId' - The identifier of the API key associated with the usage plans.
--
-- * 'gupLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gupPosition' - The current pagination position in the paged result set.
getUsagePlans
    :: GetUsagePlans
getUsagePlans =
  GetUsagePlans'
    {_gupKeyId = Nothing, _gupLimit = Nothing, _gupPosition = Nothing}


-- | The identifier of the API key associated with the usage plans.
gupKeyId :: Lens' GetUsagePlans (Maybe Text)
gupKeyId = lens _gupKeyId (\ s a -> s{_gupKeyId = a})

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gupLimit :: Lens' GetUsagePlans (Maybe Int)
gupLimit = lens _gupLimit (\ s a -> s{_gupLimit = a})

-- | The current pagination position in the paged result set.
gupPosition :: Lens' GetUsagePlans (Maybe Text)
gupPosition = lens _gupPosition (\ s a -> s{_gupPosition = a})

instance AWSPager GetUsagePlans where
        page rq rs
          | stop (rs ^. guprsPosition) = Nothing
          | stop (rs ^. guprsItems) = Nothing
          | otherwise =
            Just $ rq & gupPosition .~ rs ^. guprsPosition

instance AWSRequest GetUsagePlans where
        type Rs GetUsagePlans = GetUsagePlansResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetUsagePlansResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetUsagePlans where

instance NFData GetUsagePlans where

instance ToHeaders GetUsagePlans where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetUsagePlans where
        toPath = const "/usageplans"

instance ToQuery GetUsagePlans where
        toQuery GetUsagePlans'{..}
          = mconcat
              ["keyId" =: _gupKeyId, "limit" =: _gupLimit,
               "position" =: _gupPosition]

-- | Represents a collection of usage plans for an AWS account.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'getUsagePlansResponse' smart constructor.
data GetUsagePlansResponse = GetUsagePlansResponse'
  { _guprsItems          :: !(Maybe [UsagePlan])
  , _guprsPosition       :: !(Maybe Text)
  , _guprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUsagePlansResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guprsItems' - The current page of elements from this collection.
--
-- * 'guprsPosition' - Undocumented member.
--
-- * 'guprsResponseStatus' - -- | The response status code.
getUsagePlansResponse
    :: Int -- ^ 'guprsResponseStatus'
    -> GetUsagePlansResponse
getUsagePlansResponse pResponseStatus_ =
  GetUsagePlansResponse'
    { _guprsItems = Nothing
    , _guprsPosition = Nothing
    , _guprsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
guprsItems :: Lens' GetUsagePlansResponse [UsagePlan]
guprsItems = lens _guprsItems (\ s a -> s{_guprsItems = a}) . _Default . _Coerce

-- | Undocumented member.
guprsPosition :: Lens' GetUsagePlansResponse (Maybe Text)
guprsPosition = lens _guprsPosition (\ s a -> s{_guprsPosition = a})

-- | -- | The response status code.
guprsResponseStatus :: Lens' GetUsagePlansResponse Int
guprsResponseStatus = lens _guprsResponseStatus (\ s a -> s{_guprsResponseStatus = a})

instance NFData GetUsagePlansResponse where
