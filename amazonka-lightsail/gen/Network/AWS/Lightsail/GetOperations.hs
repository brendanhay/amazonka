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
-- Module      : Network.AWS.Lightsail.GetOperations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all operations.
--
--
-- Results are returned from oldest to newest, up to a maximum of 200. Results can be paged by making each subsequent call to @GetOperations@ use the maximum (last) @statusChangedAt@ value from the previous request.
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetOperations
    (
    -- * Creating a Request
      getOperations
    , GetOperations
    -- * Request Lenses
    , goPageToken

    -- * Destructuring the Response
    , getOperationsResponse
    , GetOperationsResponse
    -- * Response Lenses
    , gosrsNextPageToken
    , gosrsOperations
    , gosrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOperations' smart constructor.
newtype GetOperations = GetOperations'
  { _goPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goPageToken' - A token used for advancing to the next page of results from your get operations request.
getOperations
    :: GetOperations
getOperations = GetOperations' {_goPageToken = Nothing}


-- | A token used for advancing to the next page of results from your get operations request.
goPageToken :: Lens' GetOperations (Maybe Text)
goPageToken = lens _goPageToken (\ s a -> s{_goPageToken = a})

instance AWSPager GetOperations where
        page rq rs
          | stop (rs ^. gosrsNextPageToken) = Nothing
          | stop (rs ^. gosrsOperations) = Nothing
          | otherwise =
            Just $ rq & goPageToken .~ rs ^. gosrsNextPageToken

instance AWSRequest GetOperations where
        type Rs GetOperations = GetOperationsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetOperationsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "operations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetOperations where

instance NFData GetOperations where

instance ToHeaders GetOperations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetOperations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOperations where
        toJSON GetOperations'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _goPageToken])

instance ToPath GetOperations where
        toPath = const "/"

instance ToQuery GetOperations where
        toQuery = const mempty

-- | /See:/ 'getOperationsResponse' smart constructor.
data GetOperationsResponse = GetOperationsResponse'
  { _gosrsNextPageToken  :: !(Maybe Text)
  , _gosrsOperations     :: !(Maybe [Operation])
  , _gosrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosrsNextPageToken' - A token used for advancing to the next page of results from your get operations request.
--
-- * 'gosrsOperations' - An array of key-value pairs containing information about the results of your get operations request.
--
-- * 'gosrsResponseStatus' - -- | The response status code.
getOperationsResponse
    :: Int -- ^ 'gosrsResponseStatus'
    -> GetOperationsResponse
getOperationsResponse pResponseStatus_ =
  GetOperationsResponse'
    { _gosrsNextPageToken = Nothing
    , _gosrsOperations = Nothing
    , _gosrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get operations request.
gosrsNextPageToken :: Lens' GetOperationsResponse (Maybe Text)
gosrsNextPageToken = lens _gosrsNextPageToken (\ s a -> s{_gosrsNextPageToken = a})

-- | An array of key-value pairs containing information about the results of your get operations request.
gosrsOperations :: Lens' GetOperationsResponse [Operation]
gosrsOperations = lens _gosrsOperations (\ s a -> s{_gosrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
gosrsResponseStatus :: Lens' GetOperationsResponse Int
gosrsResponseStatus = lens _gosrsResponseStatus (\ s a -> s{_gosrsResponseStatus = a})

instance NFData GetOperationsResponse where
