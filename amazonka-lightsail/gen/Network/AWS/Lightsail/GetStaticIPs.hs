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
-- Module      : Network.AWS.Lightsail.GetStaticIPs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all static IPs in the user's account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetStaticIPs
    (
    -- * Creating a Request
      getStaticIPs
    , GetStaticIPs
    -- * Request Lenses
    , gsiPageToken

    -- * Destructuring the Response
    , getStaticIPsResponse
    , GetStaticIPsResponse
    -- * Response Lenses
    , gsiprsNextPageToken
    , gsiprsStaticIPs
    , gsiprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getStaticIPs' smart constructor.
newtype GetStaticIPs = GetStaticIPs'
  { _gsiPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStaticIPs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiPageToken' - A token used for advancing to the next page of results from your get static IPs request.
getStaticIPs
    :: GetStaticIPs
getStaticIPs = GetStaticIPs' {_gsiPageToken = Nothing}


-- | A token used for advancing to the next page of results from your get static IPs request.
gsiPageToken :: Lens' GetStaticIPs (Maybe Text)
gsiPageToken = lens _gsiPageToken (\ s a -> s{_gsiPageToken = a})

instance AWSPager GetStaticIPs where
        page rq rs
          | stop (rs ^. gsiprsNextPageToken) = Nothing
          | stop (rs ^. gsiprsStaticIPs) = Nothing
          | otherwise =
            Just $ rq & gsiPageToken .~ rs ^. gsiprsNextPageToken

instance AWSRequest GetStaticIPs where
        type Rs GetStaticIPs = GetStaticIPsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetStaticIPsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "staticIps" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetStaticIPs where

instance NFData GetStaticIPs where

instance ToHeaders GetStaticIPs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetStaticIps" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetStaticIPs where
        toJSON GetStaticIPs'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _gsiPageToken])

instance ToPath GetStaticIPs where
        toPath = const "/"

instance ToQuery GetStaticIPs where
        toQuery = const mempty

-- | /See:/ 'getStaticIPsResponse' smart constructor.
data GetStaticIPsResponse = GetStaticIPsResponse'
  { _gsiprsNextPageToken  :: !(Maybe Text)
  , _gsiprsStaticIPs      :: !(Maybe [StaticIP])
  , _gsiprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStaticIPsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiprsNextPageToken' - A token used for advancing to the next page of results from your get static IPs request.
--
-- * 'gsiprsStaticIPs' - An array of key-value pairs containing information about your get static IPs request.
--
-- * 'gsiprsResponseStatus' - -- | The response status code.
getStaticIPsResponse
    :: Int -- ^ 'gsiprsResponseStatus'
    -> GetStaticIPsResponse
getStaticIPsResponse pResponseStatus_ =
  GetStaticIPsResponse'
    { _gsiprsNextPageToken = Nothing
    , _gsiprsStaticIPs = Nothing
    , _gsiprsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get static IPs request.
gsiprsNextPageToken :: Lens' GetStaticIPsResponse (Maybe Text)
gsiprsNextPageToken = lens _gsiprsNextPageToken (\ s a -> s{_gsiprsNextPageToken = a})

-- | An array of key-value pairs containing information about your get static IPs request.
gsiprsStaticIPs :: Lens' GetStaticIPsResponse [StaticIP]
gsiprsStaticIPs = lens _gsiprsStaticIPs (\ s a -> s{_gsiprsStaticIPs = a}) . _Default . _Coerce

-- | -- | The response status code.
gsiprsResponseStatus :: Lens' GetStaticIPsResponse Int
gsiprsResponseStatus = lens _gsiprsResponseStatus (\ s a -> s{_gsiprsResponseStatus = a})

instance NFData GetStaticIPsResponse where
