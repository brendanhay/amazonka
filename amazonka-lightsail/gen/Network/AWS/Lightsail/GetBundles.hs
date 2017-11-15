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
-- Module      : Network.AWS.Lightsail.GetBundles
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available for purchase. A bundle describes the specs for your virtual private server (or /instance/ ).
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetBundles
    (
    -- * Creating a Request
      getBundles
    , GetBundles
    -- * Request Lenses
    , gIncludeInactive
    , gPageToken

    -- * Destructuring the Response
    , getBundlesResponse
    , GetBundlesResponse
    -- * Response Lenses
    , gbrsNextPageToken
    , gbrsBundles
    , gbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBundles' smart constructor.
data GetBundles = GetBundles'
  { _gIncludeInactive :: !(Maybe Bool)
  , _gPageToken       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBundles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gIncludeInactive' - A Boolean value that indicates whether to include inactive bundle results in your request.
--
-- * 'gPageToken' - A token used for advancing to the next page of results from your get bundles request.
getBundles
    :: GetBundles
getBundles = GetBundles' {_gIncludeInactive = Nothing, _gPageToken = Nothing}


-- | A Boolean value that indicates whether to include inactive bundle results in your request.
gIncludeInactive :: Lens' GetBundles (Maybe Bool)
gIncludeInactive = lens _gIncludeInactive (\ s a -> s{_gIncludeInactive = a});

-- | A token used for advancing to the next page of results from your get bundles request.
gPageToken :: Lens' GetBundles (Maybe Text)
gPageToken = lens _gPageToken (\ s a -> s{_gPageToken = a});

instance AWSPager GetBundles where
        page rq rs
          | stop (rs ^. gbrsNextPageToken) = Nothing
          | stop (rs ^. gbrsBundles) = Nothing
          | otherwise =
            Just $ rq & gPageToken .~ rs ^. gbrsNextPageToken

instance AWSRequest GetBundles where
        type Rs GetBundles = GetBundlesResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetBundlesResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "bundles" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetBundles where

instance NFData GetBundles where

instance ToHeaders GetBundles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetBundles" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetBundles where
        toJSON GetBundles'{..}
          = object
              (catMaybes
                 [("includeInactive" .=) <$> _gIncludeInactive,
                  ("pageToken" .=) <$> _gPageToken])

instance ToPath GetBundles where
        toPath = const "/"

instance ToQuery GetBundles where
        toQuery = const mempty

-- | /See:/ 'getBundlesResponse' smart constructor.
data GetBundlesResponse = GetBundlesResponse'
  { _gbrsNextPageToken  :: !(Maybe Text)
  , _gbrsBundles        :: !(Maybe [Bundle])
  , _gbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBundlesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrsNextPageToken' - A token used for advancing to the next page of results from your get active names request.
--
-- * 'gbrsBundles' - An array of key-value pairs that contains information about the available bundles.
--
-- * 'gbrsResponseStatus' - -- | The response status code.
getBundlesResponse
    :: Int -- ^ 'gbrsResponseStatus'
    -> GetBundlesResponse
getBundlesResponse pResponseStatus_ =
  GetBundlesResponse'
  { _gbrsNextPageToken = Nothing
  , _gbrsBundles = Nothing
  , _gbrsResponseStatus = pResponseStatus_
  }


-- | A token used for advancing to the next page of results from your get active names request.
gbrsNextPageToken :: Lens' GetBundlesResponse (Maybe Text)
gbrsNextPageToken = lens _gbrsNextPageToken (\ s a -> s{_gbrsNextPageToken = a});

-- | An array of key-value pairs that contains information about the available bundles.
gbrsBundles :: Lens' GetBundlesResponse [Bundle]
gbrsBundles = lens _gbrsBundles (\ s a -> s{_gbrsBundles = a}) . _Default . _Coerce;

-- | -- | The response status code.
gbrsResponseStatus :: Lens' GetBundlesResponse Int
gbrsResponseStatus = lens _gbrsResponseStatus (\ s a -> s{_gbrsResponseStatus = a});

instance NFData GetBundlesResponse where
