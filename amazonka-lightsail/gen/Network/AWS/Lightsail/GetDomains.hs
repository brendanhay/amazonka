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
-- Module      : Network.AWS.Lightsail.GetDomains
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all domains in the user's account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDomains
    (
    -- * Creating a Request
      getDomains
    , GetDomains
    -- * Request Lenses
    , gdPageToken

    -- * Destructuring the Response
    , getDomainsResponse
    , GetDomainsResponse
    -- * Response Lenses
    , ggrsNextPageToken
    , ggrsDomains
    , ggrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDomains' smart constructor.
newtype GetDomains = GetDomains'
  { _gdPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomains' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdPageToken' - A token used for advancing to the next page of results from your get domains request.
getDomains
    :: GetDomains
getDomains = GetDomains' {_gdPageToken = Nothing}


-- | A token used for advancing to the next page of results from your get domains request.
gdPageToken :: Lens' GetDomains (Maybe Text)
gdPageToken = lens _gdPageToken (\ s a -> s{_gdPageToken = a})

instance AWSPager GetDomains where
        page rq rs
          | stop (rs ^. ggrsNextPageToken) = Nothing
          | stop (rs ^. ggrsDomains) = Nothing
          | otherwise =
            Just $ rq & gdPageToken .~ rs ^. ggrsNextPageToken

instance AWSRequest GetDomains where
        type Rs GetDomains = GetDomainsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetDomainsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "domains" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetDomains where

instance NFData GetDomains where

instance ToHeaders GetDomains where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetDomains" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDomains where
        toJSON GetDomains'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _gdPageToken])

instance ToPath GetDomains where
        toPath = const "/"

instance ToQuery GetDomains where
        toQuery = const mempty

-- | /See:/ 'getDomainsResponse' smart constructor.
data GetDomainsResponse = GetDomainsResponse'
  { _ggrsNextPageToken  :: !(Maybe Text)
  , _ggrsDomains        :: !(Maybe [Domain])
  , _ggrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsNextPageToken' - A token used for advancing to the next page of results from your get active names request.
--
-- * 'ggrsDomains' - An array of key-value pairs containing information about each of the domain entries in the user's account.
--
-- * 'ggrsResponseStatus' - -- | The response status code.
getDomainsResponse
    :: Int -- ^ 'ggrsResponseStatus'
    -> GetDomainsResponse
getDomainsResponse pResponseStatus_ =
  GetDomainsResponse'
    { _ggrsNextPageToken = Nothing
    , _ggrsDomains = Nothing
    , _ggrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get active names request.
ggrsNextPageToken :: Lens' GetDomainsResponse (Maybe Text)
ggrsNextPageToken = lens _ggrsNextPageToken (\ s a -> s{_ggrsNextPageToken = a})

-- | An array of key-value pairs containing information about each of the domain entries in the user's account.
ggrsDomains :: Lens' GetDomainsResponse [Domain]
ggrsDomains = lens _ggrsDomains (\ s a -> s{_ggrsDomains = a}) . _Default . _Coerce

-- | -- | The response status code.
ggrsResponseStatus :: Lens' GetDomainsResponse Int
ggrsResponseStatus = lens _ggrsResponseStatus (\ s a -> s{_ggrsResponseStatus = a})

instance NFData GetDomainsResponse where
