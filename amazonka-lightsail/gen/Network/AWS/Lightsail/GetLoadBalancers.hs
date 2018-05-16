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
-- Module      : Network.AWS.Lightsail.GetLoadBalancers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all load balancers in an account.
--
--
-- If you are describing a long list of load balancers, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.
--
module Network.AWS.Lightsail.GetLoadBalancers
    (
    -- * Creating a Request
      getLoadBalancers
    , GetLoadBalancers
    -- * Request Lenses
    , glbPageToken

    -- * Destructuring the Response
    , getLoadBalancersResponse
    , GetLoadBalancersResponse
    -- * Response Lenses
    , glbsrsNextPageToken
    , glbsrsLoadBalancers
    , glbsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLoadBalancers' smart constructor.
newtype GetLoadBalancers = GetLoadBalancers'
  { _glbPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoadBalancers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glbPageToken' - A token used for paginating the results from your GetLoadBalancers request.
getLoadBalancers
    :: GetLoadBalancers
getLoadBalancers = GetLoadBalancers' {_glbPageToken = Nothing}


-- | A token used for paginating the results from your GetLoadBalancers request.
glbPageToken :: Lens' GetLoadBalancers (Maybe Text)
glbPageToken = lens _glbPageToken (\ s a -> s{_glbPageToken = a})

instance AWSRequest GetLoadBalancers where
        type Rs GetLoadBalancers = GetLoadBalancersResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetLoadBalancersResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "loadBalancers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetLoadBalancers where

instance NFData GetLoadBalancers where

instance ToHeaders GetLoadBalancers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetLoadBalancers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLoadBalancers where
        toJSON GetLoadBalancers'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _glbPageToken])

instance ToPath GetLoadBalancers where
        toPath = const "/"

instance ToQuery GetLoadBalancers where
        toQuery = const mempty

-- | /See:/ 'getLoadBalancersResponse' smart constructor.
data GetLoadBalancersResponse = GetLoadBalancersResponse'
  { _glbsrsNextPageToken  :: !(Maybe Text)
  , _glbsrsLoadBalancers  :: !(Maybe [LoadBalancer])
  , _glbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoadBalancersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glbsrsNextPageToken' - A token used for advancing to the next page of results from your GetLoadBalancers request.
--
-- * 'glbsrsLoadBalancers' - An array of LoadBalancer objects describing your load balancers.
--
-- * 'glbsrsResponseStatus' - -- | The response status code.
getLoadBalancersResponse
    :: Int -- ^ 'glbsrsResponseStatus'
    -> GetLoadBalancersResponse
getLoadBalancersResponse pResponseStatus_ =
  GetLoadBalancersResponse'
    { _glbsrsNextPageToken = Nothing
    , _glbsrsLoadBalancers = Nothing
    , _glbsrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your GetLoadBalancers request.
glbsrsNextPageToken :: Lens' GetLoadBalancersResponse (Maybe Text)
glbsrsNextPageToken = lens _glbsrsNextPageToken (\ s a -> s{_glbsrsNextPageToken = a})

-- | An array of LoadBalancer objects describing your load balancers.
glbsrsLoadBalancers :: Lens' GetLoadBalancersResponse [LoadBalancer]
glbsrsLoadBalancers = lens _glbsrsLoadBalancers (\ s a -> s{_glbsrsLoadBalancers = a}) . _Default . _Coerce

-- | -- | The response status code.
glbsrsResponseStatus :: Lens' GetLoadBalancersResponse Int
glbsrsResponseStatus = lens _glbsrsResponseStatus (\ s a -> s{_glbsrsResponseStatus = a})

instance NFData GetLoadBalancersResponse where
