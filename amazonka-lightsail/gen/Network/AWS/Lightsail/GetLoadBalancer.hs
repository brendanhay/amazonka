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
-- Module      : Network.AWS.Lightsail.GetLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Lightsail load balancer.
--
--
module Network.AWS.Lightsail.GetLoadBalancer
    (
    -- * Creating a Request
      getLoadBalancer
    , GetLoadBalancer
    -- * Request Lenses
    , glbLoadBalancerName

    -- * Destructuring the Response
    , getLoadBalancerResponse
    , GetLoadBalancerResponse
    -- * Response Lenses
    , glbrsLoadBalancer
    , glbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLoadBalancer' smart constructor.
newtype GetLoadBalancer = GetLoadBalancer'
  { _glbLoadBalancerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glbLoadBalancerName' - The name of the load balancer.
getLoadBalancer
    :: Text -- ^ 'glbLoadBalancerName'
    -> GetLoadBalancer
getLoadBalancer pLoadBalancerName_ =
  GetLoadBalancer' {_glbLoadBalancerName = pLoadBalancerName_}


-- | The name of the load balancer.
glbLoadBalancerName :: Lens' GetLoadBalancer Text
glbLoadBalancerName = lens _glbLoadBalancerName (\ s a -> s{_glbLoadBalancerName = a})

instance AWSRequest GetLoadBalancer where
        type Rs GetLoadBalancer = GetLoadBalancerResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetLoadBalancerResponse' <$>
                   (x .?> "loadBalancer") <*> (pure (fromEnum s)))

instance Hashable GetLoadBalancer where

instance NFData GetLoadBalancer where

instance ToHeaders GetLoadBalancer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetLoadBalancer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLoadBalancer where
        toJSON GetLoadBalancer'{..}
          = object
              (catMaybes
                 [Just ("loadBalancerName" .= _glbLoadBalancerName)])

instance ToPath GetLoadBalancer where
        toPath = const "/"

instance ToQuery GetLoadBalancer where
        toQuery = const mempty

-- | /See:/ 'getLoadBalancerResponse' smart constructor.
data GetLoadBalancerResponse = GetLoadBalancerResponse'
  { _glbrsLoadBalancer   :: !(Maybe LoadBalancer)
  , _glbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glbrsLoadBalancer' - An object containing information about your load balancer.
--
-- * 'glbrsResponseStatus' - -- | The response status code.
getLoadBalancerResponse
    :: Int -- ^ 'glbrsResponseStatus'
    -> GetLoadBalancerResponse
getLoadBalancerResponse pResponseStatus_ =
  GetLoadBalancerResponse'
    {_glbrsLoadBalancer = Nothing, _glbrsResponseStatus = pResponseStatus_}


-- | An object containing information about your load balancer.
glbrsLoadBalancer :: Lens' GetLoadBalancerResponse (Maybe LoadBalancer)
glbrsLoadBalancer = lens _glbrsLoadBalancer (\ s a -> s{_glbrsLoadBalancer = a})

-- | -- | The response status code.
glbrsResponseStatus :: Lens' GetLoadBalancerResponse Int
glbrsResponseStatus = lens _glbrsResponseStatus (\ s a -> s{_glbrsResponseStatus = a})

instance NFData GetLoadBalancerResponse where
