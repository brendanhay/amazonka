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
-- Module      : Network.AWS.Glue.GetDevEndpoints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all the DevEndpoints in this AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetDevEndpoints
    (
    -- * Creating a Request
      getDevEndpoints
    , GetDevEndpoints
    -- * Request Lenses
    , gdeNextToken
    , gdeMaxResults

    -- * Destructuring the Response
    , getDevEndpointsResponse
    , GetDevEndpointsResponse
    -- * Response Lenses
    , gdersNextToken
    , gdersDevEndpoints
    , gdersResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDevEndpoints' smart constructor.
data GetDevEndpoints = GetDevEndpoints'
  { _gdeNextToken  :: !(Maybe Text)
  , _gdeMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDevEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdeNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gdeMaxResults' - The maximum size of information to return.
getDevEndpoints
    :: GetDevEndpoints
getDevEndpoints =
  GetDevEndpoints' {_gdeNextToken = Nothing, _gdeMaxResults = Nothing}


-- | A continuation token, if this is a continuation call.
gdeNextToken :: Lens' GetDevEndpoints (Maybe Text)
gdeNextToken = lens _gdeNextToken (\ s a -> s{_gdeNextToken = a})

-- | The maximum size of information to return.
gdeMaxResults :: Lens' GetDevEndpoints (Maybe Natural)
gdeMaxResults = lens _gdeMaxResults (\ s a -> s{_gdeMaxResults = a}) . mapping _Nat

instance AWSPager GetDevEndpoints where
        page rq rs
          | stop (rs ^. gdersNextToken) = Nothing
          | stop (rs ^. gdersDevEndpoints) = Nothing
          | otherwise =
            Just $ rq & gdeNextToken .~ rs ^. gdersNextToken

instance AWSRequest GetDevEndpoints where
        type Rs GetDevEndpoints = GetDevEndpointsResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetDevEndpointsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "DevEndpoints" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetDevEndpoints where

instance NFData GetDevEndpoints where

instance ToHeaders GetDevEndpoints where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetDevEndpoints" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDevEndpoints where
        toJSON GetDevEndpoints'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gdeNextToken,
                  ("MaxResults" .=) <$> _gdeMaxResults])

instance ToPath GetDevEndpoints where
        toPath = const "/"

instance ToQuery GetDevEndpoints where
        toQuery = const mempty

-- | /See:/ 'getDevEndpointsResponse' smart constructor.
data GetDevEndpointsResponse = GetDevEndpointsResponse'
  { _gdersNextToken      :: !(Maybe Text)
  , _gdersDevEndpoints   :: !(Maybe [DevEndpoint])
  , _gdersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDevEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdersNextToken' - A continuation token, if not all DevEndpoint definitions have yet been returned.
--
-- * 'gdersDevEndpoints' - A list of DevEndpoint definitions.
--
-- * 'gdersResponseStatus' - -- | The response status code.
getDevEndpointsResponse
    :: Int -- ^ 'gdersResponseStatus'
    -> GetDevEndpointsResponse
getDevEndpointsResponse pResponseStatus_ =
  GetDevEndpointsResponse'
    { _gdersNextToken = Nothing
    , _gdersDevEndpoints = Nothing
    , _gdersResponseStatus = pResponseStatus_
    }


-- | A continuation token, if not all DevEndpoint definitions have yet been returned.
gdersNextToken :: Lens' GetDevEndpointsResponse (Maybe Text)
gdersNextToken = lens _gdersNextToken (\ s a -> s{_gdersNextToken = a})

-- | A list of DevEndpoint definitions.
gdersDevEndpoints :: Lens' GetDevEndpointsResponse [DevEndpoint]
gdersDevEndpoints = lens _gdersDevEndpoints (\ s a -> s{_gdersDevEndpoints = a}) . _Default . _Coerce

-- | -- | The response status code.
gdersResponseStatus :: Lens' GetDevEndpointsResponse Int
gdersResponseStatus = lens _gdersResponseStatus (\ s a -> s{_gdersResponseStatus = a})

instance NFData GetDevEndpointsResponse where
