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
-- Module      : Network.AWS.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified health check.
--
--
module Network.AWS.Route53.GetHealthCheck
    (
    -- * Creating a Request
      getHealthCheck
    , GetHealthCheck
    -- * Request Lenses
    , ghcHealthCheckId

    -- * Destructuring the Response
    , getHealthCheckResponse
    , GetHealthCheckResponse
    -- * Response Lenses
    , ghcrsResponseStatus
    , ghcrsHealthCheck
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to get information about a specified health check.
--
--
--
-- /See:/ 'getHealthCheck' smart constructor.
newtype GetHealthCheck = GetHealthCheck'
  { _ghcHealthCheckId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghcHealthCheckId' - The identifier that Amazon Route 53 assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
getHealthCheck
    :: Text -- ^ 'ghcHealthCheckId'
    -> GetHealthCheck
getHealthCheck pHealthCheckId_ =
  GetHealthCheck' {_ghcHealthCheckId = pHealthCheckId_}


-- | The identifier that Amazon Route 53 assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
ghcHealthCheckId :: Lens' GetHealthCheck Text
ghcHealthCheckId = lens _ghcHealthCheckId (\ s a -> s{_ghcHealthCheckId = a})

instance AWSRequest GetHealthCheck where
        type Rs GetHealthCheck = GetHealthCheckResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetHealthCheckResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HealthCheck"))

instance Hashable GetHealthCheck where

instance NFData GetHealthCheck where

instance ToHeaders GetHealthCheck where
        toHeaders = const mempty

instance ToPath GetHealthCheck where
        toPath GetHealthCheck'{..}
          = mconcat
              ["/2013-04-01/healthcheck/", toBS _ghcHealthCheckId]

instance ToQuery GetHealthCheck where
        toQuery = const mempty

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
--
--
-- /See:/ 'getHealthCheckResponse' smart constructor.
data GetHealthCheckResponse = GetHealthCheckResponse'
  { _ghcrsResponseStatus :: !Int
  , _ghcrsHealthCheck    :: !HealthCheck
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghcrsResponseStatus' - -- | The response status code.
--
-- * 'ghcrsHealthCheck' - A complex type that contains information about one health check that is associated with the current AWS account.
getHealthCheckResponse
    :: Int -- ^ 'ghcrsResponseStatus'
    -> HealthCheck -- ^ 'ghcrsHealthCheck'
    -> GetHealthCheckResponse
getHealthCheckResponse pResponseStatus_ pHealthCheck_ =
  GetHealthCheckResponse'
    {_ghcrsResponseStatus = pResponseStatus_, _ghcrsHealthCheck = pHealthCheck_}


-- | -- | The response status code.
ghcrsResponseStatus :: Lens' GetHealthCheckResponse Int
ghcrsResponseStatus = lens _ghcrsResponseStatus (\ s a -> s{_ghcrsResponseStatus = a})

-- | A complex type that contains information about one health check that is associated with the current AWS account.
ghcrsHealthCheck :: Lens' GetHealthCheckResponse HealthCheck
ghcrsHealthCheck = lens _ghcrsHealthCheck (\ s a -> s{_ghcrsHealthCheck = a})

instance NFData GetHealthCheckResponse where
