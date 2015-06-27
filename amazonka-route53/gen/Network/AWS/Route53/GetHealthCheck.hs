{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | To retrieve the health check, send a @GET@ request to the
-- @2013-04-01\/healthcheck\/health check ID@ resource.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheck.html>
module Network.AWS.Route53.GetHealthCheck
    (
    -- * Request
      GetHealthCheck
    -- ** Request constructor
    , getHealthCheck
    -- ** Request lenses
    , ghcHealthCheckId

    -- * Response
    , GetHealthCheckResponse
    -- ** Response constructor
    , getHealthCheckResponse
    -- ** Response lenses
    , ghcrHealthCheck
    , ghcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type that contains information about the request to get a
-- health check.
--
-- /See:/ 'getHealthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcHealthCheckId'
newtype GetHealthCheck = GetHealthCheck'
    { _ghcHealthCheckId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetHealthCheck' smart constructor.
getHealthCheck :: Text -> GetHealthCheck
getHealthCheck pHealthCheckId =
    GetHealthCheck'
    { _ghcHealthCheckId = pHealthCheckId
    }

-- | The ID of the health check to retrieve.
ghcHealthCheckId :: Lens' GetHealthCheck Text
ghcHealthCheckId = lens _ghcHealthCheckId (\ s a -> s{_ghcHealthCheckId = a});

instance AWSRequest GetHealthCheck where
        type Sv GetHealthCheck = Route53
        type Rs GetHealthCheck = GetHealthCheckResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetHealthCheckResponse' <$>
                   (x .@ "HealthCheck") <*> (pure (fromEnum s)))

instance ToHeaders GetHealthCheck where
        toHeaders = const mempty

instance ToPath GetHealthCheck where
        toPath GetHealthCheck'{..}
          = mconcat
              ["/2013-04-01/healthcheck/",
               toText _ghcHealthCheckId]

instance ToQuery GetHealthCheck where
        toQuery = const mempty

-- | A complex type containing information about the specified health check.
--
-- /See:/ 'getHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcrHealthCheck'
--
-- * 'ghcrStatus'
data GetHealthCheckResponse = GetHealthCheckResponse'
    { _ghcrHealthCheck :: !HealthCheck
    , _ghcrStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetHealthCheckResponse' smart constructor.
getHealthCheckResponse :: HealthCheck -> Int -> GetHealthCheckResponse
getHealthCheckResponse pHealthCheck pStatus =
    GetHealthCheckResponse'
    { _ghcrHealthCheck = pHealthCheck
    , _ghcrStatus = pStatus
    }

-- | A complex type that contains the information about the specified health
-- check.
ghcrHealthCheck :: Lens' GetHealthCheckResponse HealthCheck
ghcrHealthCheck = lens _ghcrHealthCheck (\ s a -> s{_ghcrHealthCheck = a});

-- | FIXME: Undocumented member.
ghcrStatus :: Lens' GetHealthCheckResponse Int
ghcrStatus = lens _ghcrStatus (\ s a -> s{_ghcrStatus = a});
