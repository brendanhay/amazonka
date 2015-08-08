{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- To retrieve the health check, send a @GET@ request to the
-- @2013-04-01\/healthcheck\/health check ID@ resource.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheck.html AWS API Reference> for GetHealthCheck.
module Network.AWS.Route53.GetHealthCheck
    (
    -- * Creating a Request
      GetHealthCheck
    , getHealthCheck
    -- * Request Lenses
    , ghcHealthCheckId

    -- * Destructuring the Response
    , GetHealthCheckResponse
    , getHealthCheckResponse
    -- * Response Lenses
    , ghcrsStatus
    , ghcrsHealthCheck
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHealthCheck' smart constructor.
getHealthCheck :: Text -> GetHealthCheck
getHealthCheck pHealthCheckId_ =
    GetHealthCheck'
    { _ghcHealthCheckId = pHealthCheckId_
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
                   (pure (fromEnum s)) <*> (x .@ "HealthCheck"))

instance ToHeaders GetHealthCheck where
        toHeaders = const mempty

instance ToPath GetHealthCheck where
        toPath GetHealthCheck'{..}
          = mconcat
              ["/2013-04-01/healthcheck/", toBS _ghcHealthCheckId]

instance ToQuery GetHealthCheck where
        toQuery = const mempty

-- | A complex type containing information about the specified health check.
--
-- /See:/ 'getHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcrsStatus'
--
-- * 'ghcrsHealthCheck'
data GetHealthCheckResponse = GetHealthCheckResponse'
    { _ghcrsStatus      :: !Int
    , _ghcrsHealthCheck :: !HealthCheck
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHealthCheckResponse' smart constructor.
getHealthCheckResponse :: Int -> HealthCheck -> GetHealthCheckResponse
getHealthCheckResponse pStatus_ pHealthCheck_ =
    GetHealthCheckResponse'
    { _ghcrsStatus = pStatus_
    , _ghcrsHealthCheck = pHealthCheck_
    }

-- | Undocumented member.
ghcrsStatus :: Lens' GetHealthCheckResponse Int
ghcrsStatus = lens _ghcrsStatus (\ s a -> s{_ghcrsStatus = a});

-- | A complex type that contains the information about the specified health
-- check.
ghcrsHealthCheck :: Lens' GetHealthCheckResponse HealthCheck
ghcrsHealthCheck = lens _ghcrsHealthCheck (\ s a -> s{_ghcrsHealthCheck = a});
