{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.GetHealthCheckCount
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

-- | To retrieve a count of all your health checks, send a @GET@ request to
-- the @2013-04-01\/healthcheckcount@ resource.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheckCount.html>
module Network.AWS.Route53.GetHealthCheckCount
    (
    -- * Request
      GetHealthCheckCount
    -- ** Request constructor
    , getHealthCheckCount

    -- * Response
    , GetHealthCheckCountResponse
    -- ** Response constructor
    , getHealthCheckCountResponse
    -- ** Response lenses
    , ghccrHealthCheckCount
    , ghccrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | To retrieve a count of all your health checks, send a @GET@ request to
-- the @2013-04-01\/healthcheckcount@ resource.
--
-- /See:/ 'getHealthCheckCount' smart constructor.
data GetHealthCheckCount =
    GetHealthCheckCount'
    deriving (Eq,Read,Show)

-- | 'GetHealthCheckCount' smart constructor.
getHealthCheckCount :: GetHealthCheckCount
getHealthCheckCount = GetHealthCheckCount'

instance AWSRequest GetHealthCheckCount where
        type Sv GetHealthCheckCount = Route53
        type Rs GetHealthCheckCount =
             GetHealthCheckCountResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetHealthCheckCountResponse' <$>
                   (x .@ "HealthCheckCount") <*> (pure s))

instance ToHeaders GetHealthCheckCount where
        toHeaders = const mempty

instance ToPath GetHealthCheckCount where
        toPath = const "/2013-04-01/healthcheckcount"

instance ToQuery GetHealthCheckCount where
        toQuery = const mempty

-- | A complex type that contains the count of health checks associated with
-- the current AWS account.
--
-- /See:/ 'getHealthCheckCountResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghccrHealthCheckCount'
--
-- * 'ghccrStatus'
data GetHealthCheckCountResponse = GetHealthCheckCountResponse'
    { _ghccrHealthCheckCount :: !Integer
    , _ghccrStatus           :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetHealthCheckCountResponse' smart constructor.
getHealthCheckCountResponse :: Integer -> Status -> GetHealthCheckCountResponse
getHealthCheckCountResponse pHealthCheckCount pStatus =
    GetHealthCheckCountResponse'
    { _ghccrHealthCheckCount = pHealthCheckCount
    , _ghccrStatus = pStatus
    }

-- | The number of health checks associated with the current AWS account.
ghccrHealthCheckCount :: Lens' GetHealthCheckCountResponse Integer
ghccrHealthCheckCount = lens _ghccrHealthCheckCount (\ s a -> s{_ghccrHealthCheckCount = a});

-- | FIXME: Undocumented member.
ghccrStatus :: Lens' GetHealthCheckCountResponse Status
ghccrStatus = lens _ghccrStatus (\ s a -> s{_ghccrStatus = a});
