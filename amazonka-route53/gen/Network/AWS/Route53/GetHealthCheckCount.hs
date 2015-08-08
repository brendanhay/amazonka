{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckCount
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a count of all your health checks, send a @GET@ request to
-- the @2013-04-01\/healthcheckcount@ resource.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheckCount.html AWS API Reference> for GetHealthCheckCount.
module Network.AWS.Route53.GetHealthCheckCount
    (
    -- * Creating a Request
      GetHealthCheckCount
    , getHealthCheckCount

    -- * Destructuring the Response
    , GetHealthCheckCountResponse
    , getHealthCheckCountResponse
    -- * Response Lenses
    , ghccrsStatus
    , ghccrsHealthCheckCount
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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

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
                   (pure (fromEnum s)) <*> (x .@ "HealthCheckCount"))

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
-- * 'ghccrsStatus'
--
-- * 'ghccrsHealthCheckCount'
data GetHealthCheckCountResponse = GetHealthCheckCountResponse'
    { _ghccrsStatus           :: !Int
    , _ghccrsHealthCheckCount :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHealthCheckCountResponse' smart constructor.
getHealthCheckCountResponse :: Int -> Integer -> GetHealthCheckCountResponse
getHealthCheckCountResponse pStatus_ pHealthCheckCount_ =
    GetHealthCheckCountResponse'
    { _ghccrsStatus = pStatus_
    , _ghccrsHealthCheckCount = pHealthCheckCount_
    }

-- | Undocumented member.
ghccrsStatus :: Lens' GetHealthCheckCountResponse Int
ghccrsStatus = lens _ghccrsStatus (\ s a -> s{_ghccrsStatus = a});

-- | The number of health checks associated with the current AWS account.
ghccrsHealthCheckCount :: Lens' GetHealthCheckCountResponse Integer
ghccrsHealthCheckCount = lens _ghccrsHealthCheckCount (\ s a -> s{_ghccrsHealthCheckCount = a});
