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
-- Module      : Network.AWS.Route53.GetHealthCheckCount
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a count of all your health checks, send a 'GET' request to
-- the '2013-04-01\/healthcheckcount' resource.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheckCount.html AWS API Reference> for GetHealthCheckCount.
module Network.AWS.Route53.GetHealthCheckCount
    (
    -- * Creating a Request
      getHealthCheckCount
    , GetHealthCheckCount

    -- * Destructuring the Response
    , getHealthCheckCountResponse
    , GetHealthCheckCountResponse
    -- * Response Lenses
    , ghccrsStatus
    , ghccrsHealthCheckCount
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | To retrieve a count of all your health checks, send a 'GET' request to
-- the '2013-04-01\/healthcheckcount' resource.
--
-- /See:/ 'getHealthCheckCount' smart constructor.
data GetHealthCheckCount =
    GetHealthCheckCount'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetHealthCheckCount' with the minimum fields required to make a request.
--
getHealthCheckCount
    :: GetHealthCheckCount
getHealthCheckCount = GetHealthCheckCount'

instance AWSRequest GetHealthCheckCount where
        type Rs GetHealthCheckCount =
             GetHealthCheckCountResponse
        request = get route53
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
data GetHealthCheckCountResponse = GetHealthCheckCountResponse'
    { _ghccrsStatus           :: !Int
    , _ghccrsHealthCheckCount :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetHealthCheckCountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghccrsStatus'
--
-- * 'ghccrsHealthCheckCount'
getHealthCheckCountResponse
    :: Int -- ^ 'ghccrsStatus'
    -> Integer -- ^ 'ghccrsHealthCheckCount'
    -> GetHealthCheckCountResponse
getHealthCheckCountResponse pStatus_ pHealthCheckCount_ =
    GetHealthCheckCountResponse'
    { _ghccrsStatus = pStatus_
    , _ghccrsHealthCheckCount = pHealthCheckCount_
    }

-- | The response status code.
ghccrsStatus :: Lens' GetHealthCheckCountResponse Int
ghccrsStatus = lens _ghccrsStatus (\ s a -> s{_ghccrsStatus = a});

-- | The number of health checks associated with the current AWS account.
ghccrsHealthCheckCount :: Lens' GetHealthCheckCountResponse Integer
ghccrsHealthCheckCount = lens _ghccrsHealthCheckCount (\ s a -> s{_ghccrsHealthCheckCount = a});
