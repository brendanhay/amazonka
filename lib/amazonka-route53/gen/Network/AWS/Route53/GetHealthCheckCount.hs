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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the number of health checks that are associated with the current AWS account.
--
--
module Network.AWS.Route53.GetHealthCheckCount
    (
    -- * Creating a Request
      getHealthCheckCount
    , GetHealthCheckCount

    -- * Destructuring the Response
    , getHealthCheckCountResponse
    , GetHealthCheckCountResponse
    -- * Response Lenses
    , ghccrsResponseStatus
    , ghccrsHealthCheckCount
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request for the number of health checks that are associated with the current AWS account.
--
--
--
-- /See:/ 'getHealthCheckCount' smart constructor.
data GetHealthCheckCount =
  GetHealthCheckCount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


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

instance Hashable GetHealthCheckCount where

instance NFData GetHealthCheckCount where

instance ToHeaders GetHealthCheckCount where
        toHeaders = const mempty

instance ToPath GetHealthCheckCount where
        toPath = const "/2013-04-01/healthcheckcount"

instance ToQuery GetHealthCheckCount where
        toQuery = const mempty

-- | A complex type that contains the response to a @GetHealthCheckCount@ request.
--
--
--
-- /See:/ 'getHealthCheckCountResponse' smart constructor.
data GetHealthCheckCountResponse = GetHealthCheckCountResponse'
  { _ghccrsResponseStatus   :: !Int
  , _ghccrsHealthCheckCount :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHealthCheckCountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghccrsResponseStatus' - -- | The response status code.
--
-- * 'ghccrsHealthCheckCount' - The number of health checks associated with the current AWS account.
getHealthCheckCountResponse
    :: Int -- ^ 'ghccrsResponseStatus'
    -> Integer -- ^ 'ghccrsHealthCheckCount'
    -> GetHealthCheckCountResponse
getHealthCheckCountResponse pResponseStatus_ pHealthCheckCount_ =
  GetHealthCheckCountResponse'
    { _ghccrsResponseStatus = pResponseStatus_
    , _ghccrsHealthCheckCount = pHealthCheckCount_
    }


-- | -- | The response status code.
ghccrsResponseStatus :: Lens' GetHealthCheckCountResponse Int
ghccrsResponseStatus = lens _ghccrsResponseStatus (\ s a -> s{_ghccrsResponseStatus = a})

-- | The number of health checks associated with the current AWS account.
ghccrsHealthCheckCount :: Lens' GetHealthCheckCountResponse Integer
ghccrsHealthCheckCount = lens _ghccrsHealthCheckCount (\ s a -> s{_ghccrsHealthCheckCount = a})

instance NFData GetHealthCheckCountResponse where
