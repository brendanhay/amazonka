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
-- Module      : Network.AWS.Route53.GetQueryLoggingConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified configuration for DNS query logging.
--
--
-- For more information about DNS query logs, see 'CreateQueryLoggingConfig' and <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries> .
--
module Network.AWS.Route53.GetQueryLoggingConfig
    (
    -- * Creating a Request
      getQueryLoggingConfig
    , GetQueryLoggingConfig
    -- * Request Lenses
    , gqlcId

    -- * Destructuring the Response
    , getQueryLoggingConfigResponse
    , GetQueryLoggingConfigResponse
    -- * Response Lenses
    , gqlcrsResponseStatus
    , gqlcrsQueryLoggingConfig
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | /See:/ 'getQueryLoggingConfig' smart constructor.
newtype GetQueryLoggingConfig = GetQueryLoggingConfig'
  { _gqlcId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueryLoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqlcId' - The ID of the configuration for DNS query logging that you want to get information about.
getQueryLoggingConfig
    :: Text -- ^ 'gqlcId'
    -> GetQueryLoggingConfig
getQueryLoggingConfig pId_ = GetQueryLoggingConfig' {_gqlcId = pId_}


-- | The ID of the configuration for DNS query logging that you want to get information about.
gqlcId :: Lens' GetQueryLoggingConfig Text
gqlcId = lens _gqlcId (\ s a -> s{_gqlcId = a})

instance AWSRequest GetQueryLoggingConfig where
        type Rs GetQueryLoggingConfig =
             GetQueryLoggingConfigResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetQueryLoggingConfigResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "QueryLoggingConfig"))

instance Hashable GetQueryLoggingConfig where

instance NFData GetQueryLoggingConfig where

instance ToHeaders GetQueryLoggingConfig where
        toHeaders = const mempty

instance ToPath GetQueryLoggingConfig where
        toPath GetQueryLoggingConfig'{..}
          = mconcat
              ["/2013-04-01/queryloggingconfig/", toBS _gqlcId]

instance ToQuery GetQueryLoggingConfig where
        toQuery = const mempty

-- | /See:/ 'getQueryLoggingConfigResponse' smart constructor.
data GetQueryLoggingConfigResponse = GetQueryLoggingConfigResponse'
  { _gqlcrsResponseStatus     :: !Int
  , _gqlcrsQueryLoggingConfig :: !QueryLoggingConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueryLoggingConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqlcrsResponseStatus' - -- | The response status code.
--
-- * 'gqlcrsQueryLoggingConfig' - A complex type that contains information about the query logging configuration that you specified in a 'GetQueryLoggingConfig' request.
getQueryLoggingConfigResponse
    :: Int -- ^ 'gqlcrsResponseStatus'
    -> QueryLoggingConfig -- ^ 'gqlcrsQueryLoggingConfig'
    -> GetQueryLoggingConfigResponse
getQueryLoggingConfigResponse pResponseStatus_ pQueryLoggingConfig_ =
  GetQueryLoggingConfigResponse'
    { _gqlcrsResponseStatus = pResponseStatus_
    , _gqlcrsQueryLoggingConfig = pQueryLoggingConfig_
    }


-- | -- | The response status code.
gqlcrsResponseStatus :: Lens' GetQueryLoggingConfigResponse Int
gqlcrsResponseStatus = lens _gqlcrsResponseStatus (\ s a -> s{_gqlcrsResponseStatus = a})

-- | A complex type that contains information about the query logging configuration that you specified in a 'GetQueryLoggingConfig' request.
gqlcrsQueryLoggingConfig :: Lens' GetQueryLoggingConfigResponse QueryLoggingConfig
gqlcrsQueryLoggingConfig = lens _gqlcrsQueryLoggingConfig (\ s a -> s{_gqlcrsQueryLoggingConfig = a})

instance NFData GetQueryLoggingConfigResponse where
