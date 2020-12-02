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
-- Module      : Network.AWS.IoT.GetV2LoggingOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the fine grained logging options.
--
--
module Network.AWS.IoT.GetV2LoggingOptions
    (
    -- * Creating a Request
      getV2LoggingOptions
    , GetV2LoggingOptions

    -- * Destructuring the Response
    , getV2LoggingOptionsResponse
    , GetV2LoggingOptionsResponse
    -- * Response Lenses
    , gvlorsDisableAllLogs
    , gvlorsDefaultLogLevel
    , gvlorsRoleARN
    , gvlorsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getV2LoggingOptions' smart constructor.
data GetV2LoggingOptions =
  GetV2LoggingOptions'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetV2LoggingOptions' with the minimum fields required to make a request.
--
getV2LoggingOptions
    :: GetV2LoggingOptions
getV2LoggingOptions = GetV2LoggingOptions'


instance AWSRequest GetV2LoggingOptions where
        type Rs GetV2LoggingOptions =
             GetV2LoggingOptionsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetV2LoggingOptionsResponse' <$>
                   (x .?> "disableAllLogs") <*>
                     (x .?> "defaultLogLevel")
                     <*> (x .?> "roleArn")
                     <*> (pure (fromEnum s)))

instance Hashable GetV2LoggingOptions where

instance NFData GetV2LoggingOptions where

instance ToHeaders GetV2LoggingOptions where
        toHeaders = const mempty

instance ToPath GetV2LoggingOptions where
        toPath = const "/v2LoggingOptions"

instance ToQuery GetV2LoggingOptions where
        toQuery = const mempty

-- | /See:/ 'getV2LoggingOptionsResponse' smart constructor.
data GetV2LoggingOptionsResponse = GetV2LoggingOptionsResponse'
  { _gvlorsDisableAllLogs  :: !(Maybe Bool)
  , _gvlorsDefaultLogLevel :: !(Maybe LogLevel)
  , _gvlorsRoleARN         :: !(Maybe Text)
  , _gvlorsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetV2LoggingOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvlorsDisableAllLogs' - Disables all logs.
--
-- * 'gvlorsDefaultLogLevel' - The default log level.
--
-- * 'gvlorsRoleARN' - The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
--
-- * 'gvlorsResponseStatus' - -- | The response status code.
getV2LoggingOptionsResponse
    :: Int -- ^ 'gvlorsResponseStatus'
    -> GetV2LoggingOptionsResponse
getV2LoggingOptionsResponse pResponseStatus_ =
  GetV2LoggingOptionsResponse'
    { _gvlorsDisableAllLogs = Nothing
    , _gvlorsDefaultLogLevel = Nothing
    , _gvlorsRoleARN = Nothing
    , _gvlorsResponseStatus = pResponseStatus_
    }


-- | Disables all logs.
gvlorsDisableAllLogs :: Lens' GetV2LoggingOptionsResponse (Maybe Bool)
gvlorsDisableAllLogs = lens _gvlorsDisableAllLogs (\ s a -> s{_gvlorsDisableAllLogs = a})

-- | The default log level.
gvlorsDefaultLogLevel :: Lens' GetV2LoggingOptionsResponse (Maybe LogLevel)
gvlorsDefaultLogLevel = lens _gvlorsDefaultLogLevel (\ s a -> s{_gvlorsDefaultLogLevel = a})

-- | The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
gvlorsRoleARN :: Lens' GetV2LoggingOptionsResponse (Maybe Text)
gvlorsRoleARN = lens _gvlorsRoleARN (\ s a -> s{_gvlorsRoleARN = a})

-- | -- | The response status code.
gvlorsResponseStatus :: Lens' GetV2LoggingOptionsResponse Int
gvlorsResponseStatus = lens _gvlorsResponseStatus (\ s a -> s{_gvlorsResponseStatus = a})

instance NFData GetV2LoggingOptionsResponse where
