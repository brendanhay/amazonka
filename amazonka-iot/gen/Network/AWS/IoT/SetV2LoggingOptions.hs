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
-- Module      : Network.AWS.IoT.SetV2LoggingOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging options for the V2 logging service.
--
--
module Network.AWS.IoT.SetV2LoggingOptions
    (
    -- * Creating a Request
      setV2LoggingOptions
    , SetV2LoggingOptions
    -- * Request Lenses
    , svloDisableAllLogs
    , svloDefaultLogLevel
    , svloRoleARN

    -- * Destructuring the Response
    , setV2LoggingOptionsResponse
    , SetV2LoggingOptionsResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setV2LoggingOptions' smart constructor.
data SetV2LoggingOptions = SetV2LoggingOptions'
  { _svloDisableAllLogs  :: !(Maybe Bool)
  , _svloDefaultLogLevel :: !(Maybe LogLevel)
  , _svloRoleARN         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetV2LoggingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svloDisableAllLogs' - Set to true to disable all logs, otherwise set to false.
--
-- * 'svloDefaultLogLevel' - The default logging level.
--
-- * 'svloRoleARN' - The role ARN that allows IoT to write to Cloudwatch logs.
setV2LoggingOptions
    :: SetV2LoggingOptions
setV2LoggingOptions =
  SetV2LoggingOptions'
    { _svloDisableAllLogs = Nothing
    , _svloDefaultLogLevel = Nothing
    , _svloRoleARN = Nothing
    }


-- | Set to true to disable all logs, otherwise set to false.
svloDisableAllLogs :: Lens' SetV2LoggingOptions (Maybe Bool)
svloDisableAllLogs = lens _svloDisableAllLogs (\ s a -> s{_svloDisableAllLogs = a})

-- | The default logging level.
svloDefaultLogLevel :: Lens' SetV2LoggingOptions (Maybe LogLevel)
svloDefaultLogLevel = lens _svloDefaultLogLevel (\ s a -> s{_svloDefaultLogLevel = a})

-- | The role ARN that allows IoT to write to Cloudwatch logs.
svloRoleARN :: Lens' SetV2LoggingOptions (Maybe Text)
svloRoleARN = lens _svloRoleARN (\ s a -> s{_svloRoleARN = a})

instance AWSRequest SetV2LoggingOptions where
        type Rs SetV2LoggingOptions =
             SetV2LoggingOptionsResponse
        request = postJSON ioT
        response = receiveNull SetV2LoggingOptionsResponse'

instance Hashable SetV2LoggingOptions where

instance NFData SetV2LoggingOptions where

instance ToHeaders SetV2LoggingOptions where
        toHeaders = const mempty

instance ToJSON SetV2LoggingOptions where
        toJSON SetV2LoggingOptions'{..}
          = object
              (catMaybes
                 [("disableAllLogs" .=) <$> _svloDisableAllLogs,
                  ("defaultLogLevel" .=) <$> _svloDefaultLogLevel,
                  ("roleArn" .=) <$> _svloRoleARN])

instance ToPath SetV2LoggingOptions where
        toPath = const "/v2LoggingOptions"

instance ToQuery SetV2LoggingOptions where
        toQuery = const mempty

-- | /See:/ 'setV2LoggingOptionsResponse' smart constructor.
data SetV2LoggingOptionsResponse =
  SetV2LoggingOptionsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetV2LoggingOptionsResponse' with the minimum fields required to make a request.
--
setV2LoggingOptionsResponse
    :: SetV2LoggingOptionsResponse
setV2LoggingOptionsResponse = SetV2LoggingOptionsResponse'


instance NFData SetV2LoggingOptionsResponse where
