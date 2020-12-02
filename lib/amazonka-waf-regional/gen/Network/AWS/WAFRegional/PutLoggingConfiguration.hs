{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.PutLoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a 'LoggingConfiguration' with a specified web ACL.
--
--
-- You can access information about all traffic that AWS WAF inspects using the following steps:
--
--     * Create an Amazon Kinesis Data Firehose.
--
-- Create the data firehose with a PUT source and in the region that you are operating. However, if you are capturing logs for Amazon CloudFront, always create the firehose in US East (N. Virginia).
--
--     * Associate that firehose to your web ACL using a @PutLoggingConfiguration@ request.
--
--
--
-- When you successfully enable logging using a @PutLoggingConfiguration@ request, AWS WAF will create a service linked role with the necessary permissions to write logs to the Amazon Kinesis Data Firehose. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging Web ACL Traffic Information> in the /AWS WAF Developer Guide/ .
module Network.AWS.WAFRegional.PutLoggingConfiguration
  ( -- * Creating a Request
    putLoggingConfiguration,
    PutLoggingConfiguration,

    -- * Request Lenses
    plcLoggingConfiguration,

    -- * Destructuring the Response
    putLoggingConfigurationResponse,
    PutLoggingConfigurationResponse,

    -- * Response Lenses
    plcrsLoggingConfiguration,
    plcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'putLoggingConfiguration' smart constructor.
newtype PutLoggingConfiguration = PutLoggingConfiguration'
  { _plcLoggingConfiguration ::
      LoggingConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutLoggingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plcLoggingConfiguration' - The Amazon Kinesis Data Firehose that contains the inspected traffic information, the redacted fields details, and the Amazon Resource Name (ARN) of the web ACL to monitor.
putLoggingConfiguration ::
  -- | 'plcLoggingConfiguration'
  LoggingConfiguration ->
  PutLoggingConfiguration
putLoggingConfiguration pLoggingConfiguration_ =
  PutLoggingConfiguration'
    { _plcLoggingConfiguration =
        pLoggingConfiguration_
    }

-- | The Amazon Kinesis Data Firehose that contains the inspected traffic information, the redacted fields details, and the Amazon Resource Name (ARN) of the web ACL to monitor.
plcLoggingConfiguration :: Lens' PutLoggingConfiguration LoggingConfiguration
plcLoggingConfiguration = lens _plcLoggingConfiguration (\s a -> s {_plcLoggingConfiguration = a})

instance AWSRequest PutLoggingConfiguration where
  type Rs PutLoggingConfiguration = PutLoggingConfigurationResponse
  request = postJSON wAFRegional
  response =
    receiveJSON
      ( \s h x ->
          PutLoggingConfigurationResponse'
            <$> (x .?> "LoggingConfiguration") <*> (pure (fromEnum s))
      )

instance Hashable PutLoggingConfiguration

instance NFData PutLoggingConfiguration

instance ToHeaders PutLoggingConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSWAF_Regional_20161128.PutLoggingConfiguration" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutLoggingConfiguration where
  toJSON PutLoggingConfiguration' {..} =
    object
      ( catMaybes
          [Just ("LoggingConfiguration" .= _plcLoggingConfiguration)]
      )

instance ToPath PutLoggingConfiguration where
  toPath = const "/"

instance ToQuery PutLoggingConfiguration where
  toQuery = const mempty

-- | /See:/ 'putLoggingConfigurationResponse' smart constructor.
data PutLoggingConfigurationResponse = PutLoggingConfigurationResponse'
  { _plcrsLoggingConfiguration ::
      !( Maybe
           LoggingConfiguration
       ),
    _plcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutLoggingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plcrsLoggingConfiguration' - The 'LoggingConfiguration' that you submitted in the request.
--
-- * 'plcrsResponseStatus' - -- | The response status code.
putLoggingConfigurationResponse ::
  -- | 'plcrsResponseStatus'
  Int ->
  PutLoggingConfigurationResponse
putLoggingConfigurationResponse pResponseStatus_ =
  PutLoggingConfigurationResponse'
    { _plcrsLoggingConfiguration =
        Nothing,
      _plcrsResponseStatus = pResponseStatus_
    }

-- | The 'LoggingConfiguration' that you submitted in the request.
plcrsLoggingConfiguration :: Lens' PutLoggingConfigurationResponse (Maybe LoggingConfiguration)
plcrsLoggingConfiguration = lens _plcrsLoggingConfiguration (\s a -> s {_plcrsLoggingConfiguration = a})

-- | -- | The response status code.
plcrsResponseStatus :: Lens' PutLoggingConfigurationResponse Int
plcrsResponseStatus = lens _plcrsResponseStatus (\s a -> s {_plcrsResponseStatus = a})

instance NFData PutLoggingConfigurationResponse
