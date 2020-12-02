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
-- Module      : Network.AWS.CloudFront.CreateRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time log configuration.
--
--
-- After you create a real-time log configuration, you can attach it to one or more cache behaviors to send real-time log data to the specified Amazon Kinesis data stream.
--
-- For more information about real-time log configurations, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateRealtimeLogConfig
  ( -- * Creating a Request
    createRealtimeLogConfig,
    CreateRealtimeLogConfig,

    -- * Request Lenses
    crlcEndPoints,
    crlcFields,
    crlcName,
    crlcSamplingRate,

    -- * Destructuring the Response
    createRealtimeLogConfigResponse,
    CreateRealtimeLogConfigResponse,

    -- * Response Lenses
    crlcrsRealtimeLogConfig,
    crlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRealtimeLogConfig' smart constructor.
data CreateRealtimeLogConfig = CreateRealtimeLogConfig'
  { _crlcEndPoints ::
      ![EndPoint],
    _crlcFields :: ![Text],
    _crlcName :: !Text,
    _crlcSamplingRate :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRealtimeLogConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crlcEndPoints' - Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- * 'crlcFields' - A list of fields to include in each real-time log record. For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'crlcName' - A unique name to identify this real-time log configuration.
--
-- * 'crlcSamplingRate' - The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
createRealtimeLogConfig ::
  -- | 'crlcName'
  Text ->
  -- | 'crlcSamplingRate'
  Integer ->
  CreateRealtimeLogConfig
createRealtimeLogConfig pName_ pSamplingRate_ =
  CreateRealtimeLogConfig'
    { _crlcEndPoints = mempty,
      _crlcFields = mempty,
      _crlcName = pName_,
      _crlcSamplingRate = pSamplingRate_
    }

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
crlcEndPoints :: Lens' CreateRealtimeLogConfig [EndPoint]
crlcEndPoints = lens _crlcEndPoints (\s a -> s {_crlcEndPoints = a}) . _Coerce

-- | A list of fields to include in each real-time log record. For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
crlcFields :: Lens' CreateRealtimeLogConfig [Text]
crlcFields = lens _crlcFields (\s a -> s {_crlcFields = a}) . _Coerce

-- | A unique name to identify this real-time log configuration.
crlcName :: Lens' CreateRealtimeLogConfig Text
crlcName = lens _crlcName (\s a -> s {_crlcName = a})

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
crlcSamplingRate :: Lens' CreateRealtimeLogConfig Integer
crlcSamplingRate = lens _crlcSamplingRate (\s a -> s {_crlcSamplingRate = a})

instance AWSRequest CreateRealtimeLogConfig where
  type Rs CreateRealtimeLogConfig = CreateRealtimeLogConfigResponse
  request = postXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          CreateRealtimeLogConfigResponse'
            <$> (x .@? "RealtimeLogConfig") <*> (pure (fromEnum s))
      )

instance Hashable CreateRealtimeLogConfig

instance NFData CreateRealtimeLogConfig

instance ToElement CreateRealtimeLogConfig where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CreateRealtimeLogConfigRequest"

instance ToHeaders CreateRealtimeLogConfig where
  toHeaders = const mempty

instance ToPath CreateRealtimeLogConfig where
  toPath = const "/2020-05-31/realtime-log-config"

instance ToQuery CreateRealtimeLogConfig where
  toQuery = const mempty

instance ToXML CreateRealtimeLogConfig where
  toXML CreateRealtimeLogConfig' {..} =
    mconcat
      [ "EndPoints" @= toXMLList "member" _crlcEndPoints,
        "Fields" @= toXMLList "Field" _crlcFields,
        "Name" @= _crlcName,
        "SamplingRate" @= _crlcSamplingRate
      ]

-- | /See:/ 'createRealtimeLogConfigResponse' smart constructor.
data CreateRealtimeLogConfigResponse = CreateRealtimeLogConfigResponse'
  { _crlcrsRealtimeLogConfig ::
      !(Maybe RealtimeLogConfig),
    _crlcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRealtimeLogConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crlcrsRealtimeLogConfig' - A real-time log configuration.
--
-- * 'crlcrsResponseStatus' - -- | The response status code.
createRealtimeLogConfigResponse ::
  -- | 'crlcrsResponseStatus'
  Int ->
  CreateRealtimeLogConfigResponse
createRealtimeLogConfigResponse pResponseStatus_ =
  CreateRealtimeLogConfigResponse'
    { _crlcrsRealtimeLogConfig =
        Nothing,
      _crlcrsResponseStatus = pResponseStatus_
    }

-- | A real-time log configuration.
crlcrsRealtimeLogConfig :: Lens' CreateRealtimeLogConfigResponse (Maybe RealtimeLogConfig)
crlcrsRealtimeLogConfig = lens _crlcrsRealtimeLogConfig (\s a -> s {_crlcrsRealtimeLogConfig = a})

-- | -- | The response status code.
crlcrsResponseStatus :: Lens' CreateRealtimeLogConfigResponse Int
crlcrsResponseStatus = lens _crlcrsResponseStatus (\s a -> s {_crlcrsResponseStatus = a})

instance NFData CreateRealtimeLogConfigResponse
