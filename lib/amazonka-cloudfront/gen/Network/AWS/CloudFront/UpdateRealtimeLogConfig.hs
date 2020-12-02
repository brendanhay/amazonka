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
-- Module      : Network.AWS.CloudFront.UpdateRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a real-time log configuration.
--
--
-- When you update a real-time log configuration, all the parameters are updated with the values provided in the request. You cannot update some parameters independent of others. To update a real-time log configuration:
--
--     * Call @GetRealtimeLogConfig@ to get the current real-time log configuration.
--
--     * Locally modify the parameters in the real-time log configuration that you want to update.
--
--     * Call this API (@UpdateRealtimeLogConfig@ ) by providing the entire real-time log configuration, including the parameters that you modified and those that you didn’t.
--
--
--
-- You cannot update a real-time log configuration’s @Name@ or @ARN@ .
module Network.AWS.CloudFront.UpdateRealtimeLogConfig
  ( -- * Creating a Request
    updateRealtimeLogConfig,
    UpdateRealtimeLogConfig,

    -- * Request Lenses
    urlcARN,
    urlcSamplingRate,
    urlcName,
    urlcEndPoints,
    urlcFields,

    -- * Destructuring the Response
    updateRealtimeLogConfigResponse,
    UpdateRealtimeLogConfigResponse,

    -- * Response Lenses
    urlcrsRealtimeLogConfig,
    urlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRealtimeLogConfig' smart constructor.
data UpdateRealtimeLogConfig = UpdateRealtimeLogConfig'
  { _urlcARN ::
      !(Maybe Text),
    _urlcSamplingRate :: !(Maybe Integer),
    _urlcName :: !(Maybe Text),
    _urlcEndPoints :: !(Maybe [EndPoint]),
    _urlcFields :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRealtimeLogConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urlcARN' - The Amazon Resource Name (ARN) for this real-time log configuration.
--
-- * 'urlcSamplingRate' - The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
--
-- * 'urlcName' - The name for this real-time log configuration.
--
-- * 'urlcEndPoints' - Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- * 'urlcFields' - A list of fields to include in each real-time log record. For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
updateRealtimeLogConfig ::
  UpdateRealtimeLogConfig
updateRealtimeLogConfig =
  UpdateRealtimeLogConfig'
    { _urlcARN = Nothing,
      _urlcSamplingRate = Nothing,
      _urlcName = Nothing,
      _urlcEndPoints = Nothing,
      _urlcFields = Nothing
    }

-- | The Amazon Resource Name (ARN) for this real-time log configuration.
urlcARN :: Lens' UpdateRealtimeLogConfig (Maybe Text)
urlcARN = lens _urlcARN (\s a -> s {_urlcARN = a})

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
urlcSamplingRate :: Lens' UpdateRealtimeLogConfig (Maybe Integer)
urlcSamplingRate = lens _urlcSamplingRate (\s a -> s {_urlcSamplingRate = a})

-- | The name for this real-time log configuration.
urlcName :: Lens' UpdateRealtimeLogConfig (Maybe Text)
urlcName = lens _urlcName (\s a -> s {_urlcName = a})

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
urlcEndPoints :: Lens' UpdateRealtimeLogConfig [EndPoint]
urlcEndPoints = lens _urlcEndPoints (\s a -> s {_urlcEndPoints = a}) . _Default . _Coerce

-- | A list of fields to include in each real-time log record. For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
urlcFields :: Lens' UpdateRealtimeLogConfig [Text]
urlcFields = lens _urlcFields (\s a -> s {_urlcFields = a}) . _Default . _Coerce

instance AWSRequest UpdateRealtimeLogConfig where
  type Rs UpdateRealtimeLogConfig = UpdateRealtimeLogConfigResponse
  request = putXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          UpdateRealtimeLogConfigResponse'
            <$> (x .@? "RealtimeLogConfig") <*> (pure (fromEnum s))
      )

instance Hashable UpdateRealtimeLogConfig

instance NFData UpdateRealtimeLogConfig

instance ToElement UpdateRealtimeLogConfig where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}UpdateRealtimeLogConfigRequest"

instance ToHeaders UpdateRealtimeLogConfig where
  toHeaders = const mempty

instance ToPath UpdateRealtimeLogConfig where
  toPath = const "/2020-05-31/realtime-log-config/"

instance ToQuery UpdateRealtimeLogConfig where
  toQuery = const mempty

instance ToXML UpdateRealtimeLogConfig where
  toXML UpdateRealtimeLogConfig' {..} =
    mconcat
      [ "ARN" @= _urlcARN,
        "SamplingRate" @= _urlcSamplingRate,
        "Name" @= _urlcName,
        "EndPoints" @= toXML (toXMLList "member" <$> _urlcEndPoints),
        "Fields" @= toXML (toXMLList "Field" <$> _urlcFields)
      ]

-- | /See:/ 'updateRealtimeLogConfigResponse' smart constructor.
data UpdateRealtimeLogConfigResponse = UpdateRealtimeLogConfigResponse'
  { _urlcrsRealtimeLogConfig ::
      !(Maybe RealtimeLogConfig),
    _urlcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRealtimeLogConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urlcrsRealtimeLogConfig' - A real-time log configuration.
--
-- * 'urlcrsResponseStatus' - -- | The response status code.
updateRealtimeLogConfigResponse ::
  -- | 'urlcrsResponseStatus'
  Int ->
  UpdateRealtimeLogConfigResponse
updateRealtimeLogConfigResponse pResponseStatus_ =
  UpdateRealtimeLogConfigResponse'
    { _urlcrsRealtimeLogConfig =
        Nothing,
      _urlcrsResponseStatus = pResponseStatus_
    }

-- | A real-time log configuration.
urlcrsRealtimeLogConfig :: Lens' UpdateRealtimeLogConfigResponse (Maybe RealtimeLogConfig)
urlcrsRealtimeLogConfig = lens _urlcrsRealtimeLogConfig (\s a -> s {_urlcrsRealtimeLogConfig = a})

-- | -- | The response status code.
urlcrsResponseStatus :: Lens' UpdateRealtimeLogConfigResponse Int
urlcrsResponseStatus = lens _urlcrsResponseStatus (\s a -> s {_urlcrsResponseStatus = a})

instance NFData UpdateRealtimeLogConfigResponse
