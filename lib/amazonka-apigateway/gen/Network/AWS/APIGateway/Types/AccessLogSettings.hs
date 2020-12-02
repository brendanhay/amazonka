{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.AccessLogSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.AccessLogSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Access log settings, including the access log format and access log destination ARN.
--
--
--
-- /See:/ 'accessLogSettings' smart constructor.
data AccessLogSettings = AccessLogSettings'
  { _alsFormat ::
      !(Maybe Text),
    _alsDestinationARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessLogSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alsFormat' - A single line format of the access logs of data, as specified by selected <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference > context variables> . The format must include at least @> context.requestId@ .
--
-- * 'alsDestinationARN' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or Kinesis Data Firehose delivery stream to receive access logs. If you specify a Kinesis Data Firehose delivery stream, the stream name must begin with @amazon-apigateway-@ .
accessLogSettings ::
  AccessLogSettings
accessLogSettings =
  AccessLogSettings'
    { _alsFormat = Nothing,
      _alsDestinationARN = Nothing
    }

-- | A single line format of the access logs of data, as specified by selected <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference > context variables> . The format must include at least @> context.requestId@ .
alsFormat :: Lens' AccessLogSettings (Maybe Text)
alsFormat = lens _alsFormat (\s a -> s {_alsFormat = a})

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or Kinesis Data Firehose delivery stream to receive access logs. If you specify a Kinesis Data Firehose delivery stream, the stream name must begin with @amazon-apigateway-@ .
alsDestinationARN :: Lens' AccessLogSettings (Maybe Text)
alsDestinationARN = lens _alsDestinationARN (\s a -> s {_alsDestinationARN = a})

instance FromJSON AccessLogSettings where
  parseJSON =
    withObject
      "AccessLogSettings"
      ( \x ->
          AccessLogSettings'
            <$> (x .:? "format") <*> (x .:? "destinationArn")
      )

instance Hashable AccessLogSettings

instance NFData AccessLogSettings
