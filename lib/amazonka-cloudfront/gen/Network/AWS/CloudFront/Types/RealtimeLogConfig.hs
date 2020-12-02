{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeLogConfig where

import Network.AWS.CloudFront.Types.EndPoint
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A real-time log configuration.
--
--
--
-- /See:/ 'realtimeLogConfig' smart constructor.
data RealtimeLogConfig = RealtimeLogConfig'
  { _rlcARN :: !Text,
    _rlcName :: !Text,
    _rlcSamplingRate :: !Integer,
    _rlcEndPoints :: ![EndPoint],
    _rlcFields :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RealtimeLogConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlcARN' - The Amazon Resource Name (ARN) of this real-time log configuration.
--
-- * 'rlcName' - The unique name of this real-time log configuration.
--
-- * 'rlcSamplingRate' - The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. The sampling rate is an integer between 1 and 100, inclusive.
--
-- * 'rlcEndPoints' - Contains information about the Amazon Kinesis data stream where you are sending real-time log data for this real-time log configuration.
--
-- * 'rlcFields' - A list of fields that are included in each real-time log record. In an API response, the fields are provided in the same order in which they are sent to the Amazon Kinesis data stream. For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
realtimeLogConfig ::
  -- | 'rlcARN'
  Text ->
  -- | 'rlcName'
  Text ->
  -- | 'rlcSamplingRate'
  Integer ->
  RealtimeLogConfig
realtimeLogConfig pARN_ pName_ pSamplingRate_ =
  RealtimeLogConfig'
    { _rlcARN = pARN_,
      _rlcName = pName_,
      _rlcSamplingRate = pSamplingRate_,
      _rlcEndPoints = mempty,
      _rlcFields = mempty
    }

-- | The Amazon Resource Name (ARN) of this real-time log configuration.
rlcARN :: Lens' RealtimeLogConfig Text
rlcARN = lens _rlcARN (\s a -> s {_rlcARN = a})

-- | The unique name of this real-time log configuration.
rlcName :: Lens' RealtimeLogConfig Text
rlcName = lens _rlcName (\s a -> s {_rlcName = a})

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. The sampling rate is an integer between 1 and 100, inclusive.
rlcSamplingRate :: Lens' RealtimeLogConfig Integer
rlcSamplingRate = lens _rlcSamplingRate (\s a -> s {_rlcSamplingRate = a})

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data for this real-time log configuration.
rlcEndPoints :: Lens' RealtimeLogConfig [EndPoint]
rlcEndPoints = lens _rlcEndPoints (\s a -> s {_rlcEndPoints = a}) . _Coerce

-- | A list of fields that are included in each real-time log record. In an API response, the fields are provided in the same order in which they are sent to the Amazon Kinesis data stream. For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
rlcFields :: Lens' RealtimeLogConfig [Text]
rlcFields = lens _rlcFields (\s a -> s {_rlcFields = a}) . _Coerce

instance FromXML RealtimeLogConfig where
  parseXML x =
    RealtimeLogConfig'
      <$> (x .@ "ARN")
      <*> (x .@ "Name")
      <*> (x .@ "SamplingRate")
      <*> (x .@? "EndPoints" .!@ mempty >>= parseXMLList "member")
      <*> (x .@? "Fields" .!@ mempty >>= parseXMLList "Field")

instance Hashable RealtimeLogConfig

instance NFData RealtimeLogConfig
