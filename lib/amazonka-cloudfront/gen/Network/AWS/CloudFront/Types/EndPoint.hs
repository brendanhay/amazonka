{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EndPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EndPoint where

import Network.AWS.CloudFront.Types.KinesisStreamConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data in a real-time log configuration.
--
--
--
-- /See:/ 'endPoint' smart constructor.
data EndPoint = EndPoint'
  { _epKinesisStreamConfig ::
      !(Maybe KinesisStreamConfig),
    _epStreamType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epKinesisStreamConfig' - Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- * 'epStreamType' - The type of data stream where you are sending real-time log data. The only valid value is @Kinesis@ .
endPoint ::
  -- | 'epStreamType'
  Text ->
  EndPoint
endPoint pStreamType_ =
  EndPoint'
    { _epKinesisStreamConfig = Nothing,
      _epStreamType = pStreamType_
    }

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
epKinesisStreamConfig :: Lens' EndPoint (Maybe KinesisStreamConfig)
epKinesisStreamConfig = lens _epKinesisStreamConfig (\s a -> s {_epKinesisStreamConfig = a})

-- | The type of data stream where you are sending real-time log data. The only valid value is @Kinesis@ .
epStreamType :: Lens' EndPoint Text
epStreamType = lens _epStreamType (\s a -> s {_epStreamType = a})

instance FromXML EndPoint where
  parseXML x =
    EndPoint'
      <$> (x .@? "KinesisStreamConfig") <*> (x .@ "StreamType")

instance Hashable EndPoint

instance NFData EndPoint

instance ToXML EndPoint where
  toXML EndPoint' {..} =
    mconcat
      [ "KinesisStreamConfig" @= _epKinesisStreamConfig,
        "StreamType" @= _epStreamType
      ]
