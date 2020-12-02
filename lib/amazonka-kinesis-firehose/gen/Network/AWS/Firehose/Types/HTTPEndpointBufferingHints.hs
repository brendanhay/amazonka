{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointBufferingHints where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the buffering options that can be applied before data is delivered to the HTTP endpoint destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
--
--
-- /See:/ 'hTTPEndpointBufferingHints' smart constructor.
data HTTPEndpointBufferingHints = HTTPEndpointBufferingHints'
  { _httpebhSizeInMBs ::
      !(Maybe Nat),
    _httpebhIntervalInSeconds ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointBufferingHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpebhSizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.  We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
--
-- * 'httpebhIntervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
hTTPEndpointBufferingHints ::
  HTTPEndpointBufferingHints
hTTPEndpointBufferingHints =
  HTTPEndpointBufferingHints'
    { _httpebhSizeInMBs = Nothing,
      _httpebhIntervalInSeconds = Nothing
    }

-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.  We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
httpebhSizeInMBs :: Lens' HTTPEndpointBufferingHints (Maybe Natural)
httpebhSizeInMBs = lens _httpebhSizeInMBs (\s a -> s {_httpebhSizeInMBs = a}) . mapping _Nat

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
httpebhIntervalInSeconds :: Lens' HTTPEndpointBufferingHints (Maybe Natural)
httpebhIntervalInSeconds = lens _httpebhIntervalInSeconds (\s a -> s {_httpebhIntervalInSeconds = a}) . mapping _Nat

instance FromJSON HTTPEndpointBufferingHints where
  parseJSON =
    withObject
      "HTTPEndpointBufferingHints"
      ( \x ->
          HTTPEndpointBufferingHints'
            <$> (x .:? "SizeInMBs") <*> (x .:? "IntervalInSeconds")
      )

instance Hashable HTTPEndpointBufferingHints

instance NFData HTTPEndpointBufferingHints

instance ToJSON HTTPEndpointBufferingHints where
  toJSON HTTPEndpointBufferingHints' {..} =
    object
      ( catMaybes
          [ ("SizeInMBs" .=) <$> _httpebhSizeInMBs,
            ("IntervalInSeconds" .=) <$> _httpebhIntervalInSeconds
          ]
      )
