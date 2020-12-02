{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.BufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.BufferingHints where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes hints for the buffering to perform before delivering data to the destination. These options are treated as hints, and therefore Kinesis Data Firehose might choose to use different values when it is optimal. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
--
--
-- /See:/ 'bufferingHints' smart constructor.
data BufferingHints = BufferingHints'
  { _bhSizeInMBs :: !(Maybe Nat),
    _bhIntervalInSeconds :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BufferingHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bhSizeInMBs' - Buffer incoming data to the specified size, in MiBs, before delivering it to the destination. The default value is 5. This parameter is optional but if you specify a value for it, you must also specify a value for @IntervalInSeconds@ , and vice versa. We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MiB/sec, the value should be 10 MiB or higher.
--
-- * 'bhIntervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300. This parameter is optional but if you specify a value for it, you must also specify a value for @SizeInMBs@ , and vice versa.
bufferingHints ::
  BufferingHints
bufferingHints =
  BufferingHints'
    { _bhSizeInMBs = Nothing,
      _bhIntervalInSeconds = Nothing
    }

-- | Buffer incoming data to the specified size, in MiBs, before delivering it to the destination. The default value is 5. This parameter is optional but if you specify a value for it, you must also specify a value for @IntervalInSeconds@ , and vice versa. We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MiB/sec, the value should be 10 MiB or higher.
bhSizeInMBs :: Lens' BufferingHints (Maybe Natural)
bhSizeInMBs = lens _bhSizeInMBs (\s a -> s {_bhSizeInMBs = a}) . mapping _Nat

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300. This parameter is optional but if you specify a value for it, you must also specify a value for @SizeInMBs@ , and vice versa.
bhIntervalInSeconds :: Lens' BufferingHints (Maybe Natural)
bhIntervalInSeconds = lens _bhIntervalInSeconds (\s a -> s {_bhIntervalInSeconds = a}) . mapping _Nat

instance FromJSON BufferingHints where
  parseJSON =
    withObject
      "BufferingHints"
      ( \x ->
          BufferingHints'
            <$> (x .:? "SizeInMBs") <*> (x .:? "IntervalInSeconds")
      )

instance Hashable BufferingHints

instance NFData BufferingHints

instance ToJSON BufferingHints where
  toJSON BufferingHints' {..} =
    object
      ( catMaybes
          [ ("SizeInMBs" .=) <$> _bhSizeInMBs,
            ("IntervalInSeconds" .=) <$> _bhIntervalInSeconds
          ]
      )
