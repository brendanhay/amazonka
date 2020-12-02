{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchBufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchBufferingHints where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the buffering to perform before delivering data to the Amazon ES destination.
--
--
--
-- /See:/ 'elasticsearchBufferingHints' smart constructor.
data ElasticsearchBufferingHints = ElasticsearchBufferingHints'
  { _ebhSizeInMBs ::
      !(Maybe Nat),
    _ebhIntervalInSeconds ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchBufferingHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebhSizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
--
-- * 'ebhIntervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
elasticsearchBufferingHints ::
  ElasticsearchBufferingHints
elasticsearchBufferingHints =
  ElasticsearchBufferingHints'
    { _ebhSizeInMBs = Nothing,
      _ebhIntervalInSeconds = Nothing
    }

-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
ebhSizeInMBs :: Lens' ElasticsearchBufferingHints (Maybe Natural)
ebhSizeInMBs = lens _ebhSizeInMBs (\s a -> s {_ebhSizeInMBs = a}) . mapping _Nat

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
ebhIntervalInSeconds :: Lens' ElasticsearchBufferingHints (Maybe Natural)
ebhIntervalInSeconds = lens _ebhIntervalInSeconds (\s a -> s {_ebhIntervalInSeconds = a}) . mapping _Nat

instance FromJSON ElasticsearchBufferingHints where
  parseJSON =
    withObject
      "ElasticsearchBufferingHints"
      ( \x ->
          ElasticsearchBufferingHints'
            <$> (x .:? "SizeInMBs") <*> (x .:? "IntervalInSeconds")
      )

instance Hashable ElasticsearchBufferingHints

instance NFData ElasticsearchBufferingHints

instance ToJSON ElasticsearchBufferingHints where
  toJSON ElasticsearchBufferingHints' {..} =
    object
      ( catMaybes
          [ ("SizeInMBs" .=) <$> _ebhSizeInMBs,
            ("IntervalInSeconds" .=) <$> _ebhIntervalInSeconds
          ]
      )
