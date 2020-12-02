{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SourceDescription where

import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about a Kinesis data stream used as the source for a Kinesis Data Firehose delivery stream.
--
--
--
-- /See:/ 'sourceDescription' smart constructor.
newtype SourceDescription = SourceDescription'
  { _sdKinesisStreamSourceDescription ::
      Maybe KinesisStreamSourceDescription
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdKinesisStreamSourceDescription' - The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
sourceDescription ::
  SourceDescription
sourceDescription =
  SourceDescription' {_sdKinesisStreamSourceDescription = Nothing}

-- | The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
sdKinesisStreamSourceDescription :: Lens' SourceDescription (Maybe KinesisStreamSourceDescription)
sdKinesisStreamSourceDescription = lens _sdKinesisStreamSourceDescription (\s a -> s {_sdKinesisStreamSourceDescription = a})

instance FromJSON SourceDescription where
  parseJSON =
    withObject
      "SourceDescription"
      ( \x ->
          SourceDescription' <$> (x .:? "KinesisStreamSourceDescription")
      )

instance Hashable SourceDescription

instance NFData SourceDescription
