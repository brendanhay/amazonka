{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration where

import Network.AWS.KinesisAnalytics.Types.InputStartingPosition
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the point at which the application reads from the streaming source.
--
--
--
-- /See:/ 'inputStartingPositionConfiguration' smart constructor.
newtype InputStartingPositionConfiguration = InputStartingPositionConfiguration'
  { _ispcInputStartingPosition ::
      Maybe
        InputStartingPosition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputStartingPositionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ispcInputStartingPosition' - The starting position on the stream.     * @NOW@ - Start reading just after the most recent record in the stream, start at the request time stamp that the customer issued.     * @TRIM_HORIZON@ - Start reading at the last untrimmed record in the stream, which is the oldest record available in the stream. This option is not available for an Amazon Kinesis Firehose delivery stream.     * @LAST_STOPPED_POINT@ - Resume reading from where the application last stopped reading.
inputStartingPositionConfiguration ::
  InputStartingPositionConfiguration
inputStartingPositionConfiguration =
  InputStartingPositionConfiguration'
    { _ispcInputStartingPosition =
        Nothing
    }

-- | The starting position on the stream.     * @NOW@ - Start reading just after the most recent record in the stream, start at the request time stamp that the customer issued.     * @TRIM_HORIZON@ - Start reading at the last untrimmed record in the stream, which is the oldest record available in the stream. This option is not available for an Amazon Kinesis Firehose delivery stream.     * @LAST_STOPPED_POINT@ - Resume reading from where the application last stopped reading.
ispcInputStartingPosition :: Lens' InputStartingPositionConfiguration (Maybe InputStartingPosition)
ispcInputStartingPosition = lens _ispcInputStartingPosition (\s a -> s {_ispcInputStartingPosition = a})

instance FromJSON InputStartingPositionConfiguration where
  parseJSON =
    withObject
      "InputStartingPositionConfiguration"
      ( \x ->
          InputStartingPositionConfiguration'
            <$> (x .:? "InputStartingPosition")
      )

instance Hashable InputStartingPositionConfiguration

instance NFData InputStartingPositionConfiguration

instance ToJSON InputStartingPositionConfiguration where
  toJSON InputStartingPositionConfiguration' {..} =
    object
      ( catMaybes
          [("InputStartingPosition" .=) <$> _ispcInputStartingPosition]
      )
