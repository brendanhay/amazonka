{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The start and end of the timestamp range for the requested media.
--
--
-- This value should not be present if @PlaybackType@ is @LIVE@ .
--
--
-- /See:/ 'dASHTimestampRange' smart constructor.
data DASHTimestampRange = DASHTimestampRange'
  { _dashtrEndTimestamp ::
      !(Maybe POSIX),
    _dashtrStartTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DASHTimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dashtrEndTimestamp' - The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past. The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for @LIVE_REPLAY@ mode then the session will continue to include newly ingested fragments until the session expires.
--
-- * 'dashtrStartTimestamp' - The start of the timestamp range for the requested media. If the @DASHTimestampRange@ value is specified, the @StartTimestamp@ value is required.
dASHTimestampRange ::
  DASHTimestampRange
dASHTimestampRange =
  DASHTimestampRange'
    { _dashtrEndTimestamp = Nothing,
      _dashtrStartTimestamp = Nothing
    }

-- | The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past. The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for @LIVE_REPLAY@ mode then the session will continue to include newly ingested fragments until the session expires.
dashtrEndTimestamp :: Lens' DASHTimestampRange (Maybe UTCTime)
dashtrEndTimestamp = lens _dashtrEndTimestamp (\s a -> s {_dashtrEndTimestamp = a}) . mapping _Time

-- | The start of the timestamp range for the requested media. If the @DASHTimestampRange@ value is specified, the @StartTimestamp@ value is required.
dashtrStartTimestamp :: Lens' DASHTimestampRange (Maybe UTCTime)
dashtrStartTimestamp = lens _dashtrStartTimestamp (\s a -> s {_dashtrStartTimestamp = a}) . mapping _Time

instance Hashable DASHTimestampRange

instance NFData DASHTimestampRange

instance ToJSON DASHTimestampRange where
  toJSON DASHTimestampRange' {..} =
    object
      ( catMaybes
          [ ("EndTimestamp" .=) <$> _dashtrEndTimestamp,
            ("StartTimestamp" .=) <$> _dashtrStartTimestamp
          ]
      )
