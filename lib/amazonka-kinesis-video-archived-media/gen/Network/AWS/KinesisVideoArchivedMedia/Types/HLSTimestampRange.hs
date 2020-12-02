{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The start and end of the timestamp range for the requested media.
--
--
-- This value should not be present if @PlaybackType@ is @LIVE@ .
--
--
-- /See:/ 'hLSTimestampRange' smart constructor.
data HLSTimestampRange = HLSTimestampRange'
  { _hlstrEndTimestamp ::
      !(Maybe POSIX),
    _hlstrStartTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HLSTimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlstrEndTimestamp' - The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past. The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for @LIVE_REPLAY@ mode then the session will continue to include newly ingested fragments until the session expires.
--
-- * 'hlstrStartTimestamp' - The start of the timestamp range for the requested media. If the @HLSTimestampRange@ value is specified, the @StartTimestamp@ value is required.
hLSTimestampRange ::
  HLSTimestampRange
hLSTimestampRange =
  HLSTimestampRange'
    { _hlstrEndTimestamp = Nothing,
      _hlstrStartTimestamp = Nothing
    }

-- | The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past. The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for @LIVE_REPLAY@ mode then the session will continue to include newly ingested fragments until the session expires.
hlstrEndTimestamp :: Lens' HLSTimestampRange (Maybe UTCTime)
hlstrEndTimestamp = lens _hlstrEndTimestamp (\s a -> s {_hlstrEndTimestamp = a}) . mapping _Time

-- | The start of the timestamp range for the requested media. If the @HLSTimestampRange@ value is specified, the @StartTimestamp@ value is required.
hlstrStartTimestamp :: Lens' HLSTimestampRange (Maybe UTCTime)
hlstrStartTimestamp = lens _hlstrStartTimestamp (\s a -> s {_hlstrStartTimestamp = a}) . mapping _Time

instance Hashable HLSTimestampRange

instance NFData HLSTimestampRange

instance ToJSON HLSTimestampRange where
  toJSON HLSTimestampRange' {..} =
    object
      ( catMaybes
          [ ("EndTimestamp" .=) <$> _hlstrEndTimestamp,
            ("StartTimestamp" .=) <$> _hlstrStartTimestamp
          ]
      )
