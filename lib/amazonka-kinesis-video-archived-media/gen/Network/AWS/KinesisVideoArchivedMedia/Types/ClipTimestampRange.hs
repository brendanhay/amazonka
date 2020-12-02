{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The range of timestamps for which to return fragments.
--
--
-- The values in the ClipTimestampRange are @inclusive@ . Fragments that begin before the start time but continue past it, or fragments that begin before the end time but continue past it, are included in the session.
--
--
-- /See:/ 'clipTimestampRange' smart constructor.
data ClipTimestampRange = ClipTimestampRange'
  { _ctrStartTimestamp ::
      !POSIX,
    _ctrEndTimestamp :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClipTimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrStartTimestamp' - The starting timestamp in the range of timestamps for which to return fragments.  This value is inclusive. Fragments that start before the @StartTimestamp@ and continue past it are included in the session. If @FragmentSelectorType@ is @SERVER_TIMESTAMP@ , the @StartTimestamp@ must be later than the stream head.
--
-- * 'ctrEndTimestamp' - The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past.  This value is inclusive. The @EndTimestamp@ is compared to the (starting) timestamp of the fragment. Fragments that start before the @EndTimestamp@ value and continue past it are included in the session.
clipTimestampRange ::
  -- | 'ctrStartTimestamp'
  UTCTime ->
  -- | 'ctrEndTimestamp'
  UTCTime ->
  ClipTimestampRange
clipTimestampRange pStartTimestamp_ pEndTimestamp_ =
  ClipTimestampRange'
    { _ctrStartTimestamp =
        _Time # pStartTimestamp_,
      _ctrEndTimestamp = _Time # pEndTimestamp_
    }

-- | The starting timestamp in the range of timestamps for which to return fragments.  This value is inclusive. Fragments that start before the @StartTimestamp@ and continue past it are included in the session. If @FragmentSelectorType@ is @SERVER_TIMESTAMP@ , the @StartTimestamp@ must be later than the stream head.
ctrStartTimestamp :: Lens' ClipTimestampRange UTCTime
ctrStartTimestamp = lens _ctrStartTimestamp (\s a -> s {_ctrStartTimestamp = a}) . _Time

-- | The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past.  This value is inclusive. The @EndTimestamp@ is compared to the (starting) timestamp of the fragment. Fragments that start before the @EndTimestamp@ value and continue past it are included in the session.
ctrEndTimestamp :: Lens' ClipTimestampRange UTCTime
ctrEndTimestamp = lens _ctrEndTimestamp (\s a -> s {_ctrEndTimestamp = a}) . _Time

instance Hashable ClipTimestampRange

instance NFData ClipTimestampRange

instance ToJSON ClipTimestampRange where
  toJSON ClipTimestampRange' {..} =
    object
      ( catMaybes
          [ Just ("StartTimestamp" .= _ctrStartTimestamp),
            Just ("EndTimestamp" .= _ctrEndTimestamp)
          ]
      )
