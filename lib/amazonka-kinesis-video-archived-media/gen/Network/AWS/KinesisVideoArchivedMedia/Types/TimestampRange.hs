{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The range of timestamps for which to return fragments.
--
--
--
-- /See:/ 'timestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { _trStartTimestamp :: !POSIX,
    _trEndTimestamp :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trStartTimestamp' - The starting timestamp in the range of timestamps for which to return fragments.
--
-- * 'trEndTimestamp' - The ending timestamp in the range of timestamps for which to return fragments.
timestampRange ::
  -- | 'trStartTimestamp'
  UTCTime ->
  -- | 'trEndTimestamp'
  UTCTime ->
  TimestampRange
timestampRange pStartTimestamp_ pEndTimestamp_ =
  TimestampRange'
    { _trStartTimestamp = _Time # pStartTimestamp_,
      _trEndTimestamp = _Time # pEndTimestamp_
    }

-- | The starting timestamp in the range of timestamps for which to return fragments.
trStartTimestamp :: Lens' TimestampRange UTCTime
trStartTimestamp = lens _trStartTimestamp (\s a -> s {_trStartTimestamp = a}) . _Time

-- | The ending timestamp in the range of timestamps for which to return fragments.
trEndTimestamp :: Lens' TimestampRange UTCTime
trEndTimestamp = lens _trEndTimestamp (\s a -> s {_trEndTimestamp = a}) . _Time

instance Hashable TimestampRange

instance NFData TimestampRange

instance ToJSON TimestampRange where
  toJSON TimestampRange' {..} =
    object
      ( catMaybes
          [ Just ("StartTimestamp" .= _trStartTimestamp),
            Just ("EndTimestamp" .= _trEndTimestamp)
          ]
      )
