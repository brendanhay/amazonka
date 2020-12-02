{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelector where

import Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the range of timestamps for the requested media, and the source of the timestamps.
--
--
--
-- /See:/ 'hLSFragmentSelector' smart constructor.
data HLSFragmentSelector = HLSFragmentSelector'
  { _hlsfsFragmentSelectorType ::
      !(Maybe HLSFragmentSelectorType),
    _hlsfsTimestampRange :: !(Maybe HLSTimestampRange)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HLSFragmentSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlsfsFragmentSelectorType' - The source of the timestamps for the requested media. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetHLSStreamingSessionURLInput$PlaybackMode' is @ON_DEMAND@ or @LIVE_REPLAY@ , the first fragment ingested with a producer timestamp within the specified 'FragmentSelector$TimestampRange' is included in the media playlist. In addition, the fragments with producer timestamps within the @TimestampRange@ ingested immediately following the first fragment (up to the 'GetHLSStreamingSessionURLInput$MaxMediaPlaylistFragmentResults' value) are included.  Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the HLS media playlists will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetHLSStreamingSessionURLInput$PlaybackMode' is @LIVE@ , the producer timestamps are used in the MP4 fragments and for deduplication. But the most recently ingested fragments based on server timestamps are included in the HLS media playlist. This means that even if fragments ingested in the past have producer timestamps with values now, they are not included in the HLS media playlist. The default is @SERVER_TIMESTAMP@ .
--
-- * 'hlsfsTimestampRange' - The start and end of the timestamp range for the requested media. This value should not be present if @PlaybackType@ is @LIVE@ .
hLSFragmentSelector ::
  HLSFragmentSelector
hLSFragmentSelector =
  HLSFragmentSelector'
    { _hlsfsFragmentSelectorType = Nothing,
      _hlsfsTimestampRange = Nothing
    }

-- | The source of the timestamps for the requested media. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetHLSStreamingSessionURLInput$PlaybackMode' is @ON_DEMAND@ or @LIVE_REPLAY@ , the first fragment ingested with a producer timestamp within the specified 'FragmentSelector$TimestampRange' is included in the media playlist. In addition, the fragments with producer timestamps within the @TimestampRange@ ingested immediately following the first fragment (up to the 'GetHLSStreamingSessionURLInput$MaxMediaPlaylistFragmentResults' value) are included.  Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the HLS media playlists will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetHLSStreamingSessionURLInput$PlaybackMode' is @LIVE@ , the producer timestamps are used in the MP4 fragments and for deduplication. But the most recently ingested fragments based on server timestamps are included in the HLS media playlist. This means that even if fragments ingested in the past have producer timestamps with values now, they are not included in the HLS media playlist. The default is @SERVER_TIMESTAMP@ .
hlsfsFragmentSelectorType :: Lens' HLSFragmentSelector (Maybe HLSFragmentSelectorType)
hlsfsFragmentSelectorType = lens _hlsfsFragmentSelectorType (\s a -> s {_hlsfsFragmentSelectorType = a})

-- | The start and end of the timestamp range for the requested media. This value should not be present if @PlaybackType@ is @LIVE@ .
hlsfsTimestampRange :: Lens' HLSFragmentSelector (Maybe HLSTimestampRange)
hlsfsTimestampRange = lens _hlsfsTimestampRange (\s a -> s {_hlsfsTimestampRange = a})

instance Hashable HLSFragmentSelector

instance NFData HLSFragmentSelector

instance ToJSON HLSFragmentSelector where
  toJSON HLSFragmentSelector' {..} =
    object
      ( catMaybes
          [ ("FragmentSelectorType" .=) <$> _hlsfsFragmentSelectorType,
            ("TimestampRange" .=) <$> _hlsfsTimestampRange
          ]
      )
