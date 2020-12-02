{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector where

import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the range of timestamps for the requested media, and the source of the timestamps.
--
--
--
-- /See:/ 'dASHFragmentSelector' smart constructor.
data DASHFragmentSelector = DASHFragmentSelector'
  { _dashfsFragmentSelectorType ::
      !(Maybe DASHFragmentSelectorType),
    _dashfsTimestampRange ::
      !(Maybe DASHTimestampRange)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DASHFragmentSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dashfsFragmentSelectorType' - The source of the timestamps for the requested media. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetDASHStreamingSessionURLInput$PlaybackMode' is @ON_DEMAND@ or @LIVE_REPLAY@ , the first fragment ingested with a producer timestamp within the specified 'FragmentSelector$TimestampRange' is included in the media playlist. In addition, the fragments with producer timestamps within the @TimestampRange@ ingested immediately following the first fragment (up to the 'GetDASHStreamingSessionURLInput$MaxManifestFragmentResults' value) are included.  Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the MPEG-DASH manifest will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetDASHStreamingSessionURLInput$PlaybackMode' is @LIVE@ , the producer timestamps are used in the MP4 fragments and for deduplication. But the most recently ingested fragments based on server timestamps are included in the MPEG-DASH manifest. This means that even if fragments ingested in the past have producer timestamps with values now, they are not included in the HLS media playlist. The default is @SERVER_TIMESTAMP@ .
--
-- * 'dashfsTimestampRange' - The start and end of the timestamp range for the requested media. This value should not be present if @PlaybackType@ is @LIVE@ .
dASHFragmentSelector ::
  DASHFragmentSelector
dASHFragmentSelector =
  DASHFragmentSelector'
    { _dashfsFragmentSelectorType = Nothing,
      _dashfsTimestampRange = Nothing
    }

-- | The source of the timestamps for the requested media. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetDASHStreamingSessionURLInput$PlaybackMode' is @ON_DEMAND@ or @LIVE_REPLAY@ , the first fragment ingested with a producer timestamp within the specified 'FragmentSelector$TimestampRange' is included in the media playlist. In addition, the fragments with producer timestamps within the @TimestampRange@ ingested immediately following the first fragment (up to the 'GetDASHStreamingSessionURLInput$MaxManifestFragmentResults' value) are included.  Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the MPEG-DASH manifest will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetDASHStreamingSessionURLInput$PlaybackMode' is @LIVE@ , the producer timestamps are used in the MP4 fragments and for deduplication. But the most recently ingested fragments based on server timestamps are included in the MPEG-DASH manifest. This means that even if fragments ingested in the past have producer timestamps with values now, they are not included in the HLS media playlist. The default is @SERVER_TIMESTAMP@ .
dashfsFragmentSelectorType :: Lens' DASHFragmentSelector (Maybe DASHFragmentSelectorType)
dashfsFragmentSelectorType = lens _dashfsFragmentSelectorType (\s a -> s {_dashfsFragmentSelectorType = a})

-- | The start and end of the timestamp range for the requested media. This value should not be present if @PlaybackType@ is @LIVE@ .
dashfsTimestampRange :: Lens' DASHFragmentSelector (Maybe DASHTimestampRange)
dashfsTimestampRange = lens _dashfsTimestampRange (\s a -> s {_dashfsTimestampRange = a})

instance Hashable DASHFragmentSelector

instance NFData DASHFragmentSelector

instance ToJSON DASHFragmentSelector where
  toJSON DASHFragmentSelector' {..} =
    object
      ( catMaybes
          [ ("FragmentSelectorType" .=) <$> _dashfsFragmentSelectorType,
            ("TimestampRange" .=) <$> _dashfsTimestampRange
          ]
      )
