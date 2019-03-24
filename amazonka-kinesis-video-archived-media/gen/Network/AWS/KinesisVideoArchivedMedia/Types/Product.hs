{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.Product where

import Network.AWS.KinesisVideoArchivedMedia.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a segment of video or other time-delimited data.
--
--
--
-- /See:/ 'fragment' smart constructor.
data Fragment = Fragment'
  { _fFragmentLengthInMilliseconds :: !(Maybe Integer)
  , _fServerTimestamp              :: !(Maybe POSIX)
  , _fFragmentSizeInBytes          :: !(Maybe Integer)
  , _fFragmentNumber               :: !(Maybe Text)
  , _fProducerTimestamp            :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Fragment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFragmentLengthInMilliseconds' - The playback duration or other time value associated with the fragment.
--
-- * 'fServerTimestamp' - The timestamp from the AWS server corresponding to the fragment.
--
-- * 'fFragmentSizeInBytes' - The total fragment size, including information about the fragment and contained media data.
--
-- * 'fFragmentNumber' - The index value of the fragment.
--
-- * 'fProducerTimestamp' - The timestamp from the producer corresponding to the fragment.
fragment
    :: Fragment
fragment =
  Fragment'
    { _fFragmentLengthInMilliseconds = Nothing
    , _fServerTimestamp = Nothing
    , _fFragmentSizeInBytes = Nothing
    , _fFragmentNumber = Nothing
    , _fProducerTimestamp = Nothing
    }


-- | The playback duration or other time value associated with the fragment.
fFragmentLengthInMilliseconds :: Lens' Fragment (Maybe Integer)
fFragmentLengthInMilliseconds = lens _fFragmentLengthInMilliseconds (\ s a -> s{_fFragmentLengthInMilliseconds = a})

-- | The timestamp from the AWS server corresponding to the fragment.
fServerTimestamp :: Lens' Fragment (Maybe UTCTime)
fServerTimestamp = lens _fServerTimestamp (\ s a -> s{_fServerTimestamp = a}) . mapping _Time

-- | The total fragment size, including information about the fragment and contained media data.
fFragmentSizeInBytes :: Lens' Fragment (Maybe Integer)
fFragmentSizeInBytes = lens _fFragmentSizeInBytes (\ s a -> s{_fFragmentSizeInBytes = a})

-- | The index value of the fragment.
fFragmentNumber :: Lens' Fragment (Maybe Text)
fFragmentNumber = lens _fFragmentNumber (\ s a -> s{_fFragmentNumber = a})

-- | The timestamp from the producer corresponding to the fragment.
fProducerTimestamp :: Lens' Fragment (Maybe UTCTime)
fProducerTimestamp = lens _fProducerTimestamp (\ s a -> s{_fProducerTimestamp = a}) . mapping _Time

instance FromJSON Fragment where
        parseJSON
          = withObject "Fragment"
              (\ x ->
                 Fragment' <$>
                   (x .:? "FragmentLengthInMilliseconds") <*>
                     (x .:? "ServerTimestamp")
                     <*> (x .:? "FragmentSizeInBytes")
                     <*> (x .:? "FragmentNumber")
                     <*> (x .:? "ProducerTimestamp"))

instance Hashable Fragment where

instance NFData Fragment where

-- | Describes the timestamp range and timestamp origin of a range of fragments.
--
--
-- Only fragments with a start timestamp greater than or equal to the given start time and less than or equal to the end time are returned. For example, if a stream contains fragments with the following start timestamps:
--
--     * 00:00:00
--
--     * 00:00:02
--
--     * 00:00:04
--
--     * 00:00:06
--
--
--
-- A fragment selector range with a start time of 00:00:01 and end time of 00:00:04 would return the fragments with start times of 00:00:02 and 00:00:04.
--
--
-- /See:/ 'fragmentSelector' smart constructor.
data FragmentSelector = FragmentSelector'
  { _fsFragmentSelectorType :: !FragmentSelectorType
  , _fsTimestampRange       :: !TimestampRange
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FragmentSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsFragmentSelectorType' - The origin of the timestamps to use (Server or Producer).
--
-- * 'fsTimestampRange' - The range of timestamps to return.
fragmentSelector
    :: FragmentSelectorType -- ^ 'fsFragmentSelectorType'
    -> TimestampRange -- ^ 'fsTimestampRange'
    -> FragmentSelector
fragmentSelector pFragmentSelectorType_ pTimestampRange_ =
  FragmentSelector'
    { _fsFragmentSelectorType = pFragmentSelectorType_
    , _fsTimestampRange = pTimestampRange_
    }


-- | The origin of the timestamps to use (Server or Producer).
fsFragmentSelectorType :: Lens' FragmentSelector FragmentSelectorType
fsFragmentSelectorType = lens _fsFragmentSelectorType (\ s a -> s{_fsFragmentSelectorType = a})

-- | The range of timestamps to return.
fsTimestampRange :: Lens' FragmentSelector TimestampRange
fsTimestampRange = lens _fsTimestampRange (\ s a -> s{_fsTimestampRange = a})

instance Hashable FragmentSelector where

instance NFData FragmentSelector where

instance ToJSON FragmentSelector where
        toJSON FragmentSelector'{..}
          = object
              (catMaybes
                 [Just
                    ("FragmentSelectorType" .= _fsFragmentSelectorType),
                  Just ("TimestampRange" .= _fsTimestampRange)])

-- | Contains the range of timestamps for the requested media, and the source of the timestamps.
--
--
--
-- /See:/ 'hLSFragmentSelector' smart constructor.
data HLSFragmentSelector = HLSFragmentSelector'
  { _hlsfsFragmentSelectorType :: !(Maybe HLSFragmentSelectorType)
  , _hlsfsTimestampRange       :: !(Maybe HLSTimestampRange)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HLSFragmentSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlsfsFragmentSelectorType' - The source of the timestamps for the requested media. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetHLSStreamingSessionURLInput$PlaybackMode' is @ON_DEMAND@ , the first fragment ingested with a producer timestamp within the specified 'FragmentSelector$TimestampRange' is included in the media playlist. In addition, the fragments with producer timestamps within the @TimestampRange@ ingested immediately following the first fragment (up to the 'GetHLSStreamingSessionURLInput$MaxMediaPlaylistFragmentResults' value) are included.  Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the HLS media playlists will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetHLSStreamingSessionURLInput$PlaybackMode' is @LIVE@ , the producer timestamps are used in the MP4 fragments and for deduplication. But the most recently ingested fragments based on server timestamps are included in the HLS media playlist. This means that even if fragments ingested in the past have producer timestamps with values now, they are not included in the HLS media playlist. The default is @SERVER_TIMESTAMP@ .
--
-- * 'hlsfsTimestampRange' - The start and end of the timestamp range for the requested media. This value should not be present if @PlaybackType@ is @LIVE@ .
hLSFragmentSelector
    :: HLSFragmentSelector
hLSFragmentSelector =
  HLSFragmentSelector'
    {_hlsfsFragmentSelectorType = Nothing, _hlsfsTimestampRange = Nothing}


-- | The source of the timestamps for the requested media. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetHLSStreamingSessionURLInput$PlaybackMode' is @ON_DEMAND@ , the first fragment ingested with a producer timestamp within the specified 'FragmentSelector$TimestampRange' is included in the media playlist. In addition, the fragments with producer timestamps within the @TimestampRange@ ingested immediately following the first fragment (up to the 'GetHLSStreamingSessionURLInput$MaxMediaPlaylistFragmentResults' value) are included.  Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the HLS media playlists will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned. When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetHLSStreamingSessionURLInput$PlaybackMode' is @LIVE@ , the producer timestamps are used in the MP4 fragments and for deduplication. But the most recently ingested fragments based on server timestamps are included in the HLS media playlist. This means that even if fragments ingested in the past have producer timestamps with values now, they are not included in the HLS media playlist. The default is @SERVER_TIMESTAMP@ .
hlsfsFragmentSelectorType :: Lens' HLSFragmentSelector (Maybe HLSFragmentSelectorType)
hlsfsFragmentSelectorType = lens _hlsfsFragmentSelectorType (\ s a -> s{_hlsfsFragmentSelectorType = a})

-- | The start and end of the timestamp range for the requested media. This value should not be present if @PlaybackType@ is @LIVE@ .
hlsfsTimestampRange :: Lens' HLSFragmentSelector (Maybe HLSTimestampRange)
hlsfsTimestampRange = lens _hlsfsTimestampRange (\ s a -> s{_hlsfsTimestampRange = a})

instance Hashable HLSFragmentSelector where

instance NFData HLSFragmentSelector where

instance ToJSON HLSFragmentSelector where
        toJSON HLSFragmentSelector'{..}
          = object
              (catMaybes
                 [("FragmentSelectorType" .=) <$>
                    _hlsfsFragmentSelectorType,
                  ("TimestampRange" .=) <$> _hlsfsTimestampRange])

-- | The start and end of the timestamp range for the requested media.
--
--
-- This value should not be present if @PlaybackType@ is @LIVE@ .
--
--
-- /See:/ 'hLSTimestampRange' smart constructor.
data HLSTimestampRange = HLSTimestampRange'
  { _hlstrEndTimestamp   :: !(Maybe POSIX)
  , _hlstrStartTimestamp :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HLSTimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlstrEndTimestamp' - The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past. If the @HLSTimestampRange@ value is specified, the @EndTimestamp@ value is required.
--
-- * 'hlstrStartTimestamp' - The start of the timestamp range for the requested media. If the @HLSTimestampRange@ value is specified, the @StartTimestamp@ value is required.
hLSTimestampRange
    :: HLSTimestampRange
hLSTimestampRange =
  HLSTimestampRange'
    {_hlstrEndTimestamp = Nothing, _hlstrStartTimestamp = Nothing}


-- | The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past. If the @HLSTimestampRange@ value is specified, the @EndTimestamp@ value is required.
hlstrEndTimestamp :: Lens' HLSTimestampRange (Maybe UTCTime)
hlstrEndTimestamp = lens _hlstrEndTimestamp (\ s a -> s{_hlstrEndTimestamp = a}) . mapping _Time

-- | The start of the timestamp range for the requested media. If the @HLSTimestampRange@ value is specified, the @StartTimestamp@ value is required.
hlstrStartTimestamp :: Lens' HLSTimestampRange (Maybe UTCTime)
hlstrStartTimestamp = lens _hlstrStartTimestamp (\ s a -> s{_hlstrStartTimestamp = a}) . mapping _Time

instance Hashable HLSTimestampRange where

instance NFData HLSTimestampRange where

instance ToJSON HLSTimestampRange where
        toJSON HLSTimestampRange'{..}
          = object
              (catMaybes
                 [("EndTimestamp" .=) <$> _hlstrEndTimestamp,
                  ("StartTimestamp" .=) <$> _hlstrStartTimestamp])

-- | The range of timestamps for which to return fragments.
--
--
--
-- /See:/ 'timestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { _trStartTimestamp :: !POSIX
  , _trEndTimestamp   :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimestampRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trStartTimestamp' - The starting timestamp in the range of timestamps for which to return fragments.
--
-- * 'trEndTimestamp' - The ending timestamp in the range of timestamps for which to return fragments.
timestampRange
    :: UTCTime -- ^ 'trStartTimestamp'
    -> UTCTime -- ^ 'trEndTimestamp'
    -> TimestampRange
timestampRange pStartTimestamp_ pEndTimestamp_ =
  TimestampRange'
    { _trStartTimestamp = _Time # pStartTimestamp_
    , _trEndTimestamp = _Time # pEndTimestamp_
    }


-- | The starting timestamp in the range of timestamps for which to return fragments.
trStartTimestamp :: Lens' TimestampRange UTCTime
trStartTimestamp = lens _trStartTimestamp (\ s a -> s{_trStartTimestamp = a}) . _Time

-- | The ending timestamp in the range of timestamps for which to return fragments.
trEndTimestamp :: Lens' TimestampRange UTCTime
trEndTimestamp = lens _trEndTimestamp (\ s a -> s{_trEndTimestamp = a}) . _Time

instance Hashable TimestampRange where

instance NFData TimestampRange where

instance ToJSON TimestampRange where
        toJSON TimestampRange'{..}
          = object
              (catMaybes
                 [Just ("StartTimestamp" .= _trStartTimestamp),
                  Just ("EndTimestamp" .= _trEndTimestamp)])
