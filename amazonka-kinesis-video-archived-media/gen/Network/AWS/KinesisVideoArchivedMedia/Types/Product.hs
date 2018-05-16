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
-- * 'fServerTimestamp' - The time stamp from the AWS server corresponding to the fragment.
--
-- * 'fFragmentSizeInBytes' - The total fragment size, including information about the fragment and contained media data.
--
-- * 'fFragmentNumber' - The index value of the fragment.
--
-- * 'fProducerTimestamp' - The time stamp from the producer corresponding to the fragment.
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

-- | The time stamp from the AWS server corresponding to the fragment.
fServerTimestamp :: Lens' Fragment (Maybe UTCTime)
fServerTimestamp = lens _fServerTimestamp (\ s a -> s{_fServerTimestamp = a}) . mapping _Time

-- | The total fragment size, including information about the fragment and contained media data.
fFragmentSizeInBytes :: Lens' Fragment (Maybe Integer)
fFragmentSizeInBytes = lens _fFragmentSizeInBytes (\ s a -> s{_fFragmentSizeInBytes = a})

-- | The index value of the fragment.
fFragmentNumber :: Lens' Fragment (Maybe Text)
fFragmentNumber = lens _fFragmentNumber (\ s a -> s{_fFragmentNumber = a})

-- | The time stamp from the producer corresponding to the fragment.
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

-- | Describes the time stamp range and time stamp origin of a range of fragments.
--
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
-- * 'fsFragmentSelectorType' - The origin of the time stamps to use (Server or Producer).
--
-- * 'fsTimestampRange' - The range of time stamps to return.
fragmentSelector
    :: FragmentSelectorType -- ^ 'fsFragmentSelectorType'
    -> TimestampRange -- ^ 'fsTimestampRange'
    -> FragmentSelector
fragmentSelector pFragmentSelectorType_ pTimestampRange_ =
  FragmentSelector'
    { _fsFragmentSelectorType = pFragmentSelectorType_
    , _fsTimestampRange = pTimestampRange_
    }


-- | The origin of the time stamps to use (Server or Producer).
fsFragmentSelectorType :: Lens' FragmentSelector FragmentSelectorType
fsFragmentSelectorType = lens _fsFragmentSelectorType (\ s a -> s{_fsFragmentSelectorType = a})

-- | The range of time stamps to return.
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

-- | The range of time stamps for which to return fragments.
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
-- * 'trStartTimestamp' - The starting time stamp in the range of time stamps for which to return fragments.
--
-- * 'trEndTimestamp' - The ending time stamp in the range of time stamps for which to return fragments.
timestampRange
    :: UTCTime -- ^ 'trStartTimestamp'
    -> UTCTime -- ^ 'trEndTimestamp'
    -> TimestampRange
timestampRange pStartTimestamp_ pEndTimestamp_ =
  TimestampRange'
    { _trStartTimestamp = _Time # pStartTimestamp_
    , _trEndTimestamp = _Time # pEndTimestamp_
    }


-- | The starting time stamp in the range of time stamps for which to return fragments.
trStartTimestamp :: Lens' TimestampRange UTCTime
trStartTimestamp = lens _trStartTimestamp (\ s a -> s{_trStartTimestamp = a}) . _Time

-- | The ending time stamp in the range of time stamps for which to return fragments.
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
