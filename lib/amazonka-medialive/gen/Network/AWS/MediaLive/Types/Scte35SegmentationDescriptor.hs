{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
import Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
import Network.AWS.Prelude

-- | Corresponds to SCTE-35 segmentation_descriptor.
--
-- /See:/ 'scte35SegmentationDescriptor' smart constructor.
data Scte35SegmentationDescriptor = Scte35SegmentationDescriptor'
  { _ssdSegmentationUpidType ::
      !(Maybe Nat),
    _ssdSegmentsExpected ::
      !(Maybe Nat),
    _ssdSubSegmentsExpected ::
      !(Maybe Nat),
    _ssdSegmentNum :: !(Maybe Nat),
    _ssdSegmentationDuration ::
      !(Maybe Nat),
    _ssdSegmentationTypeId ::
      !(Maybe Nat),
    _ssdDeliveryRestrictions ::
      !( Maybe
           Scte35DeliveryRestrictions
       ),
    _ssdSegmentationUpid ::
      !(Maybe Text),
    _ssdSubSegmentNum :: !(Maybe Nat),
    _ssdSegmentationEventId :: !Nat,
    _ssdSegmentationCancelIndicator ::
      !Scte35SegmentationCancelIndicator
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35SegmentationDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdSegmentationUpidType' - Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
--
-- * 'ssdSegmentsExpected' - Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
--
-- * 'ssdSubSegmentsExpected' - Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
--
-- * 'ssdSegmentNum' - Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
--
-- * 'ssdSegmentationDuration' - Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
--
-- * 'ssdSegmentationTypeId' - Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
--
-- * 'ssdDeliveryRestrictions' - Holds the four SCTE-35 delivery restriction parameters.
--
-- * 'ssdSegmentationUpid' - Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
--
-- * 'ssdSubSegmentNum' - Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
--
-- * 'ssdSegmentationEventId' - Corresponds to SCTE-35 segmentation_event_id.
--
-- * 'ssdSegmentationCancelIndicator' - Corresponds to SCTE-35 segmentation_event_cancel_indicator.
scte35SegmentationDescriptor ::
  -- | 'ssdSegmentationEventId'
  Natural ->
  -- | 'ssdSegmentationCancelIndicator'
  Scte35SegmentationCancelIndicator ->
  Scte35SegmentationDescriptor
scte35SegmentationDescriptor
  pSegmentationEventId_
  pSegmentationCancelIndicator_ =
    Scte35SegmentationDescriptor'
      { _ssdSegmentationUpidType = Nothing,
        _ssdSegmentsExpected = Nothing,
        _ssdSubSegmentsExpected = Nothing,
        _ssdSegmentNum = Nothing,
        _ssdSegmentationDuration = Nothing,
        _ssdSegmentationTypeId = Nothing,
        _ssdDeliveryRestrictions = Nothing,
        _ssdSegmentationUpid = Nothing,
        _ssdSubSegmentNum = Nothing,
        _ssdSegmentationEventId = _Nat # pSegmentationEventId_,
        _ssdSegmentationCancelIndicator = pSegmentationCancelIndicator_
      }

-- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
ssdSegmentationUpidType :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentationUpidType = lens _ssdSegmentationUpidType (\s a -> s {_ssdSegmentationUpidType = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
ssdSegmentsExpected :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentsExpected = lens _ssdSegmentsExpected (\s a -> s {_ssdSegmentsExpected = a}) . mapping _Nat

-- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
ssdSubSegmentsExpected :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSubSegmentsExpected = lens _ssdSubSegmentsExpected (\s a -> s {_ssdSubSegmentsExpected = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
ssdSegmentNum :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentNum = lens _ssdSegmentNum (\s a -> s {_ssdSegmentNum = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
ssdSegmentationDuration :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentationDuration = lens _ssdSegmentationDuration (\s a -> s {_ssdSegmentationDuration = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
ssdSegmentationTypeId :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentationTypeId = lens _ssdSegmentationTypeId (\s a -> s {_ssdSegmentationTypeId = a}) . mapping _Nat

-- | Holds the four SCTE-35 delivery restriction parameters.
ssdDeliveryRestrictions :: Lens' Scte35SegmentationDescriptor (Maybe Scte35DeliveryRestrictions)
ssdDeliveryRestrictions = lens _ssdDeliveryRestrictions (\s a -> s {_ssdDeliveryRestrictions = a})

-- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
ssdSegmentationUpid :: Lens' Scte35SegmentationDescriptor (Maybe Text)
ssdSegmentationUpid = lens _ssdSegmentationUpid (\s a -> s {_ssdSegmentationUpid = a})

-- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
ssdSubSegmentNum :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSubSegmentNum = lens _ssdSubSegmentNum (\s a -> s {_ssdSubSegmentNum = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segmentation_event_id.
ssdSegmentationEventId :: Lens' Scte35SegmentationDescriptor Natural
ssdSegmentationEventId = lens _ssdSegmentationEventId (\s a -> s {_ssdSegmentationEventId = a}) . _Nat

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
ssdSegmentationCancelIndicator :: Lens' Scte35SegmentationDescriptor Scte35SegmentationCancelIndicator
ssdSegmentationCancelIndicator = lens _ssdSegmentationCancelIndicator (\s a -> s {_ssdSegmentationCancelIndicator = a})

instance FromJSON Scte35SegmentationDescriptor where
  parseJSON =
    withObject
      "Scte35SegmentationDescriptor"
      ( \x ->
          Scte35SegmentationDescriptor'
            <$> (x .:? "segmentationUpidType")
            <*> (x .:? "segmentsExpected")
            <*> (x .:? "subSegmentsExpected")
            <*> (x .:? "segmentNum")
            <*> (x .:? "segmentationDuration")
            <*> (x .:? "segmentationTypeId")
            <*> (x .:? "deliveryRestrictions")
            <*> (x .:? "segmentationUpid")
            <*> (x .:? "subSegmentNum")
            <*> (x .: "segmentationEventId")
            <*> (x .: "segmentationCancelIndicator")
      )

instance Hashable Scte35SegmentationDescriptor

instance NFData Scte35SegmentationDescriptor

instance ToJSON Scte35SegmentationDescriptor where
  toJSON Scte35SegmentationDescriptor' {..} =
    object
      ( catMaybes
          [ ("segmentationUpidType" .=) <$> _ssdSegmentationUpidType,
            ("segmentsExpected" .=) <$> _ssdSegmentsExpected,
            ("subSegmentsExpected" .=) <$> _ssdSubSegmentsExpected,
            ("segmentNum" .=) <$> _ssdSegmentNum,
            ("segmentationDuration" .=) <$> _ssdSegmentationDuration,
            ("segmentationTypeId" .=) <$> _ssdSegmentationTypeId,
            ("deliveryRestrictions" .=) <$> _ssdDeliveryRestrictions,
            ("segmentationUpid" .=) <$> _ssdSegmentationUpid,
            ("subSegmentNum" .=) <$> _ssdSubSegmentNum,
            Just ("segmentationEventId" .= _ssdSegmentationEventId),
            Just
              ( "segmentationCancelIndicator"
                  .= _ssdSegmentationCancelIndicator
              )
          ]
      )
