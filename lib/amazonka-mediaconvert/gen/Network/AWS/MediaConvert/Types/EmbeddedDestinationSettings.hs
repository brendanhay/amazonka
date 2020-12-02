{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings specific to embedded/ancillary caption outputs, including 608/708 Channel destination number.
--
-- /See:/ 'embeddedDestinationSettings' smart constructor.
data EmbeddedDestinationSettings = EmbeddedDestinationSettings'
  { _edsDestination608ChannelNumber ::
      !(Maybe Nat),
    _edsDestination708ServiceNumber ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmbeddedDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edsDestination608ChannelNumber' - Ignore this setting unless your input captions are SCC format and your output captions are embedded in the video stream. Specify a CC number for each captions channel in this output. If you have two channels, choose CC numbers that aren't in the same field. For example, choose 1 and 3. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
--
-- * 'edsDestination708ServiceNumber' - Ignore this setting unless your input captions are SCC format and you want both 608 and 708 captions embedded in your output stream. Optionally, specify the 708 service number for each output captions channel. Choose a different number for each channel. To use this setting, also set Force 608 to 708 upconvert (Convert608To708) to Upconvert (UPCONVERT) in your input captions selector settings. If you choose to upconvert but don't specify a 708 service number, MediaConvert uses the number that you specify for CC channel number (destination608ChannelNumber) for the 708 service number. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
embeddedDestinationSettings ::
  EmbeddedDestinationSettings
embeddedDestinationSettings =
  EmbeddedDestinationSettings'
    { _edsDestination608ChannelNumber =
        Nothing,
      _edsDestination708ServiceNumber = Nothing
    }

-- | Ignore this setting unless your input captions are SCC format and your output captions are embedded in the video stream. Specify a CC number for each captions channel in this output. If you have two channels, choose CC numbers that aren't in the same field. For example, choose 1 and 3. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
edsDestination608ChannelNumber :: Lens' EmbeddedDestinationSettings (Maybe Natural)
edsDestination608ChannelNumber = lens _edsDestination608ChannelNumber (\s a -> s {_edsDestination608ChannelNumber = a}) . mapping _Nat

-- | Ignore this setting unless your input captions are SCC format and you want both 608 and 708 captions embedded in your output stream. Optionally, specify the 708 service number for each output captions channel. Choose a different number for each channel. To use this setting, also set Force 608 to 708 upconvert (Convert608To708) to Upconvert (UPCONVERT) in your input captions selector settings. If you choose to upconvert but don't specify a 708 service number, MediaConvert uses the number that you specify for CC channel number (destination608ChannelNumber) for the 708 service number. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
edsDestination708ServiceNumber :: Lens' EmbeddedDestinationSettings (Maybe Natural)
edsDestination708ServiceNumber = lens _edsDestination708ServiceNumber (\s a -> s {_edsDestination708ServiceNumber = a}) . mapping _Nat

instance FromJSON EmbeddedDestinationSettings where
  parseJSON =
    withObject
      "EmbeddedDestinationSettings"
      ( \x ->
          EmbeddedDestinationSettings'
            <$> (x .:? "destination608ChannelNumber")
            <*> (x .:? "destination708ServiceNumber")
      )

instance Hashable EmbeddedDestinationSettings

instance NFData EmbeddedDestinationSettings

instance ToJSON EmbeddedDestinationSettings where
  toJSON EmbeddedDestinationSettings' {..} =
    object
      ( catMaybes
          [ ("destination608ChannelNumber" .=)
              <$> _edsDestination608ChannelNumber,
            ("destination708ServiceNumber" .=)
              <$> _edsDestination708ServiceNumber
          ]
      )
