{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillarySourceSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AncillaryConvert608To708
import Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
import Network.AWS.Prelude

-- | Settings for ancillary captions source.
--
-- /See:/ 'ancillarySourceSettings' smart constructor.
data AncillarySourceSettings = AncillarySourceSettings'
  { _assConvert608To708 ::
      !(Maybe AncillaryConvert608To708),
    _assTerminateCaptions ::
      !(Maybe AncillaryTerminateCaptions),
    _assSourceAncillaryChannelNumber ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AncillarySourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assConvert608To708' - Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
--
-- * 'assTerminateCaptions' - By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
--
-- * 'assSourceAncillaryChannelNumber' - Specifies the 608 channel number in the ancillary data track from which to extract captions. Unused for passthrough.
ancillarySourceSettings ::
  AncillarySourceSettings
ancillarySourceSettings =
  AncillarySourceSettings'
    { _assConvert608To708 = Nothing,
      _assTerminateCaptions = Nothing,
      _assSourceAncillaryChannelNumber = Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
assConvert608To708 :: Lens' AncillarySourceSettings (Maybe AncillaryConvert608To708)
assConvert608To708 = lens _assConvert608To708 (\s a -> s {_assConvert608To708 = a})

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
assTerminateCaptions :: Lens' AncillarySourceSettings (Maybe AncillaryTerminateCaptions)
assTerminateCaptions = lens _assTerminateCaptions (\s a -> s {_assTerminateCaptions = a})

-- | Specifies the 608 channel number in the ancillary data track from which to extract captions. Unused for passthrough.
assSourceAncillaryChannelNumber :: Lens' AncillarySourceSettings (Maybe Natural)
assSourceAncillaryChannelNumber = lens _assSourceAncillaryChannelNumber (\s a -> s {_assSourceAncillaryChannelNumber = a}) . mapping _Nat

instance FromJSON AncillarySourceSettings where
  parseJSON =
    withObject
      "AncillarySourceSettings"
      ( \x ->
          AncillarySourceSettings'
            <$> (x .:? "convert608To708")
            <*> (x .:? "terminateCaptions")
            <*> (x .:? "sourceAncillaryChannelNumber")
      )

instance Hashable AncillarySourceSettings

instance NFData AncillarySourceSettings

instance ToJSON AncillarySourceSettings where
  toJSON AncillarySourceSettings' {..} =
    object
      ( catMaybes
          [ ("convert608To708" .=) <$> _assConvert608To708,
            ("terminateCaptions" .=) <$> _assTerminateCaptions,
            ("sourceAncillaryChannelNumber" .=)
              <$> _assSourceAncillaryChannelNumber
          ]
      )
