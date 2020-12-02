{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSourceSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AncillarySourceSettings
import Network.AWS.MediaConvert.Types.CaptionSourceType
import Network.AWS.MediaConvert.Types.DvbSubSourceSettings
import Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
import Network.AWS.MediaConvert.Types.FileSourceSettings
import Network.AWS.MediaConvert.Types.TeletextSourceSettings
import Network.AWS.MediaConvert.Types.TrackSourceSettings
import Network.AWS.Prelude

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml file, specify the URI of the input captions source file. If your input captions are IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /See:/ 'captionSourceSettings' smart constructor.
data CaptionSourceSettings = CaptionSourceSettings'
  { _cssTeletextSourceSettings ::
      !(Maybe TeletextSourceSettings),
    _cssSourceType :: !(Maybe CaptionSourceType),
    _cssFileSourceSettings ::
      !(Maybe FileSourceSettings),
    _cssDvbSubSourceSettings ::
      !(Maybe DvbSubSourceSettings),
    _cssTrackSourceSettings ::
      !(Maybe TrackSourceSettings),
    _cssAncillarySourceSettings ::
      !(Maybe AncillarySourceSettings),
    _cssEmbeddedSourceSettings ::
      !(Maybe EmbeddedSourceSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssTeletextSourceSettings' - Settings specific to Teletext caption sources, including Page number.
--
-- * 'cssSourceType' - Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
--
-- * 'cssFileSourceSettings' - If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- * 'cssDvbSubSourceSettings' - DVB Sub Source Settings
--
-- * 'cssTrackSourceSettings' - Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
--
-- * 'cssAncillarySourceSettings' - Settings for ancillary captions source.
--
-- * 'cssEmbeddedSourceSettings' - Settings for embedded captions Source
captionSourceSettings ::
  CaptionSourceSettings
captionSourceSettings =
  CaptionSourceSettings'
    { _cssTeletextSourceSettings = Nothing,
      _cssSourceType = Nothing,
      _cssFileSourceSettings = Nothing,
      _cssDvbSubSourceSettings = Nothing,
      _cssTrackSourceSettings = Nothing,
      _cssAncillarySourceSettings = Nothing,
      _cssEmbeddedSourceSettings = Nothing
    }

-- | Settings specific to Teletext caption sources, including Page number.
cssTeletextSourceSettings :: Lens' CaptionSourceSettings (Maybe TeletextSourceSettings)
cssTeletextSourceSettings = lens _cssTeletextSourceSettings (\s a -> s {_cssTeletextSourceSettings = a})

-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
cssSourceType :: Lens' CaptionSourceSettings (Maybe CaptionSourceType)
cssSourceType = lens _cssSourceType (\s a -> s {_cssSourceType = a})

-- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
cssFileSourceSettings :: Lens' CaptionSourceSettings (Maybe FileSourceSettings)
cssFileSourceSettings = lens _cssFileSourceSettings (\s a -> s {_cssFileSourceSettings = a})

-- | DVB Sub Source Settings
cssDvbSubSourceSettings :: Lens' CaptionSourceSettings (Maybe DvbSubSourceSettings)
cssDvbSubSourceSettings = lens _cssDvbSubSourceSettings (\s a -> s {_cssDvbSubSourceSettings = a})

-- | Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
cssTrackSourceSettings :: Lens' CaptionSourceSettings (Maybe TrackSourceSettings)
cssTrackSourceSettings = lens _cssTrackSourceSettings (\s a -> s {_cssTrackSourceSettings = a})

-- | Settings for ancillary captions source.
cssAncillarySourceSettings :: Lens' CaptionSourceSettings (Maybe AncillarySourceSettings)
cssAncillarySourceSettings = lens _cssAncillarySourceSettings (\s a -> s {_cssAncillarySourceSettings = a})

-- | Settings for embedded captions Source
cssEmbeddedSourceSettings :: Lens' CaptionSourceSettings (Maybe EmbeddedSourceSettings)
cssEmbeddedSourceSettings = lens _cssEmbeddedSourceSettings (\s a -> s {_cssEmbeddedSourceSettings = a})

instance FromJSON CaptionSourceSettings where
  parseJSON =
    withObject
      "CaptionSourceSettings"
      ( \x ->
          CaptionSourceSettings'
            <$> (x .:? "teletextSourceSettings")
            <*> (x .:? "sourceType")
            <*> (x .:? "fileSourceSettings")
            <*> (x .:? "dvbSubSourceSettings")
            <*> (x .:? "trackSourceSettings")
            <*> (x .:? "ancillarySourceSettings")
            <*> (x .:? "embeddedSourceSettings")
      )

instance Hashable CaptionSourceSettings

instance NFData CaptionSourceSettings

instance ToJSON CaptionSourceSettings where
  toJSON CaptionSourceSettings' {..} =
    object
      ( catMaybes
          [ ("teletextSourceSettings" .=) <$> _cssTeletextSourceSettings,
            ("sourceType" .=) <$> _cssSourceType,
            ("fileSourceSettings" .=) <$> _cssFileSourceSettings,
            ("dvbSubSourceSettings" .=) <$> _cssDvbSubSourceSettings,
            ("trackSourceSettings" .=) <$> _cssTrackSourceSettings,
            ("ancillarySourceSettings" .=) <$> _cssAncillarySourceSettings,
            ("embeddedSourceSettings" .=) <$> _cssEmbeddedSourceSettings
          ]
      )
