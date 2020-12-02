{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionSelectorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionSelectorSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AncillarySourceSettings
import Network.AWS.MediaLive.Types.AribSourceSettings
import Network.AWS.MediaLive.Types.DvbSubSourceSettings
import Network.AWS.MediaLive.Types.EmbeddedSourceSettings
import Network.AWS.MediaLive.Types.Scte20SourceSettings
import Network.AWS.MediaLive.Types.Scte27SourceSettings
import Network.AWS.MediaLive.Types.TeletextSourceSettings
import Network.AWS.Prelude

-- | Caption Selector Settings
--
-- /See:/ 'captionSelectorSettings' smart constructor.
data CaptionSelectorSettings = CaptionSelectorSettings'
  { _cssTeletextSourceSettings ::
      !(Maybe TeletextSourceSettings),
    _cssAribSourceSettings ::
      !(Maybe AribSourceSettings),
    _cssScte27SourceSettings ::
      !(Maybe Scte27SourceSettings),
    _cssDvbSubSourceSettings ::
      !(Maybe DvbSubSourceSettings),
    _cssAncillarySourceSettings ::
      !(Maybe AncillarySourceSettings),
    _cssScte20SourceSettings ::
      !(Maybe Scte20SourceSettings),
    _cssEmbeddedSourceSettings ::
      !(Maybe EmbeddedSourceSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionSelectorSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssTeletextSourceSettings' - Undocumented member.
--
-- * 'cssAribSourceSettings' - Undocumented member.
--
-- * 'cssScte27SourceSettings' - Undocumented member.
--
-- * 'cssDvbSubSourceSettings' - Undocumented member.
--
-- * 'cssAncillarySourceSettings' - Undocumented member.
--
-- * 'cssScte20SourceSettings' - Undocumented member.
--
-- * 'cssEmbeddedSourceSettings' - Undocumented member.
captionSelectorSettings ::
  CaptionSelectorSettings
captionSelectorSettings =
  CaptionSelectorSettings'
    { _cssTeletextSourceSettings = Nothing,
      _cssAribSourceSettings = Nothing,
      _cssScte27SourceSettings = Nothing,
      _cssDvbSubSourceSettings = Nothing,
      _cssAncillarySourceSettings = Nothing,
      _cssScte20SourceSettings = Nothing,
      _cssEmbeddedSourceSettings = Nothing
    }

-- | Undocumented member.
cssTeletextSourceSettings :: Lens' CaptionSelectorSettings (Maybe TeletextSourceSettings)
cssTeletextSourceSettings = lens _cssTeletextSourceSettings (\s a -> s {_cssTeletextSourceSettings = a})

-- | Undocumented member.
cssAribSourceSettings :: Lens' CaptionSelectorSettings (Maybe AribSourceSettings)
cssAribSourceSettings = lens _cssAribSourceSettings (\s a -> s {_cssAribSourceSettings = a})

-- | Undocumented member.
cssScte27SourceSettings :: Lens' CaptionSelectorSettings (Maybe Scte27SourceSettings)
cssScte27SourceSettings = lens _cssScte27SourceSettings (\s a -> s {_cssScte27SourceSettings = a})

-- | Undocumented member.
cssDvbSubSourceSettings :: Lens' CaptionSelectorSettings (Maybe DvbSubSourceSettings)
cssDvbSubSourceSettings = lens _cssDvbSubSourceSettings (\s a -> s {_cssDvbSubSourceSettings = a})

-- | Undocumented member.
cssAncillarySourceSettings :: Lens' CaptionSelectorSettings (Maybe AncillarySourceSettings)
cssAncillarySourceSettings = lens _cssAncillarySourceSettings (\s a -> s {_cssAncillarySourceSettings = a})

-- | Undocumented member.
cssScte20SourceSettings :: Lens' CaptionSelectorSettings (Maybe Scte20SourceSettings)
cssScte20SourceSettings = lens _cssScte20SourceSettings (\s a -> s {_cssScte20SourceSettings = a})

-- | Undocumented member.
cssEmbeddedSourceSettings :: Lens' CaptionSelectorSettings (Maybe EmbeddedSourceSettings)
cssEmbeddedSourceSettings = lens _cssEmbeddedSourceSettings (\s a -> s {_cssEmbeddedSourceSettings = a})

instance FromJSON CaptionSelectorSettings where
  parseJSON =
    withObject
      "CaptionSelectorSettings"
      ( \x ->
          CaptionSelectorSettings'
            <$> (x .:? "teletextSourceSettings")
            <*> (x .:? "aribSourceSettings")
            <*> (x .:? "scte27SourceSettings")
            <*> (x .:? "dvbSubSourceSettings")
            <*> (x .:? "ancillarySourceSettings")
            <*> (x .:? "scte20SourceSettings")
            <*> (x .:? "embeddedSourceSettings")
      )

instance Hashable CaptionSelectorSettings

instance NFData CaptionSelectorSettings

instance ToJSON CaptionSelectorSettings where
  toJSON CaptionSelectorSettings' {..} =
    object
      ( catMaybes
          [ ("teletextSourceSettings" .=) <$> _cssTeletextSourceSettings,
            ("aribSourceSettings" .=) <$> _cssAribSourceSettings,
            ("scte27SourceSettings" .=) <$> _cssScte27SourceSettings,
            ("dvbSubSourceSettings" .=) <$> _cssDvbSubSourceSettings,
            ("ancillarySourceSettings" .=) <$> _cssAncillarySourceSettings,
            ("scte20SourceSettings" .=) <$> _cssScte20SourceSettings,
            ("embeddedSourceSettings" .=) <$> _cssEmbeddedSourceSettings
          ]
      )
