{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AribDestinationSettings
import Network.AWS.MediaLive.Types.BurnInDestinationSettings
import Network.AWS.MediaLive.Types.DvbSubDestinationSettings
import Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.Scte27DestinationSettings
import Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
import Network.AWS.MediaLive.Types.TeletextDestinationSettings
import Network.AWS.MediaLive.Types.TtmlDestinationSettings
import Network.AWS.MediaLive.Types.WebvttDestinationSettings
import Network.AWS.Prelude

-- | Caption Destination Settings
--
-- /See:/ 'captionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { _cdsTeletextDestinationSettings ::
      !(Maybe TeletextDestinationSettings),
    _cdsEbuTtDDestinationSettings ::
      !(Maybe EbuTtDDestinationSettings),
    _cdsRtmpCaptionInfoDestinationSettings ::
      !( Maybe
           RtmpCaptionInfoDestinationSettings
       ),
    _cdsDvbSubDestinationSettings ::
      !(Maybe DvbSubDestinationSettings),
    _cdsScte27DestinationSettings ::
      !(Maybe Scte27DestinationSettings),
    _cdsTtmlDestinationSettings ::
      !(Maybe TtmlDestinationSettings),
    _cdsScte20PlusEmbeddedDestinationSettings ::
      !( Maybe
           Scte20PlusEmbeddedDestinationSettings
       ),
    _cdsEmbeddedPlusScte20DestinationSettings ::
      !( Maybe
           EmbeddedPlusScte20DestinationSettings
       ),
    _cdsSmpteTtDestinationSettings ::
      !(Maybe SmpteTtDestinationSettings),
    _cdsWebvttDestinationSettings ::
      !(Maybe WebvttDestinationSettings),
    _cdsEmbeddedDestinationSettings ::
      !(Maybe EmbeddedDestinationSettings),
    _cdsBurnInDestinationSettings ::
      !(Maybe BurnInDestinationSettings),
    _cdsAribDestinationSettings ::
      !(Maybe AribDestinationSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsTeletextDestinationSettings' - Undocumented member.
--
-- * 'cdsEbuTtDDestinationSettings' - Undocumented member.
--
-- * 'cdsRtmpCaptionInfoDestinationSettings' - Undocumented member.
--
-- * 'cdsDvbSubDestinationSettings' - Undocumented member.
--
-- * 'cdsScte27DestinationSettings' - Undocumented member.
--
-- * 'cdsTtmlDestinationSettings' - Undocumented member.
--
-- * 'cdsScte20PlusEmbeddedDestinationSettings' - Undocumented member.
--
-- * 'cdsEmbeddedPlusScte20DestinationSettings' - Undocumented member.
--
-- * 'cdsSmpteTtDestinationSettings' - Undocumented member.
--
-- * 'cdsWebvttDestinationSettings' - Undocumented member.
--
-- * 'cdsEmbeddedDestinationSettings' - Undocumented member.
--
-- * 'cdsBurnInDestinationSettings' - Undocumented member.
--
-- * 'cdsAribDestinationSettings' - Undocumented member.
captionDestinationSettings ::
  CaptionDestinationSettings
captionDestinationSettings =
  CaptionDestinationSettings'
    { _cdsTeletextDestinationSettings =
        Nothing,
      _cdsEbuTtDDestinationSettings = Nothing,
      _cdsRtmpCaptionInfoDestinationSettings = Nothing,
      _cdsDvbSubDestinationSettings = Nothing,
      _cdsScte27DestinationSettings = Nothing,
      _cdsTtmlDestinationSettings = Nothing,
      _cdsScte20PlusEmbeddedDestinationSettings = Nothing,
      _cdsEmbeddedPlusScte20DestinationSettings = Nothing,
      _cdsSmpteTtDestinationSettings = Nothing,
      _cdsWebvttDestinationSettings = Nothing,
      _cdsEmbeddedDestinationSettings = Nothing,
      _cdsBurnInDestinationSettings = Nothing,
      _cdsAribDestinationSettings = Nothing
    }

-- | Undocumented member.
cdsTeletextDestinationSettings :: Lens' CaptionDestinationSettings (Maybe TeletextDestinationSettings)
cdsTeletextDestinationSettings = lens _cdsTeletextDestinationSettings (\s a -> s {_cdsTeletextDestinationSettings = a})

-- | Undocumented member.
cdsEbuTtDDestinationSettings :: Lens' CaptionDestinationSettings (Maybe EbuTtDDestinationSettings)
cdsEbuTtDDestinationSettings = lens _cdsEbuTtDDestinationSettings (\s a -> s {_cdsEbuTtDDestinationSettings = a})

-- | Undocumented member.
cdsRtmpCaptionInfoDestinationSettings :: Lens' CaptionDestinationSettings (Maybe RtmpCaptionInfoDestinationSettings)
cdsRtmpCaptionInfoDestinationSettings = lens _cdsRtmpCaptionInfoDestinationSettings (\s a -> s {_cdsRtmpCaptionInfoDestinationSettings = a})

-- | Undocumented member.
cdsDvbSubDestinationSettings :: Lens' CaptionDestinationSettings (Maybe DvbSubDestinationSettings)
cdsDvbSubDestinationSettings = lens _cdsDvbSubDestinationSettings (\s a -> s {_cdsDvbSubDestinationSettings = a})

-- | Undocumented member.
cdsScte27DestinationSettings :: Lens' CaptionDestinationSettings (Maybe Scte27DestinationSettings)
cdsScte27DestinationSettings = lens _cdsScte27DestinationSettings (\s a -> s {_cdsScte27DestinationSettings = a})

-- | Undocumented member.
cdsTtmlDestinationSettings :: Lens' CaptionDestinationSettings (Maybe TtmlDestinationSettings)
cdsTtmlDestinationSettings = lens _cdsTtmlDestinationSettings (\s a -> s {_cdsTtmlDestinationSettings = a})

-- | Undocumented member.
cdsScte20PlusEmbeddedDestinationSettings :: Lens' CaptionDestinationSettings (Maybe Scte20PlusEmbeddedDestinationSettings)
cdsScte20PlusEmbeddedDestinationSettings = lens _cdsScte20PlusEmbeddedDestinationSettings (\s a -> s {_cdsScte20PlusEmbeddedDestinationSettings = a})

-- | Undocumented member.
cdsEmbeddedPlusScte20DestinationSettings :: Lens' CaptionDestinationSettings (Maybe EmbeddedPlusScte20DestinationSettings)
cdsEmbeddedPlusScte20DestinationSettings = lens _cdsEmbeddedPlusScte20DestinationSettings (\s a -> s {_cdsEmbeddedPlusScte20DestinationSettings = a})

-- | Undocumented member.
cdsSmpteTtDestinationSettings :: Lens' CaptionDestinationSettings (Maybe SmpteTtDestinationSettings)
cdsSmpteTtDestinationSettings = lens _cdsSmpteTtDestinationSettings (\s a -> s {_cdsSmpteTtDestinationSettings = a})

-- | Undocumented member.
cdsWebvttDestinationSettings :: Lens' CaptionDestinationSettings (Maybe WebvttDestinationSettings)
cdsWebvttDestinationSettings = lens _cdsWebvttDestinationSettings (\s a -> s {_cdsWebvttDestinationSettings = a})

-- | Undocumented member.
cdsEmbeddedDestinationSettings :: Lens' CaptionDestinationSettings (Maybe EmbeddedDestinationSettings)
cdsEmbeddedDestinationSettings = lens _cdsEmbeddedDestinationSettings (\s a -> s {_cdsEmbeddedDestinationSettings = a})

-- | Undocumented member.
cdsBurnInDestinationSettings :: Lens' CaptionDestinationSettings (Maybe BurnInDestinationSettings)
cdsBurnInDestinationSettings = lens _cdsBurnInDestinationSettings (\s a -> s {_cdsBurnInDestinationSettings = a})

-- | Undocumented member.
cdsAribDestinationSettings :: Lens' CaptionDestinationSettings (Maybe AribDestinationSettings)
cdsAribDestinationSettings = lens _cdsAribDestinationSettings (\s a -> s {_cdsAribDestinationSettings = a})

instance FromJSON CaptionDestinationSettings where
  parseJSON =
    withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            <$> (x .:? "teletextDestinationSettings")
            <*> (x .:? "ebuTtDDestinationSettings")
            <*> (x .:? "rtmpCaptionInfoDestinationSettings")
            <*> (x .:? "dvbSubDestinationSettings")
            <*> (x .:? "scte27DestinationSettings")
            <*> (x .:? "ttmlDestinationSettings")
            <*> (x .:? "scte20PlusEmbeddedDestinationSettings")
            <*> (x .:? "embeddedPlusScte20DestinationSettings")
            <*> (x .:? "smpteTtDestinationSettings")
            <*> (x .:? "webvttDestinationSettings")
            <*> (x .:? "embeddedDestinationSettings")
            <*> (x .:? "burnInDestinationSettings")
            <*> (x .:? "aribDestinationSettings")
      )

instance Hashable CaptionDestinationSettings

instance NFData CaptionDestinationSettings

instance ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    object
      ( catMaybes
          [ ("teletextDestinationSettings" .=)
              <$> _cdsTeletextDestinationSettings,
            ("ebuTtDDestinationSettings" .=) <$> _cdsEbuTtDDestinationSettings,
            ("rtmpCaptionInfoDestinationSettings" .=)
              <$> _cdsRtmpCaptionInfoDestinationSettings,
            ("dvbSubDestinationSettings" .=) <$> _cdsDvbSubDestinationSettings,
            ("scte27DestinationSettings" .=) <$> _cdsScte27DestinationSettings,
            ("ttmlDestinationSettings" .=) <$> _cdsTtmlDestinationSettings,
            ("scte20PlusEmbeddedDestinationSettings" .=)
              <$> _cdsScte20PlusEmbeddedDestinationSettings,
            ("embeddedPlusScte20DestinationSettings" .=)
              <$> _cdsEmbeddedPlusScte20DestinationSettings,
            ("smpteTtDestinationSettings" .=)
              <$> _cdsSmpteTtDestinationSettings,
            ("webvttDestinationSettings" .=) <$> _cdsWebvttDestinationSettings,
            ("embeddedDestinationSettings" .=)
              <$> _cdsEmbeddedDestinationSettings,
            ("burnInDestinationSettings" .=) <$> _cdsBurnInDestinationSettings,
            ("aribDestinationSettings" .=) <$> _cdsAribDestinationSettings
          ]
      )
