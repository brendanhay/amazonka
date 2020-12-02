{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.MovClapAtom
import Network.AWS.MediaConvert.Types.MovCslgAtom
import Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
import Network.AWS.MediaConvert.Types.MovPaddingControl
import Network.AWS.MediaConvert.Types.MovReference
import Network.AWS.Prelude

-- | Settings for MOV Container.
--
-- /See:/ 'movSettings' smart constructor.
data MovSettings = MovSettings'
  { _msReference ::
      !(Maybe MovReference),
    _msCslgAtom :: !(Maybe MovCslgAtom),
    _msMpeg2FourCCControl :: !(Maybe MovMpeg2FourCCControl),
    _msPaddingControl :: !(Maybe MovPaddingControl),
    _msClapAtom :: !(Maybe MovClapAtom)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MovSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msReference' - Always keep the default value (SELF_CONTAINED) for this setting.
--
-- * 'msCslgAtom' - When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
--
-- * 'msMpeg2FourCCControl' - When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
--
-- * 'msPaddingControl' - To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
--
-- * 'msClapAtom' - When enabled, include 'clap' atom if appropriate for the video output settings.
movSettings ::
  MovSettings
movSettings =
  MovSettings'
    { _msReference = Nothing,
      _msCslgAtom = Nothing,
      _msMpeg2FourCCControl = Nothing,
      _msPaddingControl = Nothing,
      _msClapAtom = Nothing
    }

-- | Always keep the default value (SELF_CONTAINED) for this setting.
msReference :: Lens' MovSettings (Maybe MovReference)
msReference = lens _msReference (\s a -> s {_msReference = a})

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
msCslgAtom :: Lens' MovSettings (Maybe MovCslgAtom)
msCslgAtom = lens _msCslgAtom (\s a -> s {_msCslgAtom = a})

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
msMpeg2FourCCControl :: Lens' MovSettings (Maybe MovMpeg2FourCCControl)
msMpeg2FourCCControl = lens _msMpeg2FourCCControl (\s a -> s {_msMpeg2FourCCControl = a})

-- | To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
msPaddingControl :: Lens' MovSettings (Maybe MovPaddingControl)
msPaddingControl = lens _msPaddingControl (\s a -> s {_msPaddingControl = a})

-- | When enabled, include 'clap' atom if appropriate for the video output settings.
msClapAtom :: Lens' MovSettings (Maybe MovClapAtom)
msClapAtom = lens _msClapAtom (\s a -> s {_msClapAtom = a})

instance FromJSON MovSettings where
  parseJSON =
    withObject
      "MovSettings"
      ( \x ->
          MovSettings'
            <$> (x .:? "reference")
            <*> (x .:? "cslgAtom")
            <*> (x .:? "mpeg2FourCCControl")
            <*> (x .:? "paddingControl")
            <*> (x .:? "clapAtom")
      )

instance Hashable MovSettings

instance NFData MovSettings

instance ToJSON MovSettings where
  toJSON MovSettings' {..} =
    object
      ( catMaybes
          [ ("reference" .=) <$> _msReference,
            ("cslgAtom" .=) <$> _msCslgAtom,
            ("mpeg2FourCCControl" .=) <$> _msMpeg2FourCCControl,
            ("paddingControl" .=) <$> _msPaddingControl,
            ("clapAtom" .=) <$> _msClapAtom
          ]
      )
