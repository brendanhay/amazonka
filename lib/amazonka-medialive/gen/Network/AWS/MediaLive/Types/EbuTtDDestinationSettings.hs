{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EbuTtDDestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.EbuTtDDestinationStyleControl
import Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl
import Network.AWS.Prelude

-- | Ebu Tt DDestination Settings
--
-- /See:/ 'ebuTtDDestinationSettings' smart constructor.
data EbuTtDDestinationSettings = EbuTtDDestinationSettings'
  { _etddsFillLineGap ::
      !(Maybe EbuTtDFillLineGapControl),
    _etddsFontFamily :: !(Maybe Text),
    _etddsStyleControl ::
      !(Maybe EbuTtDDestinationStyleControl)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EbuTtDDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etddsFillLineGap' - Specifies how to handle the gap between the lines (in multi-line captions). - enabled: Fill with the captions background color (as specified in the input captions). - disabled: Leave the gap unfilled.
--
-- * 'etddsFontFamily' - Specifies the font family to include in the font data attached to the EBU-TT captions. Valid only if styleControl is set to include. If you leave this field empty, the font family is set to "monospaced". (If styleControl is set to exclude, the font family is always set to "monospaced".) You specify only the font family. All other style information (color, bold, position and so on) is copied from the input captions. The size is always set to 100% to allow the downstream player to choose the size. - Enter a list of font families, as a comma-separated list of font names, in order of preference. The name can be a font family (such as “Arial”), or a generic font family (such as “serif”), or “default” (to let the downstream player choose the font). - Leave blank to set the family to “monospace”.
--
-- * 'etddsStyleControl' - Specifies the style information (font color, font position, and so on) to include in the font data that is attached to the EBU-TT captions. - include: Take the style information (font color, font position, and so on) from the source captions and include that information in the font data attached to the EBU-TT captions. This option is valid only if the source captions are Embedded or Teletext. - exclude: In the font data attached to the EBU-TT captions, set the font family to "monospaced". Do not include any other style information.
ebuTtDDestinationSettings ::
  EbuTtDDestinationSettings
ebuTtDDestinationSettings =
  EbuTtDDestinationSettings'
    { _etddsFillLineGap = Nothing,
      _etddsFontFamily = Nothing,
      _etddsStyleControl = Nothing
    }

-- | Specifies how to handle the gap between the lines (in multi-line captions). - enabled: Fill with the captions background color (as specified in the input captions). - disabled: Leave the gap unfilled.
etddsFillLineGap :: Lens' EbuTtDDestinationSettings (Maybe EbuTtDFillLineGapControl)
etddsFillLineGap = lens _etddsFillLineGap (\s a -> s {_etddsFillLineGap = a})

-- | Specifies the font family to include in the font data attached to the EBU-TT captions. Valid only if styleControl is set to include. If you leave this field empty, the font family is set to "monospaced". (If styleControl is set to exclude, the font family is always set to "monospaced".) You specify only the font family. All other style information (color, bold, position and so on) is copied from the input captions. The size is always set to 100% to allow the downstream player to choose the size. - Enter a list of font families, as a comma-separated list of font names, in order of preference. The name can be a font family (such as “Arial”), or a generic font family (such as “serif”), or “default” (to let the downstream player choose the font). - Leave blank to set the family to “monospace”.
etddsFontFamily :: Lens' EbuTtDDestinationSettings (Maybe Text)
etddsFontFamily = lens _etddsFontFamily (\s a -> s {_etddsFontFamily = a})

-- | Specifies the style information (font color, font position, and so on) to include in the font data that is attached to the EBU-TT captions. - include: Take the style information (font color, font position, and so on) from the source captions and include that information in the font data attached to the EBU-TT captions. This option is valid only if the source captions are Embedded or Teletext. - exclude: In the font data attached to the EBU-TT captions, set the font family to "monospaced". Do not include any other style information.
etddsStyleControl :: Lens' EbuTtDDestinationSettings (Maybe EbuTtDDestinationStyleControl)
etddsStyleControl = lens _etddsStyleControl (\s a -> s {_etddsStyleControl = a})

instance FromJSON EbuTtDDestinationSettings where
  parseJSON =
    withObject
      "EbuTtDDestinationSettings"
      ( \x ->
          EbuTtDDestinationSettings'
            <$> (x .:? "fillLineGap")
            <*> (x .:? "fontFamily")
            <*> (x .:? "styleControl")
      )

instance Hashable EbuTtDDestinationSettings

instance NFData EbuTtDDestinationSettings

instance ToJSON EbuTtDDestinationSettings where
  toJSON EbuTtDDestinationSettings' {..} =
    object
      ( catMaybes
          [ ("fillLineGap" .=) <$> _etddsFillLineGap,
            ("fontFamily" .=) <$> _etddsFontFamily,
            ("styleControl" .=) <$> _etddsStyleControl
          ]
      )
