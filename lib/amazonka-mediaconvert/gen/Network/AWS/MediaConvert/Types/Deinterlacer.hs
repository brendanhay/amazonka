{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Deinterlacer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Deinterlacer where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
import Network.AWS.MediaConvert.Types.DeinterlacerControl
import Network.AWS.MediaConvert.Types.DeinterlacerMode
import Network.AWS.Prelude

-- | Settings for deinterlacer
--
-- /See:/ 'deinterlacer' smart constructor.
data Deinterlacer = Deinterlacer'
  { _dControl ::
      !(Maybe DeinterlacerControl),
    _dMode :: !(Maybe DeinterlacerMode),
    _dAlgorithm :: !(Maybe DeinterlaceAlgorithm)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Deinterlacer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dControl' - - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
--
-- * 'dMode' - Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
--
-- * 'dAlgorithm' - Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
deinterlacer ::
  Deinterlacer
deinterlacer =
  Deinterlacer'
    { _dControl = Nothing,
      _dMode = Nothing,
      _dAlgorithm = Nothing
    }

-- | - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
dControl :: Lens' Deinterlacer (Maybe DeinterlacerControl)
dControl = lens _dControl (\s a -> s {_dControl = a})

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
dMode :: Lens' Deinterlacer (Maybe DeinterlacerMode)
dMode = lens _dMode (\s a -> s {_dMode = a})

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
dAlgorithm :: Lens' Deinterlacer (Maybe DeinterlaceAlgorithm)
dAlgorithm = lens _dAlgorithm (\s a -> s {_dAlgorithm = a})

instance FromJSON Deinterlacer where
  parseJSON =
    withObject
      "Deinterlacer"
      ( \x ->
          Deinterlacer'
            <$> (x .:? "control") <*> (x .:? "mode") <*> (x .:? "algorithm")
      )

instance Hashable Deinterlacer

instance NFData Deinterlacer

instance ToJSON Deinterlacer where
  toJSON Deinterlacer' {..} =
    object
      ( catMaybes
          [ ("control" .=) <$> _dControl,
            ("mode" .=) <$> _dMode,
            ("algorithm" .=) <$> _dAlgorithm
          ]
      )
