{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImscDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ImscDestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.ImscStylePassthrough
import Network.AWS.Prelude

-- | Settings specific to IMSC caption outputs.
--
-- /See:/ 'imscDestinationSettings' smart constructor.
newtype ImscDestinationSettings = ImscDestinationSettings'
  { _idsStylePassthrough ::
      Maybe ImscStylePassthrough
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImscDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsStylePassthrough' - Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
imscDestinationSettings ::
  ImscDestinationSettings
imscDestinationSettings =
  ImscDestinationSettings' {_idsStylePassthrough = Nothing}

-- | Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
idsStylePassthrough :: Lens' ImscDestinationSettings (Maybe ImscStylePassthrough)
idsStylePassthrough = lens _idsStylePassthrough (\s a -> s {_idsStylePassthrough = a})

instance FromJSON ImscDestinationSettings where
  parseJSON =
    withObject
      "ImscDestinationSettings"
      (\x -> ImscDestinationSettings' <$> (x .:? "stylePassthrough"))

instance Hashable ImscDestinationSettings

instance NFData ImscDestinationSettings

instance ToJSON ImscDestinationSettings where
  toJSON ImscDestinationSettings' {..} =
    object
      (catMaybes [("stylePassthrough" .=) <$> _idsStylePassthrough])
