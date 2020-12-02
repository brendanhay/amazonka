{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TtmlDestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.TtmlStylePassthrough
import Network.AWS.Prelude

-- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
--
-- /See:/ 'ttmlDestinationSettings' smart constructor.
newtype TtmlDestinationSettings = TtmlDestinationSettings'
  { _tdsStylePassthrough ::
      Maybe TtmlStylePassthrough
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TtmlDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdsStylePassthrough' - Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT) to the TTML output.
ttmlDestinationSettings ::
  TtmlDestinationSettings
ttmlDestinationSettings =
  TtmlDestinationSettings' {_tdsStylePassthrough = Nothing}

-- | Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT) to the TTML output.
tdsStylePassthrough :: Lens' TtmlDestinationSettings (Maybe TtmlStylePassthrough)
tdsStylePassthrough = lens _tdsStylePassthrough (\s a -> s {_tdsStylePassthrough = a})

instance FromJSON TtmlDestinationSettings where
  parseJSON =
    withObject
      "TtmlDestinationSettings"
      (\x -> TtmlDestinationSettings' <$> (x .:? "stylePassthrough"))

instance Hashable TtmlDestinationSettings

instance NFData TtmlDestinationSettings

instance ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    object
      (catMaybes [("stylePassthrough" .=) <$> _tdsStylePassthrough])
