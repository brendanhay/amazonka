{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TtmlDestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
import Network.AWS.Prelude

-- | Ttml Destination Settings
--
-- /See:/ 'ttmlDestinationSettings' smart constructor.
newtype TtmlDestinationSettings = TtmlDestinationSettings'
  { _tdsStyleControl ::
      Maybe TtmlDestinationStyleControl
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TtmlDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdsStyleControl' - When set to passthrough, passes through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
ttmlDestinationSettings ::
  TtmlDestinationSettings
ttmlDestinationSettings =
  TtmlDestinationSettings' {_tdsStyleControl = Nothing}

-- | When set to passthrough, passes through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
tdsStyleControl :: Lens' TtmlDestinationSettings (Maybe TtmlDestinationStyleControl)
tdsStyleControl = lens _tdsStyleControl (\s a -> s {_tdsStyleControl = a})

instance FromJSON TtmlDestinationSettings where
  parseJSON =
    withObject
      "TtmlDestinationSettings"
      (\x -> TtmlDestinationSettings' <$> (x .:? "styleControl"))

instance Hashable TtmlDestinationSettings

instance NFData TtmlDestinationSettings

instance ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    object (catMaybes [("styleControl" .=) <$> _tdsStyleControl])
