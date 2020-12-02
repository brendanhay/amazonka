{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.HlsSettings
import Network.AWS.Prelude

-- | Specific settings for this type of output.
--
-- /See:/ 'outputSettings' smart constructor.
newtype OutputSettings = OutputSettings'
  { _osHlsSettings ::
      Maybe HlsSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osHlsSettings' - Settings for HLS output groups
outputSettings ::
  OutputSettings
outputSettings = OutputSettings' {_osHlsSettings = Nothing}

-- | Settings for HLS output groups
osHlsSettings :: Lens' OutputSettings (Maybe HlsSettings)
osHlsSettings = lens _osHlsSettings (\s a -> s {_osHlsSettings = a})

instance FromJSON OutputSettings where
  parseJSON =
    withObject
      "OutputSettings"
      (\x -> OutputSettings' <$> (x .:? "hlsSettings"))

instance Hashable OutputSettings

instance NFData OutputSettings

instance ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    object (catMaybes [("hlsSettings" .=) <$> _osHlsSettings])
