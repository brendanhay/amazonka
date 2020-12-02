{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
import Network.AWS.MediaLive.Types.TemporalFilterStrength
import Network.AWS.Prelude

-- | Temporal Filter Settings
--
-- /See:/ 'temporalFilterSettings' smart constructor.
data TemporalFilterSettings = TemporalFilterSettings'
  { _tfsStrength ::
      !(Maybe TemporalFilterStrength),
    _tfsPostFilterSharpening ::
      !(Maybe TemporalFilterPostFilterSharpening)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TemporalFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfsStrength' - Choose a filter strength. We recommend a strength of 1 or 2. A higher strength might take out good information, resulting in an image that is overly soft.
--
-- * 'tfsPostFilterSharpening' - If you enable this filter, the results are the following: - If the source content is noisy (it contains excessive digital artifacts), the filter cleans up the source. - If the source content is already clean, the filter tends to decrease the bitrate, especially when the rate control mode is QVBR.
temporalFilterSettings ::
  TemporalFilterSettings
temporalFilterSettings =
  TemporalFilterSettings'
    { _tfsStrength = Nothing,
      _tfsPostFilterSharpening = Nothing
    }

-- | Choose a filter strength. We recommend a strength of 1 or 2. A higher strength might take out good information, resulting in an image that is overly soft.
tfsStrength :: Lens' TemporalFilterSettings (Maybe TemporalFilterStrength)
tfsStrength = lens _tfsStrength (\s a -> s {_tfsStrength = a})

-- | If you enable this filter, the results are the following: - If the source content is noisy (it contains excessive digital artifacts), the filter cleans up the source. - If the source content is already clean, the filter tends to decrease the bitrate, especially when the rate control mode is QVBR.
tfsPostFilterSharpening :: Lens' TemporalFilterSettings (Maybe TemporalFilterPostFilterSharpening)
tfsPostFilterSharpening = lens _tfsPostFilterSharpening (\s a -> s {_tfsPostFilterSharpening = a})

instance FromJSON TemporalFilterSettings where
  parseJSON =
    withObject
      "TemporalFilterSettings"
      ( \x ->
          TemporalFilterSettings'
            <$> (x .:? "strength") <*> (x .:? "postFilterSharpening")
      )

instance Hashable TemporalFilterSettings

instance NFData TemporalFilterSettings

instance ToJSON TemporalFilterSettings where
  toJSON TemporalFilterSettings' {..} =
    object
      ( catMaybes
          [ ("strength" .=) <$> _tfsStrength,
            ("postFilterSharpening" .=) <$> _tfsPostFilterSharpening
          ]
      )
