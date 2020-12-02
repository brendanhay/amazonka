{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailBlanking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailBlanking where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AvailBlankingState
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.Prelude

-- | Avail Blanking
--
-- /See:/ 'availBlanking' smart constructor.
data AvailBlanking = AvailBlanking'
  { _abState ::
      !(Maybe AvailBlankingState),
    _abAvailBlankingImage :: !(Maybe InputLocation)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailBlanking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abState' - When set to enabled, causes video, audio and captions to be blanked when insertion metadata is added.
--
-- * 'abAvailBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
availBlanking ::
  AvailBlanking
availBlanking =
  AvailBlanking'
    { _abState = Nothing,
      _abAvailBlankingImage = Nothing
    }

-- | When set to enabled, causes video, audio and captions to be blanked when insertion metadata is added.
abState :: Lens' AvailBlanking (Maybe AvailBlankingState)
abState = lens _abState (\s a -> s {_abState = a})

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
abAvailBlankingImage :: Lens' AvailBlanking (Maybe InputLocation)
abAvailBlankingImage = lens _abAvailBlankingImage (\s a -> s {_abAvailBlankingImage = a})

instance FromJSON AvailBlanking where
  parseJSON =
    withObject
      "AvailBlanking"
      ( \x ->
          AvailBlanking'
            <$> (x .:? "state") <*> (x .:? "availBlankingImage")
      )

instance Hashable AvailBlanking

instance NFData AvailBlanking

instance ToJSON AvailBlanking where
  toJSON AvailBlanking' {..} =
    object
      ( catMaybes
          [ ("state" .=) <$> _abState,
            ("availBlankingImage" .=) <$> _abAvailBlankingImage
          ]
      )
