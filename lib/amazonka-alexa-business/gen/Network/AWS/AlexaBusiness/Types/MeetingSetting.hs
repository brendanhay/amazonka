{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.MeetingSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.MeetingSetting where

import Network.AWS.AlexaBusiness.Types.RequirePin
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The values that indicate whether a pin is always required (YES), never required (NO), or OPTIONAL.
--
--
--     * If YES, Alexa will always ask for a meeting pin.
--
--     * If NO, Alexa will never ask for a meeting pin.
--
--     * If OPTIONAL, Alexa will ask if you have a meeting pin and if the customer responds with yes, it will ask for the meeting pin.
--
--
--
--
-- /See:/ 'meetingSetting' smart constructor.
newtype MeetingSetting = MeetingSetting'
  { _msRequirePin ::
      RequirePin
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MeetingSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msRequirePin' - The values that indicate whether the pin is always required.
meetingSetting ::
  -- | 'msRequirePin'
  RequirePin ->
  MeetingSetting
meetingSetting pRequirePin_ =
  MeetingSetting' {_msRequirePin = pRequirePin_}

-- | The values that indicate whether the pin is always required.
msRequirePin :: Lens' MeetingSetting RequirePin
msRequirePin = lens _msRequirePin (\s a -> s {_msRequirePin = a})

instance FromJSON MeetingSetting where
  parseJSON =
    withObject
      "MeetingSetting"
      (\x -> MeetingSetting' <$> (x .: "RequirePin"))

instance Hashable MeetingSetting

instance NFData MeetingSetting

instance ToJSON MeetingSetting where
  toJSON MeetingSetting' {..} =
    object (catMaybes [Just ("RequirePin" .= _msRequirePin)])
