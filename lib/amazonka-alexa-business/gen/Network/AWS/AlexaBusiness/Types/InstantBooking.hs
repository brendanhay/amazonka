{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.InstantBooking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.InstantBooking where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for the instant booking feature that are applied to a room profile. When users start their meeting with Alexa, Alexa automatically books the room for the configured duration if the room is available.
--
--
--
-- /See:/ 'instantBooking' smart constructor.
data InstantBooking = InstantBooking'
  { _ibEnabled :: !(Maybe Bool),
    _ibDurationInMinutes :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstantBooking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibEnabled' - Whether instant booking is enabled or not.
--
-- * 'ibDurationInMinutes' - Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
instantBooking ::
  InstantBooking
instantBooking =
  InstantBooking'
    { _ibEnabled = Nothing,
      _ibDurationInMinutes = Nothing
    }

-- | Whether instant booking is enabled or not.
ibEnabled :: Lens' InstantBooking (Maybe Bool)
ibEnabled = lens _ibEnabled (\s a -> s {_ibEnabled = a})

-- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
ibDurationInMinutes :: Lens' InstantBooking (Maybe Int)
ibDurationInMinutes = lens _ibDurationInMinutes (\s a -> s {_ibDurationInMinutes = a})

instance FromJSON InstantBooking where
  parseJSON =
    withObject
      "InstantBooking"
      ( \x ->
          InstantBooking'
            <$> (x .:? "Enabled") <*> (x .:? "DurationInMinutes")
      )

instance Hashable InstantBooking

instance NFData InstantBooking
