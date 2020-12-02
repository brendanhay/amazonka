{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateInstantBooking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateInstantBooking where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Creates settings for the instant booking feature that are applied to a room profile. When users start their meeting with Alexa, Alexa automatically books the room for the configured duration if the room is available.
--
--
--
-- /See:/ 'createInstantBooking' smart constructor.
data CreateInstantBooking = CreateInstantBooking'
  { _cibDurationInMinutes ::
      !Int,
    _cibEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInstantBooking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cibDurationInMinutes' - Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
--
-- * 'cibEnabled' - Whether instant booking is enabled or not.
createInstantBooking ::
  -- | 'cibDurationInMinutes'
  Int ->
  -- | 'cibEnabled'
  Bool ->
  CreateInstantBooking
createInstantBooking pDurationInMinutes_ pEnabled_ =
  CreateInstantBooking'
    { _cibDurationInMinutes =
        pDurationInMinutes_,
      _cibEnabled = pEnabled_
    }

-- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
cibDurationInMinutes :: Lens' CreateInstantBooking Int
cibDurationInMinutes = lens _cibDurationInMinutes (\s a -> s {_cibDurationInMinutes = a})

-- | Whether instant booking is enabled or not.
cibEnabled :: Lens' CreateInstantBooking Bool
cibEnabled = lens _cibEnabled (\s a -> s {_cibEnabled = a})

instance Hashable CreateInstantBooking

instance NFData CreateInstantBooking

instance ToJSON CreateInstantBooking where
  toJSON CreateInstantBooking' {..} =
    object
      ( catMaybes
          [ Just ("DurationInMinutes" .= _cibDurationInMinutes),
            Just ("Enabled" .= _cibEnabled)
          ]
      )
