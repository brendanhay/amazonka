{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyEmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyEmailMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the "From" address for an email message that's sent to participants in a journey.
--
--
--
-- /See:/ 'journeyEmailMessage' smart constructor.
newtype JourneyEmailMessage = JourneyEmailMessage'
  { _jemFromAddress ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyEmailMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jemFromAddress' - The verified email address to send the email message from. The default address is the FromAddress specified for the email channel for the application.
journeyEmailMessage ::
  JourneyEmailMessage
journeyEmailMessage =
  JourneyEmailMessage' {_jemFromAddress = Nothing}

-- | The verified email address to send the email message from. The default address is the FromAddress specified for the email channel for the application.
jemFromAddress :: Lens' JourneyEmailMessage (Maybe Text)
jemFromAddress = lens _jemFromAddress (\s a -> s {_jemFromAddress = a})

instance FromJSON JourneyEmailMessage where
  parseJSON =
    withObject
      "JourneyEmailMessage"
      (\x -> JourneyEmailMessage' <$> (x .:? "FromAddress"))

instance Hashable JourneyEmailMessage

instance NFData JourneyEmailMessage

instance ToJSON JourneyEmailMessage where
  toJSON JourneyEmailMessage' {..} =
    object (catMaybes [("FromAddress" .=) <$> _jemFromAddress])
