{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyCustomMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyCustomMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the message content for a custom channel message that's sent to participants in a journey.
--
--
--
-- /See:/ 'journeyCustomMessage' smart constructor.
newtype JourneyCustomMessage = JourneyCustomMessage'
  { _jcmData ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyCustomMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jcmData' - The message content that's passed to an AWS Lambda function or to a web hook.
journeyCustomMessage ::
  JourneyCustomMessage
journeyCustomMessage = JourneyCustomMessage' {_jcmData = Nothing}

-- | The message content that's passed to an AWS Lambda function or to a web hook.
jcmData :: Lens' JourneyCustomMessage (Maybe Text)
jcmData = lens _jcmData (\s a -> s {_jcmData = a})

instance FromJSON JourneyCustomMessage where
  parseJSON =
    withObject
      "JourneyCustomMessage"
      (\x -> JourneyCustomMessage' <$> (x .:? "Data"))

instance Hashable JourneyCustomMessage

instance NFData JourneyCustomMessage

instance ToJSON JourneyCustomMessage where
  toJSON JourneyCustomMessage' {..} =
    object (catMaybes [("Data" .=) <$> _jcmData])
