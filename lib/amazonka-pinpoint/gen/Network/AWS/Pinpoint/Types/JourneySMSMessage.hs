{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneySMSMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneySMSMessage where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.MessageType
import Network.AWS.Prelude

-- | Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
--
--
--
-- /See:/ 'journeySMSMessage' smart constructor.
data JourneySMSMessage = JourneySMSMessage'
  { _jsmsmMessageType ::
      !(Maybe MessageType),
    _jsmsmSenderId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneySMSMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsmsmMessageType' - The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
--
-- * 'jsmsmSenderId' - The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region. For more information, see <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions> in the Amazon Pinpoint User Guide.
journeySMSMessage ::
  JourneySMSMessage
journeySMSMessage =
  JourneySMSMessage'
    { _jsmsmMessageType = Nothing,
      _jsmsmSenderId = Nothing
    }

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
jsmsmMessageType :: Lens' JourneySMSMessage (Maybe MessageType)
jsmsmMessageType = lens _jsmsmMessageType (\s a -> s {_jsmsmMessageType = a})

-- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region. For more information, see <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions> in the Amazon Pinpoint User Guide.
jsmsmSenderId :: Lens' JourneySMSMessage (Maybe Text)
jsmsmSenderId = lens _jsmsmSenderId (\s a -> s {_jsmsmSenderId = a})

instance FromJSON JourneySMSMessage where
  parseJSON =
    withObject
      "JourneySMSMessage"
      ( \x ->
          JourneySMSMessage'
            <$> (x .:? "MessageType") <*> (x .:? "SenderId")
      )

instance Hashable JourneySMSMessage

instance NFData JourneySMSMessage

instance ToJSON JourneySMSMessage where
  toJSON JourneySMSMessage' {..} =
    object
      ( catMaybes
          [ ("MessageType" .=) <$> _jsmsmMessageType,
            ("SenderId" .=) <$> _jsmsmSenderId
          ]
      )
