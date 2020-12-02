{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSMessage where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.MessageType
import Network.AWS.Prelude

-- | Specifies the default settings for a one-time SMS message that's sent directly to an endpoint.
--
--
--
-- /See:/ 'sMSMessage' smart constructor.
data SMSMessage = SMSMessage'
  { _smsmSubstitutions ::
      !(Maybe (Map Text ([Text]))),
    _smsmOriginationNumber :: !(Maybe Text),
    _smsmBody :: !(Maybe Text),
    _smsmMessageType :: !(Maybe MessageType),
    _smsmSenderId :: !(Maybe Text),
    _smsmMediaURL :: !(Maybe Text),
    _smsmKeyword :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SMSMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smsmSubstitutions' - The message variables to use in the SMS message. You can override the default variables with individual address variables.
--
-- * 'smsmOriginationNumber' - The number to send the SMS message from. This value should be one of the dedicated long or short codes that's assigned to your AWS account. If you don't specify a long or short code, Amazon Pinpoint assigns a random long code to the SMS message and sends the message from that code.
--
-- * 'smsmBody' - The body of the SMS message.
--
-- * 'smsmMessageType' - The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
--
-- * 'smsmSenderId' - The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region.
--
-- * 'smsmMediaURL' - This field is reserved for future use.
--
-- * 'smsmKeyword' - The SMS program name that you provided to AWS Support when you requested your dedicated number.
sMSMessage ::
  SMSMessage
sMSMessage =
  SMSMessage'
    { _smsmSubstitutions = Nothing,
      _smsmOriginationNumber = Nothing,
      _smsmBody = Nothing,
      _smsmMessageType = Nothing,
      _smsmSenderId = Nothing,
      _smsmMediaURL = Nothing,
      _smsmKeyword = Nothing
    }

-- | The message variables to use in the SMS message. You can override the default variables with individual address variables.
smsmSubstitutions :: Lens' SMSMessage (HashMap Text ([Text]))
smsmSubstitutions = lens _smsmSubstitutions (\s a -> s {_smsmSubstitutions = a}) . _Default . _Map

-- | The number to send the SMS message from. This value should be one of the dedicated long or short codes that's assigned to your AWS account. If you don't specify a long or short code, Amazon Pinpoint assigns a random long code to the SMS message and sends the message from that code.
smsmOriginationNumber :: Lens' SMSMessage (Maybe Text)
smsmOriginationNumber = lens _smsmOriginationNumber (\s a -> s {_smsmOriginationNumber = a})

-- | The body of the SMS message.
smsmBody :: Lens' SMSMessage (Maybe Text)
smsmBody = lens _smsmBody (\s a -> s {_smsmBody = a})

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
smsmMessageType :: Lens' SMSMessage (Maybe MessageType)
smsmMessageType = lens _smsmMessageType (\s a -> s {_smsmMessageType = a})

-- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region.
smsmSenderId :: Lens' SMSMessage (Maybe Text)
smsmSenderId = lens _smsmSenderId (\s a -> s {_smsmSenderId = a})

-- | This field is reserved for future use.
smsmMediaURL :: Lens' SMSMessage (Maybe Text)
smsmMediaURL = lens _smsmMediaURL (\s a -> s {_smsmMediaURL = a})

-- | The SMS program name that you provided to AWS Support when you requested your dedicated number.
smsmKeyword :: Lens' SMSMessage (Maybe Text)
smsmKeyword = lens _smsmKeyword (\s a -> s {_smsmKeyword = a})

instance Hashable SMSMessage

instance NFData SMSMessage

instance ToJSON SMSMessage where
  toJSON SMSMessage' {..} =
    object
      ( catMaybes
          [ ("Substitutions" .=) <$> _smsmSubstitutions,
            ("OriginationNumber" .=) <$> _smsmOriginationNumber,
            ("Body" .=) <$> _smsmBody,
            ("MessageType" .=) <$> _smsmMessageType,
            ("SenderId" .=) <$> _smsmSenderId,
            ("MediaUrl" .=) <$> _smsmMediaURL,
            ("Keyword" .=) <$> _smsmKeyword
          ]
      )
