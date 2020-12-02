{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailMessage where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.RawEmail
import Network.AWS.Pinpoint.Types.SimpleEmail
import Network.AWS.Prelude

-- | Specifies the default settings and content for a one-time email message that's sent directly to an endpoint.
--
--
--
-- /See:/ 'emailMessage' smart constructor.
data EmailMessage = EmailMessage'
  { _emSubstitutions ::
      !(Maybe (Map Text ([Text]))),
    _emBody :: !(Maybe Text),
    _emFromAddress :: !(Maybe Text),
    _emRawEmail :: !(Maybe RawEmail),
    _emFeedbackForwardingAddress :: !(Maybe Text),
    _emSimpleEmail :: !(Maybe SimpleEmail),
    _emReplyToAddresses :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmailMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emSubstitutions' - The default message variables to use in the email message. You can override the default variables with individual address variables.
--
-- * 'emBody' - The body of the email message.
--
-- * 'emFromAddress' - The verified email address to send the email message from. The default value is the FromAddress specified for the email channel.
--
-- * 'emRawEmail' - The email message, represented as a raw MIME message.
--
-- * 'emFeedbackForwardingAddress' - The email address to forward bounces and complaints to, if feedback forwarding is enabled.
--
-- * 'emSimpleEmail' - The email message, composed of a subject, a text part, and an HTML part.
--
-- * 'emReplyToAddresses' - The reply-to email address(es) for the email message. If a recipient replies to the email, each reply-to address receives the reply.
emailMessage ::
  EmailMessage
emailMessage =
  EmailMessage'
    { _emSubstitutions = Nothing,
      _emBody = Nothing,
      _emFromAddress = Nothing,
      _emRawEmail = Nothing,
      _emFeedbackForwardingAddress = Nothing,
      _emSimpleEmail = Nothing,
      _emReplyToAddresses = Nothing
    }

-- | The default message variables to use in the email message. You can override the default variables with individual address variables.
emSubstitutions :: Lens' EmailMessage (HashMap Text ([Text]))
emSubstitutions = lens _emSubstitutions (\s a -> s {_emSubstitutions = a}) . _Default . _Map

-- | The body of the email message.
emBody :: Lens' EmailMessage (Maybe Text)
emBody = lens _emBody (\s a -> s {_emBody = a})

-- | The verified email address to send the email message from. The default value is the FromAddress specified for the email channel.
emFromAddress :: Lens' EmailMessage (Maybe Text)
emFromAddress = lens _emFromAddress (\s a -> s {_emFromAddress = a})

-- | The email message, represented as a raw MIME message.
emRawEmail :: Lens' EmailMessage (Maybe RawEmail)
emRawEmail = lens _emRawEmail (\s a -> s {_emRawEmail = a})

-- | The email address to forward bounces and complaints to, if feedback forwarding is enabled.
emFeedbackForwardingAddress :: Lens' EmailMessage (Maybe Text)
emFeedbackForwardingAddress = lens _emFeedbackForwardingAddress (\s a -> s {_emFeedbackForwardingAddress = a})

-- | The email message, composed of a subject, a text part, and an HTML part.
emSimpleEmail :: Lens' EmailMessage (Maybe SimpleEmail)
emSimpleEmail = lens _emSimpleEmail (\s a -> s {_emSimpleEmail = a})

-- | The reply-to email address(es) for the email message. If a recipient replies to the email, each reply-to address receives the reply.
emReplyToAddresses :: Lens' EmailMessage [Text]
emReplyToAddresses = lens _emReplyToAddresses (\s a -> s {_emReplyToAddresses = a}) . _Default . _Coerce

instance Hashable EmailMessage

instance NFData EmailMessage

instance ToJSON EmailMessage where
  toJSON EmailMessage' {..} =
    object
      ( catMaybes
          [ ("Substitutions" .=) <$> _emSubstitutions,
            ("Body" .=) <$> _emBody,
            ("FromAddress" .=) <$> _emFromAddress,
            ("RawEmail" .=) <$> _emRawEmail,
            ("FeedbackForwardingAddress" .=) <$> _emFeedbackForwardingAddress,
            ("SimpleEmail" .=) <$> _emSimpleEmail,
            ("ReplyToAddresses" .=) <$> _emReplyToAddresses
          ]
      )
