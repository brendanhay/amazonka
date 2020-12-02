{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SimpleEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleEmail where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.SimpleEmailPart
import Network.AWS.Prelude

-- | Specifies the contents of an email message, composed of a subject, a text part, and an HTML part.
--
--
--
-- /See:/ 'simpleEmail' smart constructor.
data SimpleEmail = SimpleEmail'
  { _seSubject ::
      !(Maybe SimpleEmailPart),
    _seTextPart :: !(Maybe SimpleEmailPart),
    _seHTMLPart :: !(Maybe SimpleEmailPart)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SimpleEmail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seSubject' - The subject line, or title, of the email.
--
-- * 'seTextPart' - The body of the email message, in plain text format. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
--
-- * 'seHTMLPart' - The body of the email message, in HTML format. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
simpleEmail ::
  SimpleEmail
simpleEmail =
  SimpleEmail'
    { _seSubject = Nothing,
      _seTextPart = Nothing,
      _seHTMLPart = Nothing
    }

-- | The subject line, or title, of the email.
seSubject :: Lens' SimpleEmail (Maybe SimpleEmailPart)
seSubject = lens _seSubject (\s a -> s {_seSubject = a})

-- | The body of the email message, in plain text format. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
seTextPart :: Lens' SimpleEmail (Maybe SimpleEmailPart)
seTextPart = lens _seTextPart (\s a -> s {_seTextPart = a})

-- | The body of the email message, in HTML format. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
seHTMLPart :: Lens' SimpleEmail (Maybe SimpleEmailPart)
seHTMLPart = lens _seHTMLPart (\s a -> s {_seHTMLPart = a})

instance Hashable SimpleEmail

instance NFData SimpleEmail

instance ToJSON SimpleEmail where
  toJSON SimpleEmail' {..} =
    object
      ( catMaybes
          [ ("Subject" .=) <$> _seSubject,
            ("TextPart" .=) <$> _seTextPart,
            ("HtmlPart" .=) <$> _seHTMLPart
          ]
      )
