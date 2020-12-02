{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignEmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignEmailMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the content and "From" address for an email message that's sent to recipients of a campaign.
--
--
--
-- /See:/ 'campaignEmailMessage' smart constructor.
data CampaignEmailMessage = CampaignEmailMessage'
  { _cemBody ::
      !(Maybe Text),
    _cemFromAddress :: !(Maybe Text),
    _cemHTMLBody :: !(Maybe Text),
    _cemTitle :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignEmailMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cemBody' - The body of the email for recipients whose email clients don't render HTML content.
--
-- * 'cemFromAddress' - The verified email address to send the email from. The default address is the FromAddress specified for the email channel for the application.
--
-- * 'cemHTMLBody' - The body of the email, in HTML format, for recipients whose email clients render HTML content.
--
-- * 'cemTitle' - The subject line, or title, of the email.
campaignEmailMessage ::
  CampaignEmailMessage
campaignEmailMessage =
  CampaignEmailMessage'
    { _cemBody = Nothing,
      _cemFromAddress = Nothing,
      _cemHTMLBody = Nothing,
      _cemTitle = Nothing
    }

-- | The body of the email for recipients whose email clients don't render HTML content.
cemBody :: Lens' CampaignEmailMessage (Maybe Text)
cemBody = lens _cemBody (\s a -> s {_cemBody = a})

-- | The verified email address to send the email from. The default address is the FromAddress specified for the email channel for the application.
cemFromAddress :: Lens' CampaignEmailMessage (Maybe Text)
cemFromAddress = lens _cemFromAddress (\s a -> s {_cemFromAddress = a})

-- | The body of the email, in HTML format, for recipients whose email clients render HTML content.
cemHTMLBody :: Lens' CampaignEmailMessage (Maybe Text)
cemHTMLBody = lens _cemHTMLBody (\s a -> s {_cemHTMLBody = a})

-- | The subject line, or title, of the email.
cemTitle :: Lens' CampaignEmailMessage (Maybe Text)
cemTitle = lens _cemTitle (\s a -> s {_cemTitle = a})

instance FromJSON CampaignEmailMessage where
  parseJSON =
    withObject
      "CampaignEmailMessage"
      ( \x ->
          CampaignEmailMessage'
            <$> (x .:? "Body")
            <*> (x .:? "FromAddress")
            <*> (x .:? "HtmlBody")
            <*> (x .:? "Title")
      )

instance Hashable CampaignEmailMessage

instance NFData CampaignEmailMessage

instance ToJSON CampaignEmailMessage where
  toJSON CampaignEmailMessage' {..} =
    object
      ( catMaybes
          [ ("Body" .=) <$> _cemBody,
            ("FromAddress" .=) <$> _cemFromAddress,
            ("HtmlBody" .=) <$> _cemHTMLBody,
            ("Title" .=) <$> _cemTitle
          ]
      )
