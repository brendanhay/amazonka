{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignEmailMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignEmailMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the content and \"From\" address for an email message that\'s
-- sent to recipients of a campaign.
--
-- /See:/ 'newCampaignEmailMessage' smart constructor.
data CampaignEmailMessage = CampaignEmailMessage'
  { -- | The subject line, or title, of the email.
    title :: Prelude.Maybe Prelude.Text,
    -- | The body of the email for recipients whose email clients don\'t render
    -- HTML content.
    body :: Prelude.Maybe Prelude.Text,
    -- | The body of the email, in HTML format, for recipients whose email
    -- clients render HTML content.
    htmlBody :: Prelude.Maybe Prelude.Text,
    -- | The verified email address to send the email from. The default address
    -- is the FromAddress specified for the email channel for the application.
    fromAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CampaignEmailMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'title', 'campaignEmailMessage_title' - The subject line, or title, of the email.
--
-- 'body', 'campaignEmailMessage_body' - The body of the email for recipients whose email clients don\'t render
-- HTML content.
--
-- 'htmlBody', 'campaignEmailMessage_htmlBody' - The body of the email, in HTML format, for recipients whose email
-- clients render HTML content.
--
-- 'fromAddress', 'campaignEmailMessage_fromAddress' - The verified email address to send the email from. The default address
-- is the FromAddress specified for the email channel for the application.
newCampaignEmailMessage ::
  CampaignEmailMessage
newCampaignEmailMessage =
  CampaignEmailMessage'
    { title = Prelude.Nothing,
      body = Prelude.Nothing,
      htmlBody = Prelude.Nothing,
      fromAddress = Prelude.Nothing
    }

-- | The subject line, or title, of the email.
campaignEmailMessage_title :: Lens.Lens' CampaignEmailMessage (Prelude.Maybe Prelude.Text)
campaignEmailMessage_title = Lens.lens (\CampaignEmailMessage' {title} -> title) (\s@CampaignEmailMessage' {} a -> s {title = a} :: CampaignEmailMessage)

-- | The body of the email for recipients whose email clients don\'t render
-- HTML content.
campaignEmailMessage_body :: Lens.Lens' CampaignEmailMessage (Prelude.Maybe Prelude.Text)
campaignEmailMessage_body = Lens.lens (\CampaignEmailMessage' {body} -> body) (\s@CampaignEmailMessage' {} a -> s {body = a} :: CampaignEmailMessage)

-- | The body of the email, in HTML format, for recipients whose email
-- clients render HTML content.
campaignEmailMessage_htmlBody :: Lens.Lens' CampaignEmailMessage (Prelude.Maybe Prelude.Text)
campaignEmailMessage_htmlBody = Lens.lens (\CampaignEmailMessage' {htmlBody} -> htmlBody) (\s@CampaignEmailMessage' {} a -> s {htmlBody = a} :: CampaignEmailMessage)

-- | The verified email address to send the email from. The default address
-- is the FromAddress specified for the email channel for the application.
campaignEmailMessage_fromAddress :: Lens.Lens' CampaignEmailMessage (Prelude.Maybe Prelude.Text)
campaignEmailMessage_fromAddress = Lens.lens (\CampaignEmailMessage' {fromAddress} -> fromAddress) (\s@CampaignEmailMessage' {} a -> s {fromAddress = a} :: CampaignEmailMessage)

instance Prelude.FromJSON CampaignEmailMessage where
  parseJSON =
    Prelude.withObject
      "CampaignEmailMessage"
      ( \x ->
          CampaignEmailMessage'
            Prelude.<$> (x Prelude..:? "Title")
            Prelude.<*> (x Prelude..:? "Body")
            Prelude.<*> (x Prelude..:? "HtmlBody")
            Prelude.<*> (x Prelude..:? "FromAddress")
      )

instance Prelude.Hashable CampaignEmailMessage

instance Prelude.NFData CampaignEmailMessage

instance Prelude.ToJSON CampaignEmailMessage where
  toJSON CampaignEmailMessage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Title" Prelude..=) Prelude.<$> title,
            ("Body" Prelude..=) Prelude.<$> body,
            ("HtmlBody" Prelude..=) Prelude.<$> htmlBody,
            ("FromAddress" Prelude..=) Prelude.<$> fromAddress
          ]
      )
