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
-- Module      : Amazonka.Pinpoint.Types.CampaignEmailMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignEmailMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the content and \"From\" address for an email message that\'s
-- sent to recipients of a campaign.
--
-- /See:/ 'newCampaignEmailMessage' smart constructor.
data CampaignEmailMessage = CampaignEmailMessage'
  { -- | The verified email address to send the email from. The default address
    -- is the FromAddress specified for the email channel for the application.
    fromAddress :: Prelude.Maybe Prelude.Text,
    -- | The body of the email for recipients whose email clients don\'t render
    -- HTML content.
    body :: Prelude.Maybe Prelude.Text,
    -- | The subject line, or title, of the email.
    title :: Prelude.Maybe Prelude.Text,
    -- | The body of the email, in HTML format, for recipients whose email
    -- clients render HTML content.
    htmlBody :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignEmailMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromAddress', 'campaignEmailMessage_fromAddress' - The verified email address to send the email from. The default address
-- is the FromAddress specified for the email channel for the application.
--
-- 'body', 'campaignEmailMessage_body' - The body of the email for recipients whose email clients don\'t render
-- HTML content.
--
-- 'title', 'campaignEmailMessage_title' - The subject line, or title, of the email.
--
-- 'htmlBody', 'campaignEmailMessage_htmlBody' - The body of the email, in HTML format, for recipients whose email
-- clients render HTML content.
newCampaignEmailMessage ::
  CampaignEmailMessage
newCampaignEmailMessage =
  CampaignEmailMessage'
    { fromAddress =
        Prelude.Nothing,
      body = Prelude.Nothing,
      title = Prelude.Nothing,
      htmlBody = Prelude.Nothing
    }

-- | The verified email address to send the email from. The default address
-- is the FromAddress specified for the email channel for the application.
campaignEmailMessage_fromAddress :: Lens.Lens' CampaignEmailMessage (Prelude.Maybe Prelude.Text)
campaignEmailMessage_fromAddress = Lens.lens (\CampaignEmailMessage' {fromAddress} -> fromAddress) (\s@CampaignEmailMessage' {} a -> s {fromAddress = a} :: CampaignEmailMessage)

-- | The body of the email for recipients whose email clients don\'t render
-- HTML content.
campaignEmailMessage_body :: Lens.Lens' CampaignEmailMessage (Prelude.Maybe Prelude.Text)
campaignEmailMessage_body = Lens.lens (\CampaignEmailMessage' {body} -> body) (\s@CampaignEmailMessage' {} a -> s {body = a} :: CampaignEmailMessage)

-- | The subject line, or title, of the email.
campaignEmailMessage_title :: Lens.Lens' CampaignEmailMessage (Prelude.Maybe Prelude.Text)
campaignEmailMessage_title = Lens.lens (\CampaignEmailMessage' {title} -> title) (\s@CampaignEmailMessage' {} a -> s {title = a} :: CampaignEmailMessage)

-- | The body of the email, in HTML format, for recipients whose email
-- clients render HTML content.
campaignEmailMessage_htmlBody :: Lens.Lens' CampaignEmailMessage (Prelude.Maybe Prelude.Text)
campaignEmailMessage_htmlBody = Lens.lens (\CampaignEmailMessage' {htmlBody} -> htmlBody) (\s@CampaignEmailMessage' {} a -> s {htmlBody = a} :: CampaignEmailMessage)

instance Core.FromJSON CampaignEmailMessage where
  parseJSON =
    Core.withObject
      "CampaignEmailMessage"
      ( \x ->
          CampaignEmailMessage'
            Prelude.<$> (x Core..:? "FromAddress")
            Prelude.<*> (x Core..:? "Body")
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "HtmlBody")
      )

instance Prelude.Hashable CampaignEmailMessage where
  hashWithSalt _salt CampaignEmailMessage' {..} =
    _salt `Prelude.hashWithSalt` fromAddress
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` htmlBody

instance Prelude.NFData CampaignEmailMessage where
  rnf CampaignEmailMessage' {..} =
    Prelude.rnf fromAddress
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf htmlBody

instance Core.ToJSON CampaignEmailMessage where
  toJSON CampaignEmailMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FromAddress" Core..=) Prelude.<$> fromAddress,
            ("Body" Core..=) Prelude.<$> body,
            ("Title" Core..=) Prelude.<$> title,
            ("HtmlBody" Core..=) Prelude.<$> htmlBody
          ]
      )
