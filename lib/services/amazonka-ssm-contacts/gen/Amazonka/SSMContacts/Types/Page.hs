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
-- Module      : Amazonka.SSMContacts.Types.Page
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Page where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Incident Manager engaging a contact\'s contact channel.
--
-- /See:/ 'newPage' smart constructor.
data Page = Page'
  { -- | The time the message was delivered to the contact channel.
    deliveryTime :: Prelude.Maybe Core.POSIX,
    -- | The time that Incident Manager engaged the contact channel.
    sentTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the incident that\'s engaging the contact channel.
    incidentId :: Prelude.Maybe Prelude.Text,
    -- | The time that the contact channel acknowledged engagement.
    readTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the page to the contact channel.
    pageArn :: Prelude.Text,
    -- | The ARN of the engagement that this page is part of.
    engagementArn :: Prelude.Text,
    -- | The ARN of the contact that Incident Manager is engaging.
    contactArn :: Prelude.Text,
    -- | The user that started the engagement.
    sender :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Page' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryTime', 'page_deliveryTime' - The time the message was delivered to the contact channel.
--
-- 'sentTime', 'page_sentTime' - The time that Incident Manager engaged the contact channel.
--
-- 'incidentId', 'page_incidentId' - The ARN of the incident that\'s engaging the contact channel.
--
-- 'readTime', 'page_readTime' - The time that the contact channel acknowledged engagement.
--
-- 'pageArn', 'page_pageArn' - The Amazon Resource Name (ARN) of the page to the contact channel.
--
-- 'engagementArn', 'page_engagementArn' - The ARN of the engagement that this page is part of.
--
-- 'contactArn', 'page_contactArn' - The ARN of the contact that Incident Manager is engaging.
--
-- 'sender', 'page_sender' - The user that started the engagement.
newPage ::
  -- | 'pageArn'
  Prelude.Text ->
  -- | 'engagementArn'
  Prelude.Text ->
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'sender'
  Prelude.Text ->
  Page
newPage
  pPageArn_
  pEngagementArn_
  pContactArn_
  pSender_ =
    Page'
      { deliveryTime = Prelude.Nothing,
        sentTime = Prelude.Nothing,
        incidentId = Prelude.Nothing,
        readTime = Prelude.Nothing,
        pageArn = pPageArn_,
        engagementArn = pEngagementArn_,
        contactArn = pContactArn_,
        sender = pSender_
      }

-- | The time the message was delivered to the contact channel.
page_deliveryTime :: Lens.Lens' Page (Prelude.Maybe Prelude.UTCTime)
page_deliveryTime = Lens.lens (\Page' {deliveryTime} -> deliveryTime) (\s@Page' {} a -> s {deliveryTime = a} :: Page) Prelude.. Lens.mapping Core._Time

-- | The time that Incident Manager engaged the contact channel.
page_sentTime :: Lens.Lens' Page (Prelude.Maybe Prelude.UTCTime)
page_sentTime = Lens.lens (\Page' {sentTime} -> sentTime) (\s@Page' {} a -> s {sentTime = a} :: Page) Prelude.. Lens.mapping Core._Time

-- | The ARN of the incident that\'s engaging the contact channel.
page_incidentId :: Lens.Lens' Page (Prelude.Maybe Prelude.Text)
page_incidentId = Lens.lens (\Page' {incidentId} -> incidentId) (\s@Page' {} a -> s {incidentId = a} :: Page)

-- | The time that the contact channel acknowledged engagement.
page_readTime :: Lens.Lens' Page (Prelude.Maybe Prelude.UTCTime)
page_readTime = Lens.lens (\Page' {readTime} -> readTime) (\s@Page' {} a -> s {readTime = a} :: Page) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the page to the contact channel.
page_pageArn :: Lens.Lens' Page Prelude.Text
page_pageArn = Lens.lens (\Page' {pageArn} -> pageArn) (\s@Page' {} a -> s {pageArn = a} :: Page)

-- | The ARN of the engagement that this page is part of.
page_engagementArn :: Lens.Lens' Page Prelude.Text
page_engagementArn = Lens.lens (\Page' {engagementArn} -> engagementArn) (\s@Page' {} a -> s {engagementArn = a} :: Page)

-- | The ARN of the contact that Incident Manager is engaging.
page_contactArn :: Lens.Lens' Page Prelude.Text
page_contactArn = Lens.lens (\Page' {contactArn} -> contactArn) (\s@Page' {} a -> s {contactArn = a} :: Page)

-- | The user that started the engagement.
page_sender :: Lens.Lens' Page Prelude.Text
page_sender = Lens.lens (\Page' {sender} -> sender) (\s@Page' {} a -> s {sender = a} :: Page)

instance Core.FromJSON Page where
  parseJSON =
    Core.withObject
      "Page"
      ( \x ->
          Page'
            Prelude.<$> (x Core..:? "DeliveryTime")
            Prelude.<*> (x Core..:? "SentTime")
            Prelude.<*> (x Core..:? "IncidentId")
            Prelude.<*> (x Core..:? "ReadTime")
            Prelude.<*> (x Core..: "PageArn")
            Prelude.<*> (x Core..: "EngagementArn")
            Prelude.<*> (x Core..: "ContactArn")
            Prelude.<*> (x Core..: "Sender")
      )

instance Prelude.Hashable Page where
  hashWithSalt _salt Page' {..} =
    _salt `Prelude.hashWithSalt` deliveryTime
      `Prelude.hashWithSalt` sentTime
      `Prelude.hashWithSalt` incidentId
      `Prelude.hashWithSalt` readTime
      `Prelude.hashWithSalt` pageArn
      `Prelude.hashWithSalt` engagementArn
      `Prelude.hashWithSalt` contactArn
      `Prelude.hashWithSalt` sender

instance Prelude.NFData Page where
  rnf Page' {..} =
    Prelude.rnf deliveryTime
      `Prelude.seq` Prelude.rnf sentTime
      `Prelude.seq` Prelude.rnf incidentId
      `Prelude.seq` Prelude.rnf readTime
      `Prelude.seq` Prelude.rnf pageArn
      `Prelude.seq` Prelude.rnf engagementArn
      `Prelude.seq` Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf sender
