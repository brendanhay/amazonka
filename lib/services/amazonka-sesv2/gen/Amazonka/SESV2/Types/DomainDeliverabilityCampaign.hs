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
-- Module      : Amazonka.SESV2.Types.DomainDeliverabilityCampaign
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DomainDeliverabilityCampaign where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the deliverability data for a specific campaign.
-- This data is available for a campaign only if the campaign sent email by
-- using a domain that the Deliverability dashboard is enabled for
-- (@PutDeliverabilityDashboardOption@ operation).
--
-- /See:/ 'newDomainDeliverabilityCampaign' smart constructor.
data DomainDeliverabilityCampaign = DomainDeliverabilityCampaign'
  { -- | The number of email messages that were delivered to recipients’ inboxes.
    inboxCount :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier for the campaign. The Deliverability dashboard
    -- automatically generates and assigns this identifier to a campaign.
    campaignId :: Prelude.Maybe Prelude.Text,
    -- | The verified email address that the email message was sent from.
    fromAddress :: Prelude.Maybe Prelude.Text,
    -- | The percentage of email messages that were deleted by recipients,
    -- without being opened first. Due to technical limitations, this value
    -- only includes recipients who opened the message by using an email client
    -- that supports images.
    deleteRate :: Prelude.Maybe Prelude.Double,
    -- | The last time when the email message was delivered to any recipient\'s
    -- inbox. This value can help you determine how long it took for a campaign
    -- to deliver an email message.
    lastSeenDateTime :: Prelude.Maybe Core.POSIX,
    -- | The IP addresses that were used to send the email message.
    sendingIps :: Prelude.Maybe [Prelude.Text],
    -- | The URL of an image that contains a snapshot of the email message that
    -- was sent.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The major email providers who handled the email message.
    esps :: Prelude.Maybe [Prelude.Text],
    -- | The projected number of recipients that the email message was sent to.
    projectedVolume :: Prelude.Maybe Prelude.Integer,
    -- | The percentage of email messages that were opened and then deleted by
    -- recipients. Due to technical limitations, this value only includes
    -- recipients who opened the message by using an email client that supports
    -- images.
    readDeleteRate :: Prelude.Maybe Prelude.Double,
    -- | The number of email messages that were delivered to recipients\' spam or
    -- junk mail folders.
    spamCount :: Prelude.Maybe Prelude.Integer,
    -- | The subject line, or title, of the email message.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The first time when the email message was delivered to any recipient\'s
    -- inbox. This value can help you determine how long it took for a campaign
    -- to deliver an email message.
    firstSeenDateTime :: Prelude.Maybe Core.POSIX,
    -- | The percentage of email messages that were opened by recipients. Due to
    -- technical limitations, this value only includes recipients who opened
    -- the message by using an email client that supports images.
    readRate :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainDeliverabilityCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inboxCount', 'domainDeliverabilityCampaign_inboxCount' - The number of email messages that were delivered to recipients’ inboxes.
--
-- 'campaignId', 'domainDeliverabilityCampaign_campaignId' - The unique identifier for the campaign. The Deliverability dashboard
-- automatically generates and assigns this identifier to a campaign.
--
-- 'fromAddress', 'domainDeliverabilityCampaign_fromAddress' - The verified email address that the email message was sent from.
--
-- 'deleteRate', 'domainDeliverabilityCampaign_deleteRate' - The percentage of email messages that were deleted by recipients,
-- without being opened first. Due to technical limitations, this value
-- only includes recipients who opened the message by using an email client
-- that supports images.
--
-- 'lastSeenDateTime', 'domainDeliverabilityCampaign_lastSeenDateTime' - The last time when the email message was delivered to any recipient\'s
-- inbox. This value can help you determine how long it took for a campaign
-- to deliver an email message.
--
-- 'sendingIps', 'domainDeliverabilityCampaign_sendingIps' - The IP addresses that were used to send the email message.
--
-- 'imageUrl', 'domainDeliverabilityCampaign_imageUrl' - The URL of an image that contains a snapshot of the email message that
-- was sent.
--
-- 'esps', 'domainDeliverabilityCampaign_esps' - The major email providers who handled the email message.
--
-- 'projectedVolume', 'domainDeliverabilityCampaign_projectedVolume' - The projected number of recipients that the email message was sent to.
--
-- 'readDeleteRate', 'domainDeliverabilityCampaign_readDeleteRate' - The percentage of email messages that were opened and then deleted by
-- recipients. Due to technical limitations, this value only includes
-- recipients who opened the message by using an email client that supports
-- images.
--
-- 'spamCount', 'domainDeliverabilityCampaign_spamCount' - The number of email messages that were delivered to recipients\' spam or
-- junk mail folders.
--
-- 'subject', 'domainDeliverabilityCampaign_subject' - The subject line, or title, of the email message.
--
-- 'firstSeenDateTime', 'domainDeliverabilityCampaign_firstSeenDateTime' - The first time when the email message was delivered to any recipient\'s
-- inbox. This value can help you determine how long it took for a campaign
-- to deliver an email message.
--
-- 'readRate', 'domainDeliverabilityCampaign_readRate' - The percentage of email messages that were opened by recipients. Due to
-- technical limitations, this value only includes recipients who opened
-- the message by using an email client that supports images.
newDomainDeliverabilityCampaign ::
  DomainDeliverabilityCampaign
newDomainDeliverabilityCampaign =
  DomainDeliverabilityCampaign'
    { inboxCount =
        Prelude.Nothing,
      campaignId = Prelude.Nothing,
      fromAddress = Prelude.Nothing,
      deleteRate = Prelude.Nothing,
      lastSeenDateTime = Prelude.Nothing,
      sendingIps = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      esps = Prelude.Nothing,
      projectedVolume = Prelude.Nothing,
      readDeleteRate = Prelude.Nothing,
      spamCount = Prelude.Nothing,
      subject = Prelude.Nothing,
      firstSeenDateTime = Prelude.Nothing,
      readRate = Prelude.Nothing
    }

-- | The number of email messages that were delivered to recipients’ inboxes.
domainDeliverabilityCampaign_inboxCount :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Integer)
domainDeliverabilityCampaign_inboxCount = Lens.lens (\DomainDeliverabilityCampaign' {inboxCount} -> inboxCount) (\s@DomainDeliverabilityCampaign' {} a -> s {inboxCount = a} :: DomainDeliverabilityCampaign)

-- | The unique identifier for the campaign. The Deliverability dashboard
-- automatically generates and assigns this identifier to a campaign.
domainDeliverabilityCampaign_campaignId :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Text)
domainDeliverabilityCampaign_campaignId = Lens.lens (\DomainDeliverabilityCampaign' {campaignId} -> campaignId) (\s@DomainDeliverabilityCampaign' {} a -> s {campaignId = a} :: DomainDeliverabilityCampaign)

-- | The verified email address that the email message was sent from.
domainDeliverabilityCampaign_fromAddress :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Text)
domainDeliverabilityCampaign_fromAddress = Lens.lens (\DomainDeliverabilityCampaign' {fromAddress} -> fromAddress) (\s@DomainDeliverabilityCampaign' {} a -> s {fromAddress = a} :: DomainDeliverabilityCampaign)

-- | The percentage of email messages that were deleted by recipients,
-- without being opened first. Due to technical limitations, this value
-- only includes recipients who opened the message by using an email client
-- that supports images.
domainDeliverabilityCampaign_deleteRate :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Double)
domainDeliverabilityCampaign_deleteRate = Lens.lens (\DomainDeliverabilityCampaign' {deleteRate} -> deleteRate) (\s@DomainDeliverabilityCampaign' {} a -> s {deleteRate = a} :: DomainDeliverabilityCampaign)

-- | The last time when the email message was delivered to any recipient\'s
-- inbox. This value can help you determine how long it took for a campaign
-- to deliver an email message.
domainDeliverabilityCampaign_lastSeenDateTime :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.UTCTime)
domainDeliverabilityCampaign_lastSeenDateTime = Lens.lens (\DomainDeliverabilityCampaign' {lastSeenDateTime} -> lastSeenDateTime) (\s@DomainDeliverabilityCampaign' {} a -> s {lastSeenDateTime = a} :: DomainDeliverabilityCampaign) Prelude.. Lens.mapping Core._Time

-- | The IP addresses that were used to send the email message.
domainDeliverabilityCampaign_sendingIps :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe [Prelude.Text])
domainDeliverabilityCampaign_sendingIps = Lens.lens (\DomainDeliverabilityCampaign' {sendingIps} -> sendingIps) (\s@DomainDeliverabilityCampaign' {} a -> s {sendingIps = a} :: DomainDeliverabilityCampaign) Prelude.. Lens.mapping Lens.coerced

-- | The URL of an image that contains a snapshot of the email message that
-- was sent.
domainDeliverabilityCampaign_imageUrl :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Text)
domainDeliverabilityCampaign_imageUrl = Lens.lens (\DomainDeliverabilityCampaign' {imageUrl} -> imageUrl) (\s@DomainDeliverabilityCampaign' {} a -> s {imageUrl = a} :: DomainDeliverabilityCampaign)

-- | The major email providers who handled the email message.
domainDeliverabilityCampaign_esps :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe [Prelude.Text])
domainDeliverabilityCampaign_esps = Lens.lens (\DomainDeliverabilityCampaign' {esps} -> esps) (\s@DomainDeliverabilityCampaign' {} a -> s {esps = a} :: DomainDeliverabilityCampaign) Prelude.. Lens.mapping Lens.coerced

-- | The projected number of recipients that the email message was sent to.
domainDeliverabilityCampaign_projectedVolume :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Integer)
domainDeliverabilityCampaign_projectedVolume = Lens.lens (\DomainDeliverabilityCampaign' {projectedVolume} -> projectedVolume) (\s@DomainDeliverabilityCampaign' {} a -> s {projectedVolume = a} :: DomainDeliverabilityCampaign)

-- | The percentage of email messages that were opened and then deleted by
-- recipients. Due to technical limitations, this value only includes
-- recipients who opened the message by using an email client that supports
-- images.
domainDeliverabilityCampaign_readDeleteRate :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Double)
domainDeliverabilityCampaign_readDeleteRate = Lens.lens (\DomainDeliverabilityCampaign' {readDeleteRate} -> readDeleteRate) (\s@DomainDeliverabilityCampaign' {} a -> s {readDeleteRate = a} :: DomainDeliverabilityCampaign)

-- | The number of email messages that were delivered to recipients\' spam or
-- junk mail folders.
domainDeliverabilityCampaign_spamCount :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Integer)
domainDeliverabilityCampaign_spamCount = Lens.lens (\DomainDeliverabilityCampaign' {spamCount} -> spamCount) (\s@DomainDeliverabilityCampaign' {} a -> s {spamCount = a} :: DomainDeliverabilityCampaign)

-- | The subject line, or title, of the email message.
domainDeliverabilityCampaign_subject :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Text)
domainDeliverabilityCampaign_subject = Lens.lens (\DomainDeliverabilityCampaign' {subject} -> subject) (\s@DomainDeliverabilityCampaign' {} a -> s {subject = a} :: DomainDeliverabilityCampaign)

-- | The first time when the email message was delivered to any recipient\'s
-- inbox. This value can help you determine how long it took for a campaign
-- to deliver an email message.
domainDeliverabilityCampaign_firstSeenDateTime :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.UTCTime)
domainDeliverabilityCampaign_firstSeenDateTime = Lens.lens (\DomainDeliverabilityCampaign' {firstSeenDateTime} -> firstSeenDateTime) (\s@DomainDeliverabilityCampaign' {} a -> s {firstSeenDateTime = a} :: DomainDeliverabilityCampaign) Prelude.. Lens.mapping Core._Time

-- | The percentage of email messages that were opened by recipients. Due to
-- technical limitations, this value only includes recipients who opened
-- the message by using an email client that supports images.
domainDeliverabilityCampaign_readRate :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Double)
domainDeliverabilityCampaign_readRate = Lens.lens (\DomainDeliverabilityCampaign' {readRate} -> readRate) (\s@DomainDeliverabilityCampaign' {} a -> s {readRate = a} :: DomainDeliverabilityCampaign)

instance Core.FromJSON DomainDeliverabilityCampaign where
  parseJSON =
    Core.withObject
      "DomainDeliverabilityCampaign"
      ( \x ->
          DomainDeliverabilityCampaign'
            Prelude.<$> (x Core..:? "InboxCount")
            Prelude.<*> (x Core..:? "CampaignId")
            Prelude.<*> (x Core..:? "FromAddress")
            Prelude.<*> (x Core..:? "DeleteRate")
            Prelude.<*> (x Core..:? "LastSeenDateTime")
            Prelude.<*> (x Core..:? "SendingIps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ImageUrl")
            Prelude.<*> (x Core..:? "Esps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ProjectedVolume")
            Prelude.<*> (x Core..:? "ReadDeleteRate")
            Prelude.<*> (x Core..:? "SpamCount")
            Prelude.<*> (x Core..:? "Subject")
            Prelude.<*> (x Core..:? "FirstSeenDateTime")
            Prelude.<*> (x Core..:? "ReadRate")
      )

instance
  Prelude.Hashable
    DomainDeliverabilityCampaign
  where
  hashWithSalt _salt DomainDeliverabilityCampaign' {..} =
    _salt `Prelude.hashWithSalt` inboxCount
      `Prelude.hashWithSalt` campaignId
      `Prelude.hashWithSalt` fromAddress
      `Prelude.hashWithSalt` deleteRate
      `Prelude.hashWithSalt` lastSeenDateTime
      `Prelude.hashWithSalt` sendingIps
      `Prelude.hashWithSalt` imageUrl
      `Prelude.hashWithSalt` esps
      `Prelude.hashWithSalt` projectedVolume
      `Prelude.hashWithSalt` readDeleteRate
      `Prelude.hashWithSalt` spamCount
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` firstSeenDateTime
      `Prelude.hashWithSalt` readRate

instance Prelude.NFData DomainDeliverabilityCampaign where
  rnf DomainDeliverabilityCampaign' {..} =
    Prelude.rnf inboxCount
      `Prelude.seq` Prelude.rnf campaignId
      `Prelude.seq` Prelude.rnf fromAddress
      `Prelude.seq` Prelude.rnf deleteRate
      `Prelude.seq` Prelude.rnf lastSeenDateTime
      `Prelude.seq` Prelude.rnf sendingIps
      `Prelude.seq` Prelude.rnf imageUrl
      `Prelude.seq` Prelude.rnf esps
      `Prelude.seq` Prelude.rnf projectedVolume
      `Prelude.seq` Prelude.rnf readDeleteRate
      `Prelude.seq` Prelude.rnf spamCount
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf firstSeenDateTime
      `Prelude.seq` Prelude.rnf readRate
