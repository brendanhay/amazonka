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
-- Module      : Network.AWS.SESv2.Types.DomainDeliverabilityCampaign
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.DomainDeliverabilityCampaign where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains the deliverability data for a specific campaign.
-- This data is available for a campaign only if the campaign sent email by
-- using a domain that the Deliverability dashboard is enabled for
-- (@PutDeliverabilityDashboardOption@ operation).
--
-- /See:/ 'newDomainDeliverabilityCampaign' smart constructor.
data DomainDeliverabilityCampaign = DomainDeliverabilityCampaign'
  { -- | The projected number of recipients that the email message was sent to.
    projectedVolume :: Prelude.Maybe Prelude.Integer,
    -- | The number of email messages that were delivered to recipients’ inboxes.
    inboxCount :: Prelude.Maybe Prelude.Integer,
    -- | The percentage of email messages that were opened and then deleted by
    -- recipients. Due to technical limitations, this value only includes
    -- recipients who opened the message by using an email client that supports
    -- images.
    readDeleteRate :: Prelude.Maybe Prelude.Double,
    -- | The first time, in Unix time format, when the email message was
    -- delivered to any recipient\'s inbox. This value can help you determine
    -- how long it took for a campaign to deliver an email message.
    firstSeenDateTime :: Prelude.Maybe Core.POSIX,
    -- | The last time, in Unix time format, when the email message was delivered
    -- to any recipient\'s inbox. This value can help you determine how long it
    -- took for a campaign to deliver an email message.
    lastSeenDateTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier for the campaign. The Deliverability dashboard
    -- automatically generates and assigns this identifier to a campaign.
    campaignId :: Prelude.Maybe Prelude.Text,
    -- | The percentage of email messages that were deleted by recipients,
    -- without being opened first. Due to technical limitations, this value
    -- only includes recipients who opened the message by using an email client
    -- that supports images.
    deleteRate :: Prelude.Maybe Prelude.Double,
    -- | The URL of an image that contains a snapshot of the email message that
    -- was sent.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The number of email messages that were delivered to recipients\' spam or
    -- junk mail folders.
    spamCount :: Prelude.Maybe Prelude.Integer,
    -- | The subject line, or title, of the email message.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The IP addresses that were used to send the email message.
    sendingIps :: Prelude.Maybe [Prelude.Text],
    -- | The percentage of email messages that were opened by recipients. Due to
    -- technical limitations, this value only includes recipients who opened
    -- the message by using an email client that supports images.
    readRate :: Prelude.Maybe Prelude.Double,
    -- | The verified email address that the email message was sent from.
    fromAddress :: Prelude.Maybe Prelude.Text,
    -- | The major email providers who handled the email message.
    esps :: Prelude.Maybe [Prelude.Text]
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
-- 'projectedVolume', 'domainDeliverabilityCampaign_projectedVolume' - The projected number of recipients that the email message was sent to.
--
-- 'inboxCount', 'domainDeliverabilityCampaign_inboxCount' - The number of email messages that were delivered to recipients’ inboxes.
--
-- 'readDeleteRate', 'domainDeliverabilityCampaign_readDeleteRate' - The percentage of email messages that were opened and then deleted by
-- recipients. Due to technical limitations, this value only includes
-- recipients who opened the message by using an email client that supports
-- images.
--
-- 'firstSeenDateTime', 'domainDeliverabilityCampaign_firstSeenDateTime' - The first time, in Unix time format, when the email message was
-- delivered to any recipient\'s inbox. This value can help you determine
-- how long it took for a campaign to deliver an email message.
--
-- 'lastSeenDateTime', 'domainDeliverabilityCampaign_lastSeenDateTime' - The last time, in Unix time format, when the email message was delivered
-- to any recipient\'s inbox. This value can help you determine how long it
-- took for a campaign to deliver an email message.
--
-- 'campaignId', 'domainDeliverabilityCampaign_campaignId' - The unique identifier for the campaign. The Deliverability dashboard
-- automatically generates and assigns this identifier to a campaign.
--
-- 'deleteRate', 'domainDeliverabilityCampaign_deleteRate' - The percentage of email messages that were deleted by recipients,
-- without being opened first. Due to technical limitations, this value
-- only includes recipients who opened the message by using an email client
-- that supports images.
--
-- 'imageUrl', 'domainDeliverabilityCampaign_imageUrl' - The URL of an image that contains a snapshot of the email message that
-- was sent.
--
-- 'spamCount', 'domainDeliverabilityCampaign_spamCount' - The number of email messages that were delivered to recipients\' spam or
-- junk mail folders.
--
-- 'subject', 'domainDeliverabilityCampaign_subject' - The subject line, or title, of the email message.
--
-- 'sendingIps', 'domainDeliverabilityCampaign_sendingIps' - The IP addresses that were used to send the email message.
--
-- 'readRate', 'domainDeliverabilityCampaign_readRate' - The percentage of email messages that were opened by recipients. Due to
-- technical limitations, this value only includes recipients who opened
-- the message by using an email client that supports images.
--
-- 'fromAddress', 'domainDeliverabilityCampaign_fromAddress' - The verified email address that the email message was sent from.
--
-- 'esps', 'domainDeliverabilityCampaign_esps' - The major email providers who handled the email message.
newDomainDeliverabilityCampaign ::
  DomainDeliverabilityCampaign
newDomainDeliverabilityCampaign =
  DomainDeliverabilityCampaign'
    { projectedVolume =
        Prelude.Nothing,
      inboxCount = Prelude.Nothing,
      readDeleteRate = Prelude.Nothing,
      firstSeenDateTime = Prelude.Nothing,
      lastSeenDateTime = Prelude.Nothing,
      campaignId = Prelude.Nothing,
      deleteRate = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      spamCount = Prelude.Nothing,
      subject = Prelude.Nothing,
      sendingIps = Prelude.Nothing,
      readRate = Prelude.Nothing,
      fromAddress = Prelude.Nothing,
      esps = Prelude.Nothing
    }

-- | The projected number of recipients that the email message was sent to.
domainDeliverabilityCampaign_projectedVolume :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Integer)
domainDeliverabilityCampaign_projectedVolume = Lens.lens (\DomainDeliverabilityCampaign' {projectedVolume} -> projectedVolume) (\s@DomainDeliverabilityCampaign' {} a -> s {projectedVolume = a} :: DomainDeliverabilityCampaign)

-- | The number of email messages that were delivered to recipients’ inboxes.
domainDeliverabilityCampaign_inboxCount :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Integer)
domainDeliverabilityCampaign_inboxCount = Lens.lens (\DomainDeliverabilityCampaign' {inboxCount} -> inboxCount) (\s@DomainDeliverabilityCampaign' {} a -> s {inboxCount = a} :: DomainDeliverabilityCampaign)

-- | The percentage of email messages that were opened and then deleted by
-- recipients. Due to technical limitations, this value only includes
-- recipients who opened the message by using an email client that supports
-- images.
domainDeliverabilityCampaign_readDeleteRate :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Double)
domainDeliverabilityCampaign_readDeleteRate = Lens.lens (\DomainDeliverabilityCampaign' {readDeleteRate} -> readDeleteRate) (\s@DomainDeliverabilityCampaign' {} a -> s {readDeleteRate = a} :: DomainDeliverabilityCampaign)

-- | The first time, in Unix time format, when the email message was
-- delivered to any recipient\'s inbox. This value can help you determine
-- how long it took for a campaign to deliver an email message.
domainDeliverabilityCampaign_firstSeenDateTime :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.UTCTime)
domainDeliverabilityCampaign_firstSeenDateTime = Lens.lens (\DomainDeliverabilityCampaign' {firstSeenDateTime} -> firstSeenDateTime) (\s@DomainDeliverabilityCampaign' {} a -> s {firstSeenDateTime = a} :: DomainDeliverabilityCampaign) Prelude.. Lens.mapping Core._Time

-- | The last time, in Unix time format, when the email message was delivered
-- to any recipient\'s inbox. This value can help you determine how long it
-- took for a campaign to deliver an email message.
domainDeliverabilityCampaign_lastSeenDateTime :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.UTCTime)
domainDeliverabilityCampaign_lastSeenDateTime = Lens.lens (\DomainDeliverabilityCampaign' {lastSeenDateTime} -> lastSeenDateTime) (\s@DomainDeliverabilityCampaign' {} a -> s {lastSeenDateTime = a} :: DomainDeliverabilityCampaign) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for the campaign. The Deliverability dashboard
-- automatically generates and assigns this identifier to a campaign.
domainDeliverabilityCampaign_campaignId :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Text)
domainDeliverabilityCampaign_campaignId = Lens.lens (\DomainDeliverabilityCampaign' {campaignId} -> campaignId) (\s@DomainDeliverabilityCampaign' {} a -> s {campaignId = a} :: DomainDeliverabilityCampaign)

-- | The percentage of email messages that were deleted by recipients,
-- without being opened first. Due to technical limitations, this value
-- only includes recipients who opened the message by using an email client
-- that supports images.
domainDeliverabilityCampaign_deleteRate :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Double)
domainDeliverabilityCampaign_deleteRate = Lens.lens (\DomainDeliverabilityCampaign' {deleteRate} -> deleteRate) (\s@DomainDeliverabilityCampaign' {} a -> s {deleteRate = a} :: DomainDeliverabilityCampaign)

-- | The URL of an image that contains a snapshot of the email message that
-- was sent.
domainDeliverabilityCampaign_imageUrl :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Text)
domainDeliverabilityCampaign_imageUrl = Lens.lens (\DomainDeliverabilityCampaign' {imageUrl} -> imageUrl) (\s@DomainDeliverabilityCampaign' {} a -> s {imageUrl = a} :: DomainDeliverabilityCampaign)

-- | The number of email messages that were delivered to recipients\' spam or
-- junk mail folders.
domainDeliverabilityCampaign_spamCount :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Integer)
domainDeliverabilityCampaign_spamCount = Lens.lens (\DomainDeliverabilityCampaign' {spamCount} -> spamCount) (\s@DomainDeliverabilityCampaign' {} a -> s {spamCount = a} :: DomainDeliverabilityCampaign)

-- | The subject line, or title, of the email message.
domainDeliverabilityCampaign_subject :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Text)
domainDeliverabilityCampaign_subject = Lens.lens (\DomainDeliverabilityCampaign' {subject} -> subject) (\s@DomainDeliverabilityCampaign' {} a -> s {subject = a} :: DomainDeliverabilityCampaign)

-- | The IP addresses that were used to send the email message.
domainDeliverabilityCampaign_sendingIps :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe [Prelude.Text])
domainDeliverabilityCampaign_sendingIps = Lens.lens (\DomainDeliverabilityCampaign' {sendingIps} -> sendingIps) (\s@DomainDeliverabilityCampaign' {} a -> s {sendingIps = a} :: DomainDeliverabilityCampaign) Prelude.. Lens.mapping Lens._Coerce

-- | The percentage of email messages that were opened by recipients. Due to
-- technical limitations, this value only includes recipients who opened
-- the message by using an email client that supports images.
domainDeliverabilityCampaign_readRate :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Double)
domainDeliverabilityCampaign_readRate = Lens.lens (\DomainDeliverabilityCampaign' {readRate} -> readRate) (\s@DomainDeliverabilityCampaign' {} a -> s {readRate = a} :: DomainDeliverabilityCampaign)

-- | The verified email address that the email message was sent from.
domainDeliverabilityCampaign_fromAddress :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe Prelude.Text)
domainDeliverabilityCampaign_fromAddress = Lens.lens (\DomainDeliverabilityCampaign' {fromAddress} -> fromAddress) (\s@DomainDeliverabilityCampaign' {} a -> s {fromAddress = a} :: DomainDeliverabilityCampaign)

-- | The major email providers who handled the email message.
domainDeliverabilityCampaign_esps :: Lens.Lens' DomainDeliverabilityCampaign (Prelude.Maybe [Prelude.Text])
domainDeliverabilityCampaign_esps = Lens.lens (\DomainDeliverabilityCampaign' {esps} -> esps) (\s@DomainDeliverabilityCampaign' {} a -> s {esps = a} :: DomainDeliverabilityCampaign) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON DomainDeliverabilityCampaign where
  parseJSON =
    Core.withObject
      "DomainDeliverabilityCampaign"
      ( \x ->
          DomainDeliverabilityCampaign'
            Prelude.<$> (x Core..:? "ProjectedVolume")
            Prelude.<*> (x Core..:? "InboxCount")
            Prelude.<*> (x Core..:? "ReadDeleteRate")
            Prelude.<*> (x Core..:? "FirstSeenDateTime")
            Prelude.<*> (x Core..:? "LastSeenDateTime")
            Prelude.<*> (x Core..:? "CampaignId")
            Prelude.<*> (x Core..:? "DeleteRate")
            Prelude.<*> (x Core..:? "ImageUrl")
            Prelude.<*> (x Core..:? "SpamCount")
            Prelude.<*> (x Core..:? "Subject")
            Prelude.<*> (x Core..:? "SendingIps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ReadRate")
            Prelude.<*> (x Core..:? "FromAddress")
            Prelude.<*> (x Core..:? "Esps" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    DomainDeliverabilityCampaign

instance Prelude.NFData DomainDeliverabilityCampaign
