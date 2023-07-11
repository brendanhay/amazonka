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
-- Module      : Amazonka.SSMContacts.Types.Engagement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Engagement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Incident Manager reaching out to a contact or escalation plan to engage
-- contact during an incident.
--
-- /See:/ 'newEngagement' smart constructor.
data Engagement = Engagement'
  { -- | The ARN of the incident that\'s engaging the contact.
    incidentId :: Prelude.Maybe Prelude.Text,
    -- | The time that the engagement began.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The time that the engagement ended.
    stopTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the engagement.
    engagementArn :: Prelude.Text,
    -- | The ARN of the escalation plan or contact that Incident Manager is
    -- engaging.
    contactArn :: Prelude.Text,
    -- | The user that started the engagement.
    sender :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Engagement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'incidentId', 'engagement_incidentId' - The ARN of the incident that\'s engaging the contact.
--
-- 'startTime', 'engagement_startTime' - The time that the engagement began.
--
-- 'stopTime', 'engagement_stopTime' - The time that the engagement ended.
--
-- 'engagementArn', 'engagement_engagementArn' - The Amazon Resource Name (ARN) of the engagement.
--
-- 'contactArn', 'engagement_contactArn' - The ARN of the escalation plan or contact that Incident Manager is
-- engaging.
--
-- 'sender', 'engagement_sender' - The user that started the engagement.
newEngagement ::
  -- | 'engagementArn'
  Prelude.Text ->
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'sender'
  Prelude.Text ->
  Engagement
newEngagement pEngagementArn_ pContactArn_ pSender_ =
  Engagement'
    { incidentId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      stopTime = Prelude.Nothing,
      engagementArn = pEngagementArn_,
      contactArn = pContactArn_,
      sender = pSender_
    }

-- | The ARN of the incident that\'s engaging the contact.
engagement_incidentId :: Lens.Lens' Engagement (Prelude.Maybe Prelude.Text)
engagement_incidentId = Lens.lens (\Engagement' {incidentId} -> incidentId) (\s@Engagement' {} a -> s {incidentId = a} :: Engagement)

-- | The time that the engagement began.
engagement_startTime :: Lens.Lens' Engagement (Prelude.Maybe Prelude.UTCTime)
engagement_startTime = Lens.lens (\Engagement' {startTime} -> startTime) (\s@Engagement' {} a -> s {startTime = a} :: Engagement) Prelude.. Lens.mapping Data._Time

-- | The time that the engagement ended.
engagement_stopTime :: Lens.Lens' Engagement (Prelude.Maybe Prelude.UTCTime)
engagement_stopTime = Lens.lens (\Engagement' {stopTime} -> stopTime) (\s@Engagement' {} a -> s {stopTime = a} :: Engagement) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the engagement.
engagement_engagementArn :: Lens.Lens' Engagement Prelude.Text
engagement_engagementArn = Lens.lens (\Engagement' {engagementArn} -> engagementArn) (\s@Engagement' {} a -> s {engagementArn = a} :: Engagement)

-- | The ARN of the escalation plan or contact that Incident Manager is
-- engaging.
engagement_contactArn :: Lens.Lens' Engagement Prelude.Text
engagement_contactArn = Lens.lens (\Engagement' {contactArn} -> contactArn) (\s@Engagement' {} a -> s {contactArn = a} :: Engagement)

-- | The user that started the engagement.
engagement_sender :: Lens.Lens' Engagement Prelude.Text
engagement_sender = Lens.lens (\Engagement' {sender} -> sender) (\s@Engagement' {} a -> s {sender = a} :: Engagement)

instance Data.FromJSON Engagement where
  parseJSON =
    Data.withObject
      "Engagement"
      ( \x ->
          Engagement'
            Prelude.<$> (x Data..:? "IncidentId")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "StopTime")
            Prelude.<*> (x Data..: "EngagementArn")
            Prelude.<*> (x Data..: "ContactArn")
            Prelude.<*> (x Data..: "Sender")
      )

instance Prelude.Hashable Engagement where
  hashWithSalt _salt Engagement' {..} =
    _salt
      `Prelude.hashWithSalt` incidentId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` stopTime
      `Prelude.hashWithSalt` engagementArn
      `Prelude.hashWithSalt` contactArn
      `Prelude.hashWithSalt` sender

instance Prelude.NFData Engagement where
  rnf Engagement' {..} =
    Prelude.rnf incidentId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf stopTime
      `Prelude.seq` Prelude.rnf engagementArn
      `Prelude.seq` Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf sender
