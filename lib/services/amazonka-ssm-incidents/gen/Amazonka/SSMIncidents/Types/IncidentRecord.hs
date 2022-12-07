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
-- Module      : Amazonka.SSMIncidents.Types.IncidentRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.IncidentRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.AutomationExecution
import Amazonka.SSMIncidents.Types.ChatChannel
import Amazonka.SSMIncidents.Types.IncidentRecordSource
import Amazonka.SSMIncidents.Types.IncidentRecordStatus
import Amazonka.SSMIncidents.Types.NotificationTargetItem

-- | The record of the incident that\'s created when an incident occurs.
--
-- /See:/ 'newIncidentRecord' smart constructor.
data IncidentRecord = IncidentRecord'
  { -- | The chat channel used for collaboration during an incident.
    chatChannel :: Prelude.Maybe ChatChannel,
    -- | The summary of the incident. The summary is a brief synopsis of what
    -- occurred, what\'s currently happening, and context of the incident.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The runbook, or automation document, that\'s run at the beginning of the
    -- incident.
    automationExecutions :: Prelude.Maybe [AutomationExecution],
    -- | The time at which the incident was resolved. This appears as a timeline
    -- event.
    resolvedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon SNS targets that are notified when updates are made to an
    -- incident.
    notificationTargets :: Prelude.Maybe [NotificationTargetItem],
    -- | The Amazon Resource Name (ARN) of the incident record.
    arn :: Prelude.Text,
    -- | The time that Incident Manager created the incident record.
    creationTime :: Data.POSIX,
    -- | The string Incident Manager uses to prevent duplicate incidents from
    -- being created by the same incident in the same account.
    dedupeString :: Prelude.Text,
    -- | The impact of the incident on customers and applications.
    impact :: Prelude.Natural,
    -- | Details about the action that started the incident.
    incidentRecordSource :: IncidentRecordSource,
    -- | Who modified the incident most recently.
    lastModifiedBy :: Prelude.Text,
    -- | The time at which the incident was most recently modified.
    lastModifiedTime :: Data.POSIX,
    -- | The current status of the incident.
    status :: IncidentRecordStatus,
    -- | The title of the incident.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncidentRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chatChannel', 'incidentRecord_chatChannel' - The chat channel used for collaboration during an incident.
--
-- 'summary', 'incidentRecord_summary' - The summary of the incident. The summary is a brief synopsis of what
-- occurred, what\'s currently happening, and context of the incident.
--
-- 'automationExecutions', 'incidentRecord_automationExecutions' - The runbook, or automation document, that\'s run at the beginning of the
-- incident.
--
-- 'resolvedTime', 'incidentRecord_resolvedTime' - The time at which the incident was resolved. This appears as a timeline
-- event.
--
-- 'notificationTargets', 'incidentRecord_notificationTargets' - The Amazon SNS targets that are notified when updates are made to an
-- incident.
--
-- 'arn', 'incidentRecord_arn' - The Amazon Resource Name (ARN) of the incident record.
--
-- 'creationTime', 'incidentRecord_creationTime' - The time that Incident Manager created the incident record.
--
-- 'dedupeString', 'incidentRecord_dedupeString' - The string Incident Manager uses to prevent duplicate incidents from
-- being created by the same incident in the same account.
--
-- 'impact', 'incidentRecord_impact' - The impact of the incident on customers and applications.
--
-- 'incidentRecordSource', 'incidentRecord_incidentRecordSource' - Details about the action that started the incident.
--
-- 'lastModifiedBy', 'incidentRecord_lastModifiedBy' - Who modified the incident most recently.
--
-- 'lastModifiedTime', 'incidentRecord_lastModifiedTime' - The time at which the incident was most recently modified.
--
-- 'status', 'incidentRecord_status' - The current status of the incident.
--
-- 'title', 'incidentRecord_title' - The title of the incident.
newIncidentRecord ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'dedupeString'
  Prelude.Text ->
  -- | 'impact'
  Prelude.Natural ->
  -- | 'incidentRecordSource'
  IncidentRecordSource ->
  -- | 'lastModifiedBy'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'status'
  IncidentRecordStatus ->
  -- | 'title'
  Prelude.Text ->
  IncidentRecord
newIncidentRecord
  pArn_
  pCreationTime_
  pDedupeString_
  pImpact_
  pIncidentRecordSource_
  pLastModifiedBy_
  pLastModifiedTime_
  pStatus_
  pTitle_ =
    IncidentRecord'
      { chatChannel = Prelude.Nothing,
        summary = Prelude.Nothing,
        automationExecutions = Prelude.Nothing,
        resolvedTime = Prelude.Nothing,
        notificationTargets = Prelude.Nothing,
        arn = pArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        dedupeString = pDedupeString_,
        impact = pImpact_,
        incidentRecordSource = pIncidentRecordSource_,
        lastModifiedBy = pLastModifiedBy_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        status = pStatus_,
        title = pTitle_
      }

-- | The chat channel used for collaboration during an incident.
incidentRecord_chatChannel :: Lens.Lens' IncidentRecord (Prelude.Maybe ChatChannel)
incidentRecord_chatChannel = Lens.lens (\IncidentRecord' {chatChannel} -> chatChannel) (\s@IncidentRecord' {} a -> s {chatChannel = a} :: IncidentRecord)

-- | The summary of the incident. The summary is a brief synopsis of what
-- occurred, what\'s currently happening, and context of the incident.
incidentRecord_summary :: Lens.Lens' IncidentRecord (Prelude.Maybe Prelude.Text)
incidentRecord_summary = Lens.lens (\IncidentRecord' {summary} -> summary) (\s@IncidentRecord' {} a -> s {summary = a} :: IncidentRecord)

-- | The runbook, or automation document, that\'s run at the beginning of the
-- incident.
incidentRecord_automationExecutions :: Lens.Lens' IncidentRecord (Prelude.Maybe [AutomationExecution])
incidentRecord_automationExecutions = Lens.lens (\IncidentRecord' {automationExecutions} -> automationExecutions) (\s@IncidentRecord' {} a -> s {automationExecutions = a} :: IncidentRecord) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the incident was resolved. This appears as a timeline
-- event.
incidentRecord_resolvedTime :: Lens.Lens' IncidentRecord (Prelude.Maybe Prelude.UTCTime)
incidentRecord_resolvedTime = Lens.lens (\IncidentRecord' {resolvedTime} -> resolvedTime) (\s@IncidentRecord' {} a -> s {resolvedTime = a} :: IncidentRecord) Prelude.. Lens.mapping Data._Time

-- | The Amazon SNS targets that are notified when updates are made to an
-- incident.
incidentRecord_notificationTargets :: Lens.Lens' IncidentRecord (Prelude.Maybe [NotificationTargetItem])
incidentRecord_notificationTargets = Lens.lens (\IncidentRecord' {notificationTargets} -> notificationTargets) (\s@IncidentRecord' {} a -> s {notificationTargets = a} :: IncidentRecord) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the incident record.
incidentRecord_arn :: Lens.Lens' IncidentRecord Prelude.Text
incidentRecord_arn = Lens.lens (\IncidentRecord' {arn} -> arn) (\s@IncidentRecord' {} a -> s {arn = a} :: IncidentRecord)

-- | The time that Incident Manager created the incident record.
incidentRecord_creationTime :: Lens.Lens' IncidentRecord Prelude.UTCTime
incidentRecord_creationTime = Lens.lens (\IncidentRecord' {creationTime} -> creationTime) (\s@IncidentRecord' {} a -> s {creationTime = a} :: IncidentRecord) Prelude.. Data._Time

-- | The string Incident Manager uses to prevent duplicate incidents from
-- being created by the same incident in the same account.
incidentRecord_dedupeString :: Lens.Lens' IncidentRecord Prelude.Text
incidentRecord_dedupeString = Lens.lens (\IncidentRecord' {dedupeString} -> dedupeString) (\s@IncidentRecord' {} a -> s {dedupeString = a} :: IncidentRecord)

-- | The impact of the incident on customers and applications.
incidentRecord_impact :: Lens.Lens' IncidentRecord Prelude.Natural
incidentRecord_impact = Lens.lens (\IncidentRecord' {impact} -> impact) (\s@IncidentRecord' {} a -> s {impact = a} :: IncidentRecord)

-- | Details about the action that started the incident.
incidentRecord_incidentRecordSource :: Lens.Lens' IncidentRecord IncidentRecordSource
incidentRecord_incidentRecordSource = Lens.lens (\IncidentRecord' {incidentRecordSource} -> incidentRecordSource) (\s@IncidentRecord' {} a -> s {incidentRecordSource = a} :: IncidentRecord)

-- | Who modified the incident most recently.
incidentRecord_lastModifiedBy :: Lens.Lens' IncidentRecord Prelude.Text
incidentRecord_lastModifiedBy = Lens.lens (\IncidentRecord' {lastModifiedBy} -> lastModifiedBy) (\s@IncidentRecord' {} a -> s {lastModifiedBy = a} :: IncidentRecord)

-- | The time at which the incident was most recently modified.
incidentRecord_lastModifiedTime :: Lens.Lens' IncidentRecord Prelude.UTCTime
incidentRecord_lastModifiedTime = Lens.lens (\IncidentRecord' {lastModifiedTime} -> lastModifiedTime) (\s@IncidentRecord' {} a -> s {lastModifiedTime = a} :: IncidentRecord) Prelude.. Data._Time

-- | The current status of the incident.
incidentRecord_status :: Lens.Lens' IncidentRecord IncidentRecordStatus
incidentRecord_status = Lens.lens (\IncidentRecord' {status} -> status) (\s@IncidentRecord' {} a -> s {status = a} :: IncidentRecord)

-- | The title of the incident.
incidentRecord_title :: Lens.Lens' IncidentRecord Prelude.Text
incidentRecord_title = Lens.lens (\IncidentRecord' {title} -> title) (\s@IncidentRecord' {} a -> s {title = a} :: IncidentRecord)

instance Data.FromJSON IncidentRecord where
  parseJSON =
    Data.withObject
      "IncidentRecord"
      ( \x ->
          IncidentRecord'
            Prelude.<$> (x Data..:? "chatChannel")
            Prelude.<*> (x Data..:? "summary")
            Prelude.<*> ( x Data..:? "automationExecutions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "resolvedTime")
            Prelude.<*> ( x Data..:? "notificationTargets"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "dedupeString")
            Prelude.<*> (x Data..: "impact")
            Prelude.<*> (x Data..: "incidentRecordSource")
            Prelude.<*> (x Data..: "lastModifiedBy")
            Prelude.<*> (x Data..: "lastModifiedTime")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "title")
      )

instance Prelude.Hashable IncidentRecord where
  hashWithSalt _salt IncidentRecord' {..} =
    _salt `Prelude.hashWithSalt` chatChannel
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` automationExecutions
      `Prelude.hashWithSalt` resolvedTime
      `Prelude.hashWithSalt` notificationTargets
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dedupeString
      `Prelude.hashWithSalt` impact
      `Prelude.hashWithSalt` incidentRecordSource
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` title

instance Prelude.NFData IncidentRecord where
  rnf IncidentRecord' {..} =
    Prelude.rnf chatChannel
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf automationExecutions
      `Prelude.seq` Prelude.rnf resolvedTime
      `Prelude.seq` Prelude.rnf notificationTargets
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dedupeString
      `Prelude.seq` Prelude.rnf impact
      `Prelude.seq` Prelude.rnf incidentRecordSource
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf title
