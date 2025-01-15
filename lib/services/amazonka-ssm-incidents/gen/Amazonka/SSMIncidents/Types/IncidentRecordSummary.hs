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
-- Module      : Amazonka.SSMIncidents.Types.IncidentRecordSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.IncidentRecordSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.IncidentRecordSource
import Amazonka.SSMIncidents.Types.IncidentRecordStatus

-- | Details describing an incident record.
--
-- /See:/ 'newIncidentRecordSummary' smart constructor.
data IncidentRecordSummary = IncidentRecordSummary'
  { -- | The time the incident was resolved.
    resolvedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the incident.
    arn :: Prelude.Text,
    -- | The time the incident was created.
    creationTime :: Data.POSIX,
    -- | Defines the impact to customers and applications.
    impact :: Prelude.Natural,
    -- | What caused Incident Manager to create the incident.
    incidentRecordSource :: IncidentRecordSource,
    -- | The current status of the incident.
    status :: IncidentRecordStatus,
    -- | The title of the incident. This value is either provided by the response
    -- plan or overwritten on creation.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncidentRecordSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolvedTime', 'incidentRecordSummary_resolvedTime' - The time the incident was resolved.
--
-- 'arn', 'incidentRecordSummary_arn' - The Amazon Resource Name (ARN) of the incident.
--
-- 'creationTime', 'incidentRecordSummary_creationTime' - The time the incident was created.
--
-- 'impact', 'incidentRecordSummary_impact' - Defines the impact to customers and applications.
--
-- 'incidentRecordSource', 'incidentRecordSummary_incidentRecordSource' - What caused Incident Manager to create the incident.
--
-- 'status', 'incidentRecordSummary_status' - The current status of the incident.
--
-- 'title', 'incidentRecordSummary_title' - The title of the incident. This value is either provided by the response
-- plan or overwritten on creation.
newIncidentRecordSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'impact'
  Prelude.Natural ->
  -- | 'incidentRecordSource'
  IncidentRecordSource ->
  -- | 'status'
  IncidentRecordStatus ->
  -- | 'title'
  Prelude.Text ->
  IncidentRecordSummary
newIncidentRecordSummary
  pArn_
  pCreationTime_
  pImpact_
  pIncidentRecordSource_
  pStatus_
  pTitle_ =
    IncidentRecordSummary'
      { resolvedTime =
          Prelude.Nothing,
        arn = pArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        impact = pImpact_,
        incidentRecordSource = pIncidentRecordSource_,
        status = pStatus_,
        title = pTitle_
      }

-- | The time the incident was resolved.
incidentRecordSummary_resolvedTime :: Lens.Lens' IncidentRecordSummary (Prelude.Maybe Prelude.UTCTime)
incidentRecordSummary_resolvedTime = Lens.lens (\IncidentRecordSummary' {resolvedTime} -> resolvedTime) (\s@IncidentRecordSummary' {} a -> s {resolvedTime = a} :: IncidentRecordSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the incident.
incidentRecordSummary_arn :: Lens.Lens' IncidentRecordSummary Prelude.Text
incidentRecordSummary_arn = Lens.lens (\IncidentRecordSummary' {arn} -> arn) (\s@IncidentRecordSummary' {} a -> s {arn = a} :: IncidentRecordSummary)

-- | The time the incident was created.
incidentRecordSummary_creationTime :: Lens.Lens' IncidentRecordSummary Prelude.UTCTime
incidentRecordSummary_creationTime = Lens.lens (\IncidentRecordSummary' {creationTime} -> creationTime) (\s@IncidentRecordSummary' {} a -> s {creationTime = a} :: IncidentRecordSummary) Prelude.. Data._Time

-- | Defines the impact to customers and applications.
incidentRecordSummary_impact :: Lens.Lens' IncidentRecordSummary Prelude.Natural
incidentRecordSummary_impact = Lens.lens (\IncidentRecordSummary' {impact} -> impact) (\s@IncidentRecordSummary' {} a -> s {impact = a} :: IncidentRecordSummary)

-- | What caused Incident Manager to create the incident.
incidentRecordSummary_incidentRecordSource :: Lens.Lens' IncidentRecordSummary IncidentRecordSource
incidentRecordSummary_incidentRecordSource = Lens.lens (\IncidentRecordSummary' {incidentRecordSource} -> incidentRecordSource) (\s@IncidentRecordSummary' {} a -> s {incidentRecordSource = a} :: IncidentRecordSummary)

-- | The current status of the incident.
incidentRecordSummary_status :: Lens.Lens' IncidentRecordSummary IncidentRecordStatus
incidentRecordSummary_status = Lens.lens (\IncidentRecordSummary' {status} -> status) (\s@IncidentRecordSummary' {} a -> s {status = a} :: IncidentRecordSummary)

-- | The title of the incident. This value is either provided by the response
-- plan or overwritten on creation.
incidentRecordSummary_title :: Lens.Lens' IncidentRecordSummary Prelude.Text
incidentRecordSummary_title = Lens.lens (\IncidentRecordSummary' {title} -> title) (\s@IncidentRecordSummary' {} a -> s {title = a} :: IncidentRecordSummary)

instance Data.FromJSON IncidentRecordSummary where
  parseJSON =
    Data.withObject
      "IncidentRecordSummary"
      ( \x ->
          IncidentRecordSummary'
            Prelude.<$> (x Data..:? "resolvedTime")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "impact")
            Prelude.<*> (x Data..: "incidentRecordSource")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "title")
      )

instance Prelude.Hashable IncidentRecordSummary where
  hashWithSalt _salt IncidentRecordSummary' {..} =
    _salt
      `Prelude.hashWithSalt` resolvedTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` impact
      `Prelude.hashWithSalt` incidentRecordSource
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` title

instance Prelude.NFData IncidentRecordSummary where
  rnf IncidentRecordSummary' {..} =
    Prelude.rnf resolvedTime `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf impact `Prelude.seq`
            Prelude.rnf incidentRecordSource `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf title
