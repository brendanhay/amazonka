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
-- Module      : Amazonka.SSMIncidents.Types.IncidentTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.IncidentTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.NotificationTargetItem

-- | Basic details used in creating a response plan. The response plan is
-- then used to create an incident record.
--
-- /See:/ 'newIncidentTemplate' smart constructor.
data IncidentTemplate = IncidentTemplate'
  { -- | Used to stop Incident Manager from creating multiple incident records
    -- for the same incident.
    dedupeString :: Prelude.Maybe Prelude.Text,
    -- | Tags to assign to the template. When the @StartIncident@ API action is
    -- called, Incident Manager assigns the tags specified in the template to
    -- the incident.
    incidentTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon SNS targets that are notified when updates are made to an
    -- incident.
    notificationTargets :: Prelude.Maybe [NotificationTargetItem],
    -- | The summary of the incident. The summary is a brief synopsis of what
    -- occurred, what\'s currently happening, and context.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The impact of the incident on your customers and applications.
    impact :: Prelude.Natural,
    -- | The title of the incident.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncidentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedupeString', 'incidentTemplate_dedupeString' - Used to stop Incident Manager from creating multiple incident records
-- for the same incident.
--
-- 'incidentTags', 'incidentTemplate_incidentTags' - Tags to assign to the template. When the @StartIncident@ API action is
-- called, Incident Manager assigns the tags specified in the template to
-- the incident.
--
-- 'notificationTargets', 'incidentTemplate_notificationTargets' - The Amazon SNS targets that are notified when updates are made to an
-- incident.
--
-- 'summary', 'incidentTemplate_summary' - The summary of the incident. The summary is a brief synopsis of what
-- occurred, what\'s currently happening, and context.
--
-- 'impact', 'incidentTemplate_impact' - The impact of the incident on your customers and applications.
--
-- 'title', 'incidentTemplate_title' - The title of the incident.
newIncidentTemplate ::
  -- | 'impact'
  Prelude.Natural ->
  -- | 'title'
  Prelude.Text ->
  IncidentTemplate
newIncidentTemplate pImpact_ pTitle_ =
  IncidentTemplate'
    { dedupeString = Prelude.Nothing,
      incidentTags = Prelude.Nothing,
      notificationTargets = Prelude.Nothing,
      summary = Prelude.Nothing,
      impact = pImpact_,
      title = pTitle_
    }

-- | Used to stop Incident Manager from creating multiple incident records
-- for the same incident.
incidentTemplate_dedupeString :: Lens.Lens' IncidentTemplate (Prelude.Maybe Prelude.Text)
incidentTemplate_dedupeString = Lens.lens (\IncidentTemplate' {dedupeString} -> dedupeString) (\s@IncidentTemplate' {} a -> s {dedupeString = a} :: IncidentTemplate)

-- | Tags to assign to the template. When the @StartIncident@ API action is
-- called, Incident Manager assigns the tags specified in the template to
-- the incident.
incidentTemplate_incidentTags :: Lens.Lens' IncidentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
incidentTemplate_incidentTags = Lens.lens (\IncidentTemplate' {incidentTags} -> incidentTags) (\s@IncidentTemplate' {} a -> s {incidentTags = a} :: IncidentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon SNS targets that are notified when updates are made to an
-- incident.
incidentTemplate_notificationTargets :: Lens.Lens' IncidentTemplate (Prelude.Maybe [NotificationTargetItem])
incidentTemplate_notificationTargets = Lens.lens (\IncidentTemplate' {notificationTargets} -> notificationTargets) (\s@IncidentTemplate' {} a -> s {notificationTargets = a} :: IncidentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The summary of the incident. The summary is a brief synopsis of what
-- occurred, what\'s currently happening, and context.
incidentTemplate_summary :: Lens.Lens' IncidentTemplate (Prelude.Maybe Prelude.Text)
incidentTemplate_summary = Lens.lens (\IncidentTemplate' {summary} -> summary) (\s@IncidentTemplate' {} a -> s {summary = a} :: IncidentTemplate)

-- | The impact of the incident on your customers and applications.
incidentTemplate_impact :: Lens.Lens' IncidentTemplate Prelude.Natural
incidentTemplate_impact = Lens.lens (\IncidentTemplate' {impact} -> impact) (\s@IncidentTemplate' {} a -> s {impact = a} :: IncidentTemplate)

-- | The title of the incident.
incidentTemplate_title :: Lens.Lens' IncidentTemplate Prelude.Text
incidentTemplate_title = Lens.lens (\IncidentTemplate' {title} -> title) (\s@IncidentTemplate' {} a -> s {title = a} :: IncidentTemplate)

instance Data.FromJSON IncidentTemplate where
  parseJSON =
    Data.withObject
      "IncidentTemplate"
      ( \x ->
          IncidentTemplate'
            Prelude.<$> (x Data..:? "dedupeString")
            Prelude.<*> (x Data..:? "incidentTags" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "notificationTargets"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "summary")
            Prelude.<*> (x Data..: "impact")
            Prelude.<*> (x Data..: "title")
      )

instance Prelude.Hashable IncidentTemplate where
  hashWithSalt _salt IncidentTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` dedupeString
      `Prelude.hashWithSalt` incidentTags
      `Prelude.hashWithSalt` notificationTargets
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` impact
      `Prelude.hashWithSalt` title

instance Prelude.NFData IncidentTemplate where
  rnf IncidentTemplate' {..} =
    Prelude.rnf dedupeString
      `Prelude.seq` Prelude.rnf incidentTags
      `Prelude.seq` Prelude.rnf notificationTargets
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf impact
      `Prelude.seq` Prelude.rnf title

instance Data.ToJSON IncidentTemplate where
  toJSON IncidentTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dedupeString" Data..=) Prelude.<$> dedupeString,
            ("incidentTags" Data..=) Prelude.<$> incidentTags,
            ("notificationTargets" Data..=)
              Prelude.<$> notificationTargets,
            ("summary" Data..=) Prelude.<$> summary,
            Prelude.Just ("impact" Data..= impact),
            Prelude.Just ("title" Data..= title)
          ]
      )
