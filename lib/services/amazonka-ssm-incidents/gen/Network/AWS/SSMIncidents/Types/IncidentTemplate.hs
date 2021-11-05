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
-- Module      : Network.AWS.SSMIncidents.Types.IncidentTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.IncidentTemplate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSMIncidents.Types.NotificationTargetItem

-- | Basic details used in creating a response plan. The response plan is
-- then used to create an incident record.
--
-- /See:/ 'newIncidentTemplate' smart constructor.
data IncidentTemplate = IncidentTemplate'
  { -- | The summary of the incident. The summary is a brief synopsis of what
    -- occurred, what\'s currently happening, and context.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The SNS targets that are notified when updates are made to an incident.
    notificationTargets :: Prelude.Maybe [NotificationTargetItem],
    -- | Used to stop Incident Manager from creating multiple incident records
    -- for the same incident.
    dedupeString :: Prelude.Maybe Prelude.Text,
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
-- 'summary', 'incidentTemplate_summary' - The summary of the incident. The summary is a brief synopsis of what
-- occurred, what\'s currently happening, and context.
--
-- 'notificationTargets', 'incidentTemplate_notificationTargets' - The SNS targets that are notified when updates are made to an incident.
--
-- 'dedupeString', 'incidentTemplate_dedupeString' - Used to stop Incident Manager from creating multiple incident records
-- for the same incident.
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
    { summary = Prelude.Nothing,
      notificationTargets = Prelude.Nothing,
      dedupeString = Prelude.Nothing,
      impact = pImpact_,
      title = pTitle_
    }

-- | The summary of the incident. The summary is a brief synopsis of what
-- occurred, what\'s currently happening, and context.
incidentTemplate_summary :: Lens.Lens' IncidentTemplate (Prelude.Maybe Prelude.Text)
incidentTemplate_summary = Lens.lens (\IncidentTemplate' {summary} -> summary) (\s@IncidentTemplate' {} a -> s {summary = a} :: IncidentTemplate)

-- | The SNS targets that are notified when updates are made to an incident.
incidentTemplate_notificationTargets :: Lens.Lens' IncidentTemplate (Prelude.Maybe [NotificationTargetItem])
incidentTemplate_notificationTargets = Lens.lens (\IncidentTemplate' {notificationTargets} -> notificationTargets) (\s@IncidentTemplate' {} a -> s {notificationTargets = a} :: IncidentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Used to stop Incident Manager from creating multiple incident records
-- for the same incident.
incidentTemplate_dedupeString :: Lens.Lens' IncidentTemplate (Prelude.Maybe Prelude.Text)
incidentTemplate_dedupeString = Lens.lens (\IncidentTemplate' {dedupeString} -> dedupeString) (\s@IncidentTemplate' {} a -> s {dedupeString = a} :: IncidentTemplate)

-- | The impact of the incident on your customers and applications.
incidentTemplate_impact :: Lens.Lens' IncidentTemplate Prelude.Natural
incidentTemplate_impact = Lens.lens (\IncidentTemplate' {impact} -> impact) (\s@IncidentTemplate' {} a -> s {impact = a} :: IncidentTemplate)

-- | The title of the incident.
incidentTemplate_title :: Lens.Lens' IncidentTemplate Prelude.Text
incidentTemplate_title = Lens.lens (\IncidentTemplate' {title} -> title) (\s@IncidentTemplate' {} a -> s {title = a} :: IncidentTemplate)

instance Core.FromJSON IncidentTemplate where
  parseJSON =
    Core.withObject
      "IncidentTemplate"
      ( \x ->
          IncidentTemplate'
            Prelude.<$> (x Core..:? "summary")
            Prelude.<*> ( x Core..:? "notificationTargets"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "dedupeString")
            Prelude.<*> (x Core..: "impact")
            Prelude.<*> (x Core..: "title")
      )

instance Prelude.Hashable IncidentTemplate

instance Prelude.NFData IncidentTemplate

instance Core.ToJSON IncidentTemplate where
  toJSON IncidentTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("summary" Core..=) Prelude.<$> summary,
            ("notificationTargets" Core..=)
              Prelude.<$> notificationTargets,
            ("dedupeString" Core..=) Prelude.<$> dedupeString,
            Prelude.Just ("impact" Core..= impact),
            Prelude.Just ("title" Core..= title)
          ]
      )
