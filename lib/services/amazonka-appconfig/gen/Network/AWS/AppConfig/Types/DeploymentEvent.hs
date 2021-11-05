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
-- Module      : Network.AWS.AppConfig.Types.DeploymentEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppConfig.Types.DeploymentEvent where

import Network.AWS.AppConfig.Types.DeploymentEventType
import Network.AWS.AppConfig.Types.TriggeredBy
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that describes a deployment event.
--
-- /See:/ 'newDeploymentEvent' smart constructor.
data DeploymentEvent = DeploymentEvent'
  { -- | The entity that triggered the deployment event. Events can be triggered
    -- by a user, AWS AppConfig, an Amazon CloudWatch alarm, or an internal
    -- error.
    triggeredBy :: Prelude.Maybe TriggeredBy,
    -- | The date and time the event occurred.
    occurredAt :: Prelude.Maybe Core.POSIX,
    -- | The type of deployment event. Deployment event types include the start,
    -- stop, or completion of a deployment; a percentage update; the start or
    -- stop of a bake period; the start or completion of a rollback.
    eventType :: Prelude.Maybe DeploymentEventType,
    -- | A description of the deployment event. Descriptions include, but are not
    -- limited to, the user account or the CloudWatch alarm ARN that initiated
    -- a rollback, the percentage of hosts that received the deployment, or in
    -- the case of an internal error, a recommendation to attempt a new
    -- deployment.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggeredBy', 'deploymentEvent_triggeredBy' - The entity that triggered the deployment event. Events can be triggered
-- by a user, AWS AppConfig, an Amazon CloudWatch alarm, or an internal
-- error.
--
-- 'occurredAt', 'deploymentEvent_occurredAt' - The date and time the event occurred.
--
-- 'eventType', 'deploymentEvent_eventType' - The type of deployment event. Deployment event types include the start,
-- stop, or completion of a deployment; a percentage update; the start or
-- stop of a bake period; the start or completion of a rollback.
--
-- 'description', 'deploymentEvent_description' - A description of the deployment event. Descriptions include, but are not
-- limited to, the user account or the CloudWatch alarm ARN that initiated
-- a rollback, the percentage of hosts that received the deployment, or in
-- the case of an internal error, a recommendation to attempt a new
-- deployment.
newDeploymentEvent ::
  DeploymentEvent
newDeploymentEvent =
  DeploymentEvent'
    { triggeredBy = Prelude.Nothing,
      occurredAt = Prelude.Nothing,
      eventType = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The entity that triggered the deployment event. Events can be triggered
-- by a user, AWS AppConfig, an Amazon CloudWatch alarm, or an internal
-- error.
deploymentEvent_triggeredBy :: Lens.Lens' DeploymentEvent (Prelude.Maybe TriggeredBy)
deploymentEvent_triggeredBy = Lens.lens (\DeploymentEvent' {triggeredBy} -> triggeredBy) (\s@DeploymentEvent' {} a -> s {triggeredBy = a} :: DeploymentEvent)

-- | The date and time the event occurred.
deploymentEvent_occurredAt :: Lens.Lens' DeploymentEvent (Prelude.Maybe Prelude.UTCTime)
deploymentEvent_occurredAt = Lens.lens (\DeploymentEvent' {occurredAt} -> occurredAt) (\s@DeploymentEvent' {} a -> s {occurredAt = a} :: DeploymentEvent) Prelude.. Lens.mapping Core._Time

-- | The type of deployment event. Deployment event types include the start,
-- stop, or completion of a deployment; a percentage update; the start or
-- stop of a bake period; the start or completion of a rollback.
deploymentEvent_eventType :: Lens.Lens' DeploymentEvent (Prelude.Maybe DeploymentEventType)
deploymentEvent_eventType = Lens.lens (\DeploymentEvent' {eventType} -> eventType) (\s@DeploymentEvent' {} a -> s {eventType = a} :: DeploymentEvent)

-- | A description of the deployment event. Descriptions include, but are not
-- limited to, the user account or the CloudWatch alarm ARN that initiated
-- a rollback, the percentage of hosts that received the deployment, or in
-- the case of an internal error, a recommendation to attempt a new
-- deployment.
deploymentEvent_description :: Lens.Lens' DeploymentEvent (Prelude.Maybe Prelude.Text)
deploymentEvent_description = Lens.lens (\DeploymentEvent' {description} -> description) (\s@DeploymentEvent' {} a -> s {description = a} :: DeploymentEvent)

instance Core.FromJSON DeploymentEvent where
  parseJSON =
    Core.withObject
      "DeploymentEvent"
      ( \x ->
          DeploymentEvent'
            Prelude.<$> (x Core..:? "TriggeredBy")
            Prelude.<*> (x Core..:? "OccurredAt")
            Prelude.<*> (x Core..:? "EventType")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable DeploymentEvent

instance Prelude.NFData DeploymentEvent
