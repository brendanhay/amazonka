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
-- Module      : Amazonka.AppConfig.Types.DeploymentEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.DeploymentEvent where

import Amazonka.AppConfig.Types.ActionInvocation
import Amazonka.AppConfig.Types.DeploymentEventType
import Amazonka.AppConfig.Types.TriggeredBy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that describes a deployment event.
--
-- /See:/ 'newDeploymentEvent' smart constructor.
data DeploymentEvent = DeploymentEvent'
  { -- | The list of extensions that were invoked as part of the deployment.
    actionInvocations :: Prelude.Maybe [ActionInvocation],
    -- | A description of the deployment event. Descriptions include, but are not
    -- limited to, the user account or the Amazon CloudWatch alarm ARN that
    -- initiated a rollback, the percentage of hosts that received the
    -- deployment, or in the case of an internal error, a recommendation to
    -- attempt a new deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of deployment event. Deployment event types include the start,
    -- stop, or completion of a deployment; a percentage update; the start or
    -- stop of a bake period; and the start or completion of a rollback.
    eventType :: Prelude.Maybe DeploymentEventType,
    -- | The date and time the event occurred.
    occurredAt :: Prelude.Maybe Data.POSIX,
    -- | The entity that triggered the deployment event. Events can be triggered
    -- by a user, AppConfig, an Amazon CloudWatch alarm, or an internal error.
    triggeredBy :: Prelude.Maybe TriggeredBy
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
-- 'actionInvocations', 'deploymentEvent_actionInvocations' - The list of extensions that were invoked as part of the deployment.
--
-- 'description', 'deploymentEvent_description' - A description of the deployment event. Descriptions include, but are not
-- limited to, the user account or the Amazon CloudWatch alarm ARN that
-- initiated a rollback, the percentage of hosts that received the
-- deployment, or in the case of an internal error, a recommendation to
-- attempt a new deployment.
--
-- 'eventType', 'deploymentEvent_eventType' - The type of deployment event. Deployment event types include the start,
-- stop, or completion of a deployment; a percentage update; the start or
-- stop of a bake period; and the start or completion of a rollback.
--
-- 'occurredAt', 'deploymentEvent_occurredAt' - The date and time the event occurred.
--
-- 'triggeredBy', 'deploymentEvent_triggeredBy' - The entity that triggered the deployment event. Events can be triggered
-- by a user, AppConfig, an Amazon CloudWatch alarm, or an internal error.
newDeploymentEvent ::
  DeploymentEvent
newDeploymentEvent =
  DeploymentEvent'
    { actionInvocations =
        Prelude.Nothing,
      description = Prelude.Nothing,
      eventType = Prelude.Nothing,
      occurredAt = Prelude.Nothing,
      triggeredBy = Prelude.Nothing
    }

-- | The list of extensions that were invoked as part of the deployment.
deploymentEvent_actionInvocations :: Lens.Lens' DeploymentEvent (Prelude.Maybe [ActionInvocation])
deploymentEvent_actionInvocations = Lens.lens (\DeploymentEvent' {actionInvocations} -> actionInvocations) (\s@DeploymentEvent' {} a -> s {actionInvocations = a} :: DeploymentEvent) Prelude.. Lens.mapping Lens.coerced

-- | A description of the deployment event. Descriptions include, but are not
-- limited to, the user account or the Amazon CloudWatch alarm ARN that
-- initiated a rollback, the percentage of hosts that received the
-- deployment, or in the case of an internal error, a recommendation to
-- attempt a new deployment.
deploymentEvent_description :: Lens.Lens' DeploymentEvent (Prelude.Maybe Prelude.Text)
deploymentEvent_description = Lens.lens (\DeploymentEvent' {description} -> description) (\s@DeploymentEvent' {} a -> s {description = a} :: DeploymentEvent)

-- | The type of deployment event. Deployment event types include the start,
-- stop, or completion of a deployment; a percentage update; the start or
-- stop of a bake period; and the start or completion of a rollback.
deploymentEvent_eventType :: Lens.Lens' DeploymentEvent (Prelude.Maybe DeploymentEventType)
deploymentEvent_eventType = Lens.lens (\DeploymentEvent' {eventType} -> eventType) (\s@DeploymentEvent' {} a -> s {eventType = a} :: DeploymentEvent)

-- | The date and time the event occurred.
deploymentEvent_occurredAt :: Lens.Lens' DeploymentEvent (Prelude.Maybe Prelude.UTCTime)
deploymentEvent_occurredAt = Lens.lens (\DeploymentEvent' {occurredAt} -> occurredAt) (\s@DeploymentEvent' {} a -> s {occurredAt = a} :: DeploymentEvent) Prelude.. Lens.mapping Data._Time

-- | The entity that triggered the deployment event. Events can be triggered
-- by a user, AppConfig, an Amazon CloudWatch alarm, or an internal error.
deploymentEvent_triggeredBy :: Lens.Lens' DeploymentEvent (Prelude.Maybe TriggeredBy)
deploymentEvent_triggeredBy = Lens.lens (\DeploymentEvent' {triggeredBy} -> triggeredBy) (\s@DeploymentEvent' {} a -> s {triggeredBy = a} :: DeploymentEvent)

instance Data.FromJSON DeploymentEvent where
  parseJSON =
    Data.withObject
      "DeploymentEvent"
      ( \x ->
          DeploymentEvent'
            Prelude.<$> ( x Data..:? "ActionInvocations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EventType")
            Prelude.<*> (x Data..:? "OccurredAt")
            Prelude.<*> (x Data..:? "TriggeredBy")
      )

instance Prelude.Hashable DeploymentEvent where
  hashWithSalt _salt DeploymentEvent' {..} =
    _salt `Prelude.hashWithSalt` actionInvocations
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` occurredAt
      `Prelude.hashWithSalt` triggeredBy

instance Prelude.NFData DeploymentEvent where
  rnf DeploymentEvent' {..} =
    Prelude.rnf actionInvocations
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf occurredAt
      `Prelude.seq` Prelude.rnf triggeredBy
