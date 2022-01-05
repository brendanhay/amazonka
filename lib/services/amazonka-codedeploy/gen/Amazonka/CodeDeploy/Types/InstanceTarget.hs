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
-- Module      : Amazonka.CodeDeploy.Types.InstanceTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.InstanceTarget where

import Amazonka.CodeDeploy.Types.LifecycleEvent
import Amazonka.CodeDeploy.Types.TargetLabel
import Amazonka.CodeDeploy.Types.TargetStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A target Amazon EC2 or on-premises instance during a deployment that
-- uses the EC2\/On-premises compute platform.
--
-- /See:/ 'newInstanceTarget' smart constructor.
data InstanceTarget = InstanceTarget'
  { -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of a deployment target that has a type of
    -- @instanceTarget@.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The status an EC2\/On-premises deployment\'s target instance.
    status :: Prelude.Maybe TargetStatus,
    -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | A label that identifies whether the instance is an original target
    -- (@BLUE@) or a replacement target (@GREEN@).
    instanceLabel :: Prelude.Maybe TargetLabel,
    -- | The date and time when the target instance was updated by a deployment.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The lifecycle events of the deployment to this target instance.
    lifecycleEvents :: Prelude.Maybe [LifecycleEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetArn', 'instanceTarget_targetArn' - The Amazon Resource Name (ARN) of the target.
--
-- 'targetId', 'instanceTarget_targetId' - The unique ID of a deployment target that has a type of
-- @instanceTarget@.
--
-- 'status', 'instanceTarget_status' - The status an EC2\/On-premises deployment\'s target instance.
--
-- 'deploymentId', 'instanceTarget_deploymentId' - The unique ID of a deployment.
--
-- 'instanceLabel', 'instanceTarget_instanceLabel' - A label that identifies whether the instance is an original target
-- (@BLUE@) or a replacement target (@GREEN@).
--
-- 'lastUpdatedAt', 'instanceTarget_lastUpdatedAt' - The date and time when the target instance was updated by a deployment.
--
-- 'lifecycleEvents', 'instanceTarget_lifecycleEvents' - The lifecycle events of the deployment to this target instance.
newInstanceTarget ::
  InstanceTarget
newInstanceTarget =
  InstanceTarget'
    { targetArn = Prelude.Nothing,
      targetId = Prelude.Nothing,
      status = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      instanceLabel = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      lifecycleEvents = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target.
instanceTarget_targetArn :: Lens.Lens' InstanceTarget (Prelude.Maybe Prelude.Text)
instanceTarget_targetArn = Lens.lens (\InstanceTarget' {targetArn} -> targetArn) (\s@InstanceTarget' {} a -> s {targetArn = a} :: InstanceTarget)

-- | The unique ID of a deployment target that has a type of
-- @instanceTarget@.
instanceTarget_targetId :: Lens.Lens' InstanceTarget (Prelude.Maybe Prelude.Text)
instanceTarget_targetId = Lens.lens (\InstanceTarget' {targetId} -> targetId) (\s@InstanceTarget' {} a -> s {targetId = a} :: InstanceTarget)

-- | The status an EC2\/On-premises deployment\'s target instance.
instanceTarget_status :: Lens.Lens' InstanceTarget (Prelude.Maybe TargetStatus)
instanceTarget_status = Lens.lens (\InstanceTarget' {status} -> status) (\s@InstanceTarget' {} a -> s {status = a} :: InstanceTarget)

-- | The unique ID of a deployment.
instanceTarget_deploymentId :: Lens.Lens' InstanceTarget (Prelude.Maybe Prelude.Text)
instanceTarget_deploymentId = Lens.lens (\InstanceTarget' {deploymentId} -> deploymentId) (\s@InstanceTarget' {} a -> s {deploymentId = a} :: InstanceTarget)

-- | A label that identifies whether the instance is an original target
-- (@BLUE@) or a replacement target (@GREEN@).
instanceTarget_instanceLabel :: Lens.Lens' InstanceTarget (Prelude.Maybe TargetLabel)
instanceTarget_instanceLabel = Lens.lens (\InstanceTarget' {instanceLabel} -> instanceLabel) (\s@InstanceTarget' {} a -> s {instanceLabel = a} :: InstanceTarget)

-- | The date and time when the target instance was updated by a deployment.
instanceTarget_lastUpdatedAt :: Lens.Lens' InstanceTarget (Prelude.Maybe Prelude.UTCTime)
instanceTarget_lastUpdatedAt = Lens.lens (\InstanceTarget' {lastUpdatedAt} -> lastUpdatedAt) (\s@InstanceTarget' {} a -> s {lastUpdatedAt = a} :: InstanceTarget) Prelude.. Lens.mapping Core._Time

-- | The lifecycle events of the deployment to this target instance.
instanceTarget_lifecycleEvents :: Lens.Lens' InstanceTarget (Prelude.Maybe [LifecycleEvent])
instanceTarget_lifecycleEvents = Lens.lens (\InstanceTarget' {lifecycleEvents} -> lifecycleEvents) (\s@InstanceTarget' {} a -> s {lifecycleEvents = a} :: InstanceTarget) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON InstanceTarget where
  parseJSON =
    Core.withObject
      "InstanceTarget"
      ( \x ->
          InstanceTarget'
            Prelude.<$> (x Core..:? "targetArn")
            Prelude.<*> (x Core..:? "targetId")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "deploymentId")
            Prelude.<*> (x Core..:? "instanceLabel")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> ( x Core..:? "lifecycleEvents"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InstanceTarget where
  hashWithSalt _salt InstanceTarget' {..} =
    _salt `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` instanceLabel
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` lifecycleEvents

instance Prelude.NFData InstanceTarget where
  rnf InstanceTarget' {..} =
    Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf instanceLabel
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf lifecycleEvents
