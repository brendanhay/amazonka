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
-- Module      : Network.AWS.CodeDeploy.Types.RollbackInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RollbackInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a deployment rollback.
--
-- /See:/ 'newRollbackInfo' smart constructor.
data RollbackInfo = RollbackInfo'
  { -- | Information that describes the status of a deployment rollback (for
    -- example, whether the deployment can\'t be rolled back, is in progress,
    -- failed, or succeeded).
    rollbackMessage :: Core.Maybe Core.Text,
    -- | The deployment ID of the deployment that was underway and triggered a
    -- rollback deployment because it failed or was stopped.
    rollbackTriggeringDeploymentId :: Core.Maybe Core.Text,
    -- | The ID of the deployment rollback.
    rollbackDeploymentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RollbackInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rollbackMessage', 'rollbackInfo_rollbackMessage' - Information that describes the status of a deployment rollback (for
-- example, whether the deployment can\'t be rolled back, is in progress,
-- failed, or succeeded).
--
-- 'rollbackTriggeringDeploymentId', 'rollbackInfo_rollbackTriggeringDeploymentId' - The deployment ID of the deployment that was underway and triggered a
-- rollback deployment because it failed or was stopped.
--
-- 'rollbackDeploymentId', 'rollbackInfo_rollbackDeploymentId' - The ID of the deployment rollback.
newRollbackInfo ::
  RollbackInfo
newRollbackInfo =
  RollbackInfo'
    { rollbackMessage = Core.Nothing,
      rollbackTriggeringDeploymentId = Core.Nothing,
      rollbackDeploymentId = Core.Nothing
    }

-- | Information that describes the status of a deployment rollback (for
-- example, whether the deployment can\'t be rolled back, is in progress,
-- failed, or succeeded).
rollbackInfo_rollbackMessage :: Lens.Lens' RollbackInfo (Core.Maybe Core.Text)
rollbackInfo_rollbackMessage = Lens.lens (\RollbackInfo' {rollbackMessage} -> rollbackMessage) (\s@RollbackInfo' {} a -> s {rollbackMessage = a} :: RollbackInfo)

-- | The deployment ID of the deployment that was underway and triggered a
-- rollback deployment because it failed or was stopped.
rollbackInfo_rollbackTriggeringDeploymentId :: Lens.Lens' RollbackInfo (Core.Maybe Core.Text)
rollbackInfo_rollbackTriggeringDeploymentId = Lens.lens (\RollbackInfo' {rollbackTriggeringDeploymentId} -> rollbackTriggeringDeploymentId) (\s@RollbackInfo' {} a -> s {rollbackTriggeringDeploymentId = a} :: RollbackInfo)

-- | The ID of the deployment rollback.
rollbackInfo_rollbackDeploymentId :: Lens.Lens' RollbackInfo (Core.Maybe Core.Text)
rollbackInfo_rollbackDeploymentId = Lens.lens (\RollbackInfo' {rollbackDeploymentId} -> rollbackDeploymentId) (\s@RollbackInfo' {} a -> s {rollbackDeploymentId = a} :: RollbackInfo)

instance Core.FromJSON RollbackInfo where
  parseJSON =
    Core.withObject
      "RollbackInfo"
      ( \x ->
          RollbackInfo'
            Core.<$> (x Core..:? "rollbackMessage")
            Core.<*> (x Core..:? "rollbackTriggeringDeploymentId")
            Core.<*> (x Core..:? "rollbackDeploymentId")
      )

instance Core.Hashable RollbackInfo

instance Core.NFData RollbackInfo
