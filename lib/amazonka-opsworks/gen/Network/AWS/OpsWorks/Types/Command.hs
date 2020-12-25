{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Command
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Command
  ( Command (..),

    -- * Smart constructor
    mkCommand,

    -- * Lenses
    cAcknowledgedAt,
    cCommandId,
    cCompletedAt,
    cCreatedAt,
    cDeploymentId,
    cExitCode,
    cInstanceId,
    cLogUrl,
    cStatus,
    cType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.AcknowledgedAt as Types
import qualified Network.AWS.OpsWorks.Types.CommandId as Types
import qualified Network.AWS.OpsWorks.Types.CompletedAt as Types
import qualified Network.AWS.OpsWorks.Types.CreatedAt as Types
import qualified Network.AWS.OpsWorks.Types.DeploymentId as Types
import qualified Network.AWS.OpsWorks.Types.InstanceId as Types
import qualified Network.AWS.OpsWorks.Types.LogUrl as Types
import qualified Network.AWS.OpsWorks.Types.Status as Types
import qualified Network.AWS.OpsWorks.Types.Type as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a command.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { -- | Date and time when the command was acknowledged.
    acknowledgedAt :: Core.Maybe Types.AcknowledgedAt,
    -- | The command ID.
    commandId :: Core.Maybe Types.CommandId,
    -- | Date when the command completed.
    completedAt :: Core.Maybe Types.CompletedAt,
    -- | Date and time when the command was run.
    createdAt :: Core.Maybe Types.CreatedAt,
    -- | The command deployment ID.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | The command exit code.
    exitCode :: Core.Maybe Core.Int,
    -- | The ID of the instance where the command was executed.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The URL of the command log.
    logUrl :: Core.Maybe Types.LogUrl,
    -- | The command status:
    --
    --
    --     * failed
    --
    --
    --     * successful
    --
    --
    --     * skipped
    --
    --
    --     * pending
    status :: Core.Maybe Types.Status,
    -- | The command type:
    --
    --
    --     * @configure@
    --
    --
    --     * @deploy@
    --
    --
    --     * @execute_recipes@
    --
    --
    --     * @install_dependencies@
    --
    --
    --     * @restart@
    --
    --
    --     * @rollback@
    --
    --
    --     * @setup@
    --
    --
    --     * @start@
    --
    --
    --     * @stop@
    --
    --
    --     * @undeploy@
    --
    --
    --     * @update_custom_cookbooks@
    --
    --
    --     * @update_dependencies@
    type' :: Core.Maybe Types.Type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Command' value with any optional fields omitted.
mkCommand ::
  Command
mkCommand =
  Command'
    { acknowledgedAt = Core.Nothing,
      commandId = Core.Nothing,
      completedAt = Core.Nothing,
      createdAt = Core.Nothing,
      deploymentId = Core.Nothing,
      exitCode = Core.Nothing,
      instanceId = Core.Nothing,
      logUrl = Core.Nothing,
      status = Core.Nothing,
      type' = Core.Nothing
    }

-- | Date and time when the command was acknowledged.
--
-- /Note:/ Consider using 'acknowledgedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAcknowledgedAt :: Lens.Lens' Command (Core.Maybe Types.AcknowledgedAt)
cAcknowledgedAt = Lens.field @"acknowledgedAt"
{-# DEPRECATED cAcknowledgedAt "Use generic-lens or generic-optics with 'acknowledgedAt' instead." #-}

-- | The command ID.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommandId :: Lens.Lens' Command (Core.Maybe Types.CommandId)
cCommandId = Lens.field @"commandId"
{-# DEPRECATED cCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | Date when the command completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCompletedAt :: Lens.Lens' Command (Core.Maybe Types.CompletedAt)
cCompletedAt = Lens.field @"completedAt"
{-# DEPRECATED cCompletedAt "Use generic-lens or generic-optics with 'completedAt' instead." #-}

-- | Date and time when the command was run.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreatedAt :: Lens.Lens' Command (Core.Maybe Types.CreatedAt)
cCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED cCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The command deployment ID.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeploymentId :: Lens.Lens' Command (Core.Maybe Types.DeploymentId)
cDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED cDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The command exit code.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExitCode :: Lens.Lens' Command (Core.Maybe Core.Int)
cExitCode = Lens.field @"exitCode"
{-# DEPRECATED cExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

-- | The ID of the instance where the command was executed.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInstanceId :: Lens.Lens' Command (Core.Maybe Types.InstanceId)
cInstanceId = Lens.field @"instanceId"
{-# DEPRECATED cInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The URL of the command log.
--
-- /Note:/ Consider using 'logUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogUrl :: Lens.Lens' Command (Core.Maybe Types.LogUrl)
cLogUrl = Lens.field @"logUrl"
{-# DEPRECATED cLogUrl "Use generic-lens or generic-optics with 'logUrl' instead." #-}

-- | The command status:
--
--
--     * failed
--
--
--     * successful
--
--
--     * skipped
--
--
--     * pending
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Command (Core.Maybe Types.Status)
cStatus = Lens.field @"status"
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The command type:
--
--
--     * @configure@
--
--
--     * @deploy@
--
--
--     * @execute_recipes@
--
--
--     * @install_dependencies@
--
--
--     * @restart@
--
--
--     * @rollback@
--
--
--     * @setup@
--
--
--     * @start@
--
--
--     * @stop@
--
--
--     * @undeploy@
--
--
--     * @update_custom_cookbooks@
--
--
--     * @update_dependencies@
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Command (Core.Maybe Types.Type)
cType = Lens.field @"type'"
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Command where
  parseJSON =
    Core.withObject "Command" Core.$
      \x ->
        Command'
          Core.<$> (x Core..:? "AcknowledgedAt")
          Core.<*> (x Core..:? "CommandId")
          Core.<*> (x Core..:? "CompletedAt")
          Core.<*> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "DeploymentId")
          Core.<*> (x Core..:? "ExitCode")
          Core.<*> (x Core..:? "InstanceId")
          Core.<*> (x Core..:? "LogUrl")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Type")
