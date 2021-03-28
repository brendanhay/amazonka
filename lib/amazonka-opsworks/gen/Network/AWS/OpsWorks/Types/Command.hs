{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Command
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Command
  ( Command (..)
  -- * Smart constructor
  , mkCommand
  -- * Lenses
  , cAcknowledgedAt
  , cCommandId
  , cCompletedAt
  , cCreatedAt
  , cDeploymentId
  , cExitCode
  , cInstanceId
  , cLogUrl
  , cStatus
  , cType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.AcknowledgedAt as Types
import qualified Network.AWS.OpsWorks.Types.CompletedAt as Types
import qualified Network.AWS.OpsWorks.Types.CreatedAt as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a command.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { acknowledgedAt :: Core.Maybe Types.AcknowledgedAt
    -- ^ Date and time when the command was acknowledged.
  , commandId :: Core.Maybe Core.Text
    -- ^ The command ID.
  , completedAt :: Core.Maybe Types.CompletedAt
    -- ^ Date when the command completed.
  , createdAt :: Core.Maybe Types.CreatedAt
    -- ^ Date and time when the command was run.
  , deploymentId :: Core.Maybe Core.Text
    -- ^ The command deployment ID.
  , exitCode :: Core.Maybe Core.Int
    -- ^ The command exit code.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance where the command was executed.
  , logUrl :: Core.Maybe Core.Text
    -- ^ The URL of the command log.
  , status :: Core.Maybe Core.Text
    -- ^ The command status:
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
  , type' :: Core.Maybe Core.Text
    -- ^ The command type:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Command' value with any optional fields omitted.
mkCommand
    :: Command
mkCommand
  = Command'{acknowledgedAt = Core.Nothing, commandId = Core.Nothing,
             completedAt = Core.Nothing, createdAt = Core.Nothing,
             deploymentId = Core.Nothing, exitCode = Core.Nothing,
             instanceId = Core.Nothing, logUrl = Core.Nothing,
             status = Core.Nothing, type' = Core.Nothing}

-- | Date and time when the command was acknowledged.
--
-- /Note:/ Consider using 'acknowledgedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAcknowledgedAt :: Lens.Lens' Command (Core.Maybe Types.AcknowledgedAt)
cAcknowledgedAt = Lens.field @"acknowledgedAt"
{-# INLINEABLE cAcknowledgedAt #-}
{-# DEPRECATED acknowledgedAt "Use generic-lens or generic-optics with 'acknowledgedAt' instead"  #-}

-- | The command ID.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommandId :: Lens.Lens' Command (Core.Maybe Core.Text)
cCommandId = Lens.field @"commandId"
{-# INLINEABLE cCommandId #-}
{-# DEPRECATED commandId "Use generic-lens or generic-optics with 'commandId' instead"  #-}

-- | Date when the command completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCompletedAt :: Lens.Lens' Command (Core.Maybe Types.CompletedAt)
cCompletedAt = Lens.field @"completedAt"
{-# INLINEABLE cCompletedAt #-}
{-# DEPRECATED completedAt "Use generic-lens or generic-optics with 'completedAt' instead"  #-}

-- | Date and time when the command was run.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreatedAt :: Lens.Lens' Command (Core.Maybe Types.CreatedAt)
cCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE cCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The command deployment ID.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeploymentId :: Lens.Lens' Command (Core.Maybe Core.Text)
cDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE cDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The command exit code.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExitCode :: Lens.Lens' Command (Core.Maybe Core.Int)
cExitCode = Lens.field @"exitCode"
{-# INLINEABLE cExitCode #-}
{-# DEPRECATED exitCode "Use generic-lens or generic-optics with 'exitCode' instead"  #-}

-- | The ID of the instance where the command was executed.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInstanceId :: Lens.Lens' Command (Core.Maybe Core.Text)
cInstanceId = Lens.field @"instanceId"
{-# INLINEABLE cInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The URL of the command log.
--
-- /Note:/ Consider using 'logUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogUrl :: Lens.Lens' Command (Core.Maybe Core.Text)
cLogUrl = Lens.field @"logUrl"
{-# INLINEABLE cLogUrl #-}
{-# DEPRECATED logUrl "Use generic-lens or generic-optics with 'logUrl' instead"  #-}

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
cStatus :: Lens.Lens' Command (Core.Maybe Core.Text)
cStatus = Lens.field @"status"
{-# INLINEABLE cStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

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
cType :: Lens.Lens' Command (Core.Maybe Core.Text)
cType = Lens.field @"type'"
{-# INLINEABLE cType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Command where
        parseJSON
          = Core.withObject "Command" Core.$
              \ x ->
                Command' Core.<$>
                  (x Core..:? "AcknowledgedAt") Core.<*> x Core..:? "CommandId"
                    Core.<*> x Core..:? "CompletedAt"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "DeploymentId"
                    Core.<*> x Core..:? "ExitCode"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "LogUrl"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "Type"
