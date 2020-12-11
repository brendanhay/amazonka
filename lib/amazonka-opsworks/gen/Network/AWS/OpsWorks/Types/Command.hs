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
    cDeploymentId,
    cInstanceId,
    cStatus,
    cLogURL,
    cCreatedAt,
    cCommandId,
    cExitCode,
    cType,
    cCompletedAt,
    cAcknowledgedAt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a command.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { deploymentId :: Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    logURL :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text,
    commandId :: Lude.Maybe Lude.Text,
    exitCode :: Lude.Maybe Lude.Int,
    type' :: Lude.Maybe Lude.Text,
    completedAt :: Lude.Maybe Lude.Text,
    acknowledgedAt :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- * 'acknowledgedAt' - Date and time when the command was acknowledged.
-- * 'commandId' - The command ID.
-- * 'completedAt' - Date when the command completed.
-- * 'createdAt' - Date and time when the command was run.
-- * 'deploymentId' - The command deployment ID.
-- * 'exitCode' - The command exit code.
-- * 'instanceId' - The ID of the instance where the command was executed.
-- * 'logURL' - The URL of the command log.
-- * 'status' - The command status:
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
-- * 'type'' - The command type:
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
mkCommand ::
  Command
mkCommand =
  Command'
    { deploymentId = Lude.Nothing,
      instanceId = Lude.Nothing,
      status = Lude.Nothing,
      logURL = Lude.Nothing,
      createdAt = Lude.Nothing,
      commandId = Lude.Nothing,
      exitCode = Lude.Nothing,
      type' = Lude.Nothing,
      completedAt = Lude.Nothing,
      acknowledgedAt = Lude.Nothing
    }

-- | The command deployment ID.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeploymentId :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cDeploymentId = Lens.lens (deploymentId :: Command -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: Command)
{-# DEPRECATED cDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The ID of the instance where the command was executed.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInstanceId :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cInstanceId = Lens.lens (instanceId :: Command -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Command)
{-# DEPRECATED cInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

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
cStatus :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cStatus = Lens.lens (status :: Command -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Command)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The URL of the command log.
--
-- /Note:/ Consider using 'logURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogURL :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cLogURL = Lens.lens (logURL :: Command -> Lude.Maybe Lude.Text) (\s a -> s {logURL = a} :: Command)
{-# DEPRECATED cLogURL "Use generic-lens or generic-optics with 'logURL' instead." #-}

-- | Date and time when the command was run.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreatedAt :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cCreatedAt = Lens.lens (createdAt :: Command -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: Command)
{-# DEPRECATED cCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The command ID.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommandId :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cCommandId = Lens.lens (commandId :: Command -> Lude.Maybe Lude.Text) (\s a -> s {commandId = a} :: Command)
{-# DEPRECATED cCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | The command exit code.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExitCode :: Lens.Lens' Command (Lude.Maybe Lude.Int)
cExitCode = Lens.lens (exitCode :: Command -> Lude.Maybe Lude.Int) (\s a -> s {exitCode = a} :: Command)
{-# DEPRECATED cExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

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
cType :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cType = Lens.lens (type' :: Command -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Command)
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Date when the command completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCompletedAt :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cCompletedAt = Lens.lens (completedAt :: Command -> Lude.Maybe Lude.Text) (\s a -> s {completedAt = a} :: Command)
{-# DEPRECATED cCompletedAt "Use generic-lens or generic-optics with 'completedAt' instead." #-}

-- | Date and time when the command was acknowledged.
--
-- /Note:/ Consider using 'acknowledgedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAcknowledgedAt :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cAcknowledgedAt = Lens.lens (acknowledgedAt :: Command -> Lude.Maybe Lude.Text) (\s a -> s {acknowledgedAt = a} :: Command)
{-# DEPRECATED cAcknowledgedAt "Use generic-lens or generic-optics with 'acknowledgedAt' instead." #-}

instance Lude.FromJSON Command where
  parseJSON =
    Lude.withObject
      "Command"
      ( \x ->
          Command'
            Lude.<$> (x Lude..:? "DeploymentId")
            Lude.<*> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LogUrl")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "CommandId")
            Lude.<*> (x Lude..:? "ExitCode")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "CompletedAt")
            Lude.<*> (x Lude..:? "AcknowledgedAt")
      )
