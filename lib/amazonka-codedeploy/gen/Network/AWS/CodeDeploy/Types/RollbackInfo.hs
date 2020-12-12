{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RollbackInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RollbackInfo
  ( RollbackInfo (..),

    -- * Smart constructor
    mkRollbackInfo,

    -- * Lenses
    riRollbackTriggeringDeploymentId,
    riRollbackMessage,
    riRollbackDeploymentId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a deployment rollback.
--
-- /See:/ 'mkRollbackInfo' smart constructor.
data RollbackInfo = RollbackInfo'
  { rollbackTriggeringDeploymentId ::
      Lude.Maybe Lude.Text,
    rollbackMessage :: Lude.Maybe Lude.Text,
    rollbackDeploymentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RollbackInfo' with the minimum fields required to make a request.
--
-- * 'rollbackDeploymentId' - The ID of the deployment rollback.
-- * 'rollbackMessage' - Information that describes the status of a deployment rollback (for example, whether the deployment can't be rolled back, is in progress, failed, or succeeded).
-- * 'rollbackTriggeringDeploymentId' - The deployment ID of the deployment that was underway and triggered a rollback deployment because it failed or was stopped.
mkRollbackInfo ::
  RollbackInfo
mkRollbackInfo =
  RollbackInfo'
    { rollbackTriggeringDeploymentId = Lude.Nothing,
      rollbackMessage = Lude.Nothing,
      rollbackDeploymentId = Lude.Nothing
    }

-- | The deployment ID of the deployment that was underway and triggered a rollback deployment because it failed or was stopped.
--
-- /Note:/ Consider using 'rollbackTriggeringDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRollbackTriggeringDeploymentId :: Lens.Lens' RollbackInfo (Lude.Maybe Lude.Text)
riRollbackTriggeringDeploymentId = Lens.lens (rollbackTriggeringDeploymentId :: RollbackInfo -> Lude.Maybe Lude.Text) (\s a -> s {rollbackTriggeringDeploymentId = a} :: RollbackInfo)
{-# DEPRECATED riRollbackTriggeringDeploymentId "Use generic-lens or generic-optics with 'rollbackTriggeringDeploymentId' instead." #-}

-- | Information that describes the status of a deployment rollback (for example, whether the deployment can't be rolled back, is in progress, failed, or succeeded).
--
-- /Note:/ Consider using 'rollbackMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRollbackMessage :: Lens.Lens' RollbackInfo (Lude.Maybe Lude.Text)
riRollbackMessage = Lens.lens (rollbackMessage :: RollbackInfo -> Lude.Maybe Lude.Text) (\s a -> s {rollbackMessage = a} :: RollbackInfo)
{-# DEPRECATED riRollbackMessage "Use generic-lens or generic-optics with 'rollbackMessage' instead." #-}

-- | The ID of the deployment rollback.
--
-- /Note:/ Consider using 'rollbackDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRollbackDeploymentId :: Lens.Lens' RollbackInfo (Lude.Maybe Lude.Text)
riRollbackDeploymentId = Lens.lens (rollbackDeploymentId :: RollbackInfo -> Lude.Maybe Lude.Text) (\s a -> s {rollbackDeploymentId = a} :: RollbackInfo)
{-# DEPRECATED riRollbackDeploymentId "Use generic-lens or generic-optics with 'rollbackDeploymentId' instead." #-}

instance Lude.FromJSON RollbackInfo where
  parseJSON =
    Lude.withObject
      "RollbackInfo"
      ( \x ->
          RollbackInfo'
            Lude.<$> (x Lude..:? "rollbackTriggeringDeploymentId")
            Lude.<*> (x Lude..:? "rollbackMessage")
            Lude.<*> (x Lude..:? "rollbackDeploymentId")
      )
