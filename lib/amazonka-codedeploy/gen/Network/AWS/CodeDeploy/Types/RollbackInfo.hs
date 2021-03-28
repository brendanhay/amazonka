{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RollbackInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.RollbackInfo
  ( RollbackInfo (..)
  -- * Smart constructor
  , mkRollbackInfo
  -- * Lenses
  , riRollbackDeploymentId
  , riRollbackMessage
  , riRollbackTriggeringDeploymentId
  ) where

import qualified Network.AWS.CodeDeploy.Types.DeploymentId as Types
import qualified Network.AWS.CodeDeploy.Types.RollbackMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a deployment rollback.
--
-- /See:/ 'mkRollbackInfo' smart constructor.
data RollbackInfo = RollbackInfo'
  { rollbackDeploymentId :: Core.Maybe Types.DeploymentId
    -- ^ The ID of the deployment rollback.
  , rollbackMessage :: Core.Maybe Types.RollbackMessage
    -- ^ Information that describes the status of a deployment rollback (for example, whether the deployment can't be rolled back, is in progress, failed, or succeeded). 
  , rollbackTriggeringDeploymentId :: Core.Maybe Types.DeploymentId
    -- ^ The deployment ID of the deployment that was underway and triggered a rollback deployment because it failed or was stopped.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RollbackInfo' value with any optional fields omitted.
mkRollbackInfo
    :: RollbackInfo
mkRollbackInfo
  = RollbackInfo'{rollbackDeploymentId = Core.Nothing,
                  rollbackMessage = Core.Nothing,
                  rollbackTriggeringDeploymentId = Core.Nothing}

-- | The ID of the deployment rollback.
--
-- /Note:/ Consider using 'rollbackDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRollbackDeploymentId :: Lens.Lens' RollbackInfo (Core.Maybe Types.DeploymentId)
riRollbackDeploymentId = Lens.field @"rollbackDeploymentId"
{-# INLINEABLE riRollbackDeploymentId #-}
{-# DEPRECATED rollbackDeploymentId "Use generic-lens or generic-optics with 'rollbackDeploymentId' instead"  #-}

-- | Information that describes the status of a deployment rollback (for example, whether the deployment can't be rolled back, is in progress, failed, or succeeded). 
--
-- /Note:/ Consider using 'rollbackMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRollbackMessage :: Lens.Lens' RollbackInfo (Core.Maybe Types.RollbackMessage)
riRollbackMessage = Lens.field @"rollbackMessage"
{-# INLINEABLE riRollbackMessage #-}
{-# DEPRECATED rollbackMessage "Use generic-lens or generic-optics with 'rollbackMessage' instead"  #-}

-- | The deployment ID of the deployment that was underway and triggered a rollback deployment because it failed or was stopped.
--
-- /Note:/ Consider using 'rollbackTriggeringDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRollbackTriggeringDeploymentId :: Lens.Lens' RollbackInfo (Core.Maybe Types.DeploymentId)
riRollbackTriggeringDeploymentId = Lens.field @"rollbackTriggeringDeploymentId"
{-# INLINEABLE riRollbackTriggeringDeploymentId #-}
{-# DEPRECATED rollbackTriggeringDeploymentId "Use generic-lens or generic-optics with 'rollbackTriggeringDeploymentId' instead"  #-}

instance Core.FromJSON RollbackInfo where
        parseJSON
          = Core.withObject "RollbackInfo" Core.$
              \ x ->
                RollbackInfo' Core.<$>
                  (x Core..:? "rollbackDeploymentId") Core.<*>
                    x Core..:? "rollbackMessage"
                    Core.<*> x Core..:? "rollbackTriggeringDeploymentId"
