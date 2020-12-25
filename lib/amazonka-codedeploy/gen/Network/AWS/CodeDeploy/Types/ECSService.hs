{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ECSService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ECSService
  ( ECSService (..),

    -- * Smart constructor
    mkECSService,

    -- * Lenses
    ecssClusterName,
    ecssServiceName,
  )
where

import qualified Network.AWS.CodeDeploy.Types.ECSClusterName as Types
import qualified Network.AWS.CodeDeploy.Types.ECSServiceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the service and cluster names used to identify an Amazon ECS deployment's target.
--
-- /See:/ 'mkECSService' smart constructor.
data ECSService = ECSService'
  { -- | The name of the cluster that the Amazon ECS service is associated with.
    clusterName :: Core.Maybe Types.ECSClusterName,
    -- | The name of the target Amazon ECS service.
    serviceName :: Core.Maybe Types.ECSServiceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ECSService' value with any optional fields omitted.
mkECSService ::
  ECSService
mkECSService =
  ECSService'
    { clusterName = Core.Nothing,
      serviceName = Core.Nothing
    }

-- | The name of the cluster that the Amazon ECS service is associated with.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecssClusterName :: Lens.Lens' ECSService (Core.Maybe Types.ECSClusterName)
ecssClusterName = Lens.field @"clusterName"
{-# DEPRECATED ecssClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The name of the target Amazon ECS service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecssServiceName :: Lens.Lens' ECSService (Core.Maybe Types.ECSServiceName)
ecssServiceName = Lens.field @"serviceName"
{-# DEPRECATED ecssServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Core.FromJSON ECSService where
  toJSON ECSService {..} =
    Core.object
      ( Core.catMaybes
          [ ("clusterName" Core..=) Core.<$> clusterName,
            ("serviceName" Core..=) Core.<$> serviceName
          ]
      )

instance Core.FromJSON ECSService where
  parseJSON =
    Core.withObject "ECSService" Core.$
      \x ->
        ECSService'
          Core.<$> (x Core..:? "clusterName") Core.<*> (x Core..:? "serviceName")
