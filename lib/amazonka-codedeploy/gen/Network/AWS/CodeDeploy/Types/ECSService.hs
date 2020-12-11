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
    ecssServiceName,
    ecssClusterName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the service and cluster names used to identify an Amazon ECS deployment's target.
--
-- /See:/ 'mkECSService' smart constructor.
data ECSService = ECSService'
  { serviceName :: Lude.Maybe Lude.Text,
    clusterName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ECSService' with the minimum fields required to make a request.
--
-- * 'clusterName' - The name of the cluster that the Amazon ECS service is associated with.
-- * 'serviceName' - The name of the target Amazon ECS service.
mkECSService ::
  ECSService
mkECSService =
  ECSService'
    { serviceName = Lude.Nothing,
      clusterName = Lude.Nothing
    }

-- | The name of the target Amazon ECS service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecssServiceName :: Lens.Lens' ECSService (Lude.Maybe Lude.Text)
ecssServiceName = Lens.lens (serviceName :: ECSService -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: ECSService)
{-# DEPRECATED ecssServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The name of the cluster that the Amazon ECS service is associated with.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecssClusterName :: Lens.Lens' ECSService (Lude.Maybe Lude.Text)
ecssClusterName = Lens.lens (clusterName :: ECSService -> Lude.Maybe Lude.Text) (\s a -> s {clusterName = a} :: ECSService)
{-# DEPRECATED ecssClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

instance Lude.FromJSON ECSService where
  parseJSON =
    Lude.withObject
      "ECSService"
      ( \x ->
          ECSService'
            Lude.<$> (x Lude..:? "serviceName") Lude.<*> (x Lude..:? "clusterName")
      )

instance Lude.ToJSON ECSService where
  toJSON ECSService' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serviceName" Lude..=) Lude.<$> serviceName,
            ("clusterName" Lude..=) Lude.<$> clusterName
          ]
      )
