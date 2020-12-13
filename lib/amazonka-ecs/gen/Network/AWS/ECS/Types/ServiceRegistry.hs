{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ServiceRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ServiceRegistry
  ( ServiceRegistry (..),

    -- * Smart constructor
    mkServiceRegistry,

    -- * Lenses
    srRegistryARN,
    srContainerName,
    srContainerPort,
    srPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of the service registry.
--
-- /See:/ 'mkServiceRegistry' smart constructor.
data ServiceRegistry = ServiceRegistry'
  { -- | The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is AWS Cloud Map. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService> .
    registryARN :: Lude.Maybe Lude.Text,
    -- | The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
    containerName :: Lude.Maybe Lude.Text,
    -- | The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
    containerPort :: Lude.Maybe Lude.Int,
    -- | The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceRegistry' with the minimum fields required to make a request.
--
-- * 'registryARN' - The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is AWS Cloud Map. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService> .
-- * 'containerName' - The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
-- * 'containerPort' - The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
-- * 'port' - The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
mkServiceRegistry ::
  ServiceRegistry
mkServiceRegistry =
  ServiceRegistry'
    { registryARN = Lude.Nothing,
      containerName = Lude.Nothing,
      containerPort = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is AWS Cloud Map. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService> .
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRegistryARN :: Lens.Lens' ServiceRegistry (Lude.Maybe Lude.Text)
srRegistryARN = Lens.lens (registryARN :: ServiceRegistry -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: ServiceRegistry)
{-# DEPRECATED srRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

-- | The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srContainerName :: Lens.Lens' ServiceRegistry (Lude.Maybe Lude.Text)
srContainerName = Lens.lens (containerName :: ServiceRegistry -> Lude.Maybe Lude.Text) (\s a -> s {containerName = a} :: ServiceRegistry)
{-# DEPRECATED srContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srContainerPort :: Lens.Lens' ServiceRegistry (Lude.Maybe Lude.Int)
srContainerPort = Lens.lens (containerPort :: ServiceRegistry -> Lude.Maybe Lude.Int) (\s a -> s {containerPort = a} :: ServiceRegistry)
{-# DEPRECATED srContainerPort "Use generic-lens or generic-optics with 'containerPort' instead." #-}

-- | The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srPort :: Lens.Lens' ServiceRegistry (Lude.Maybe Lude.Int)
srPort = Lens.lens (port :: ServiceRegistry -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: ServiceRegistry)
{-# DEPRECATED srPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON ServiceRegistry where
  parseJSON =
    Lude.withObject
      "ServiceRegistry"
      ( \x ->
          ServiceRegistry'
            Lude.<$> (x Lude..:? "registryArn")
            Lude.<*> (x Lude..:? "containerName")
            Lude.<*> (x Lude..:? "containerPort")
            Lude.<*> (x Lude..:? "port")
      )

instance Lude.ToJSON ServiceRegistry where
  toJSON ServiceRegistry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryArn" Lude..=) Lude.<$> registryARN,
            ("containerName" Lude..=) Lude.<$> containerName,
            ("containerPort" Lude..=) Lude.<$> containerPort,
            ("port" Lude..=) Lude.<$> port
          ]
      )
