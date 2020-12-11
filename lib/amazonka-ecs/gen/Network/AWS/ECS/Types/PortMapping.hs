-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PortMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PortMapping
  ( PortMapping (..),

    -- * Smart constructor
    mkPortMapping,

    -- * Lenses
    pmProtocol,
    pmHostPort,
    pmContainerPort,
  )
where

import Network.AWS.ECS.Types.TransportProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition.
--
-- If you are using containers in a task with the @awsvpc@ or @host@ network mode, exposed ports should be specified using @containerPort@ . The @hostPort@ can be left blank or it must be the same value as the @containerPort@ .
-- After a task reaches the @RUNNING@ status, manual and automatic host and container port assignments are visible in the @networkBindings@ section of 'DescribeTasks' API responses.
--
-- /See:/ 'mkPortMapping' smart constructor.
data PortMapping = PortMapping'
  { protocol ::
      Lude.Maybe TransportProtocol,
    hostPort :: Lude.Maybe Lude.Int,
    containerPort :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PortMapping' with the minimum fields required to make a request.
--
-- * 'containerPort' - The port number on the container that is bound to the user-specified or automatically assigned host port.
--
-- If you are using containers in a task with the @awsvpc@ or @host@ network mode, exposed ports should be specified using @containerPort@ .
-- If you are using containers in a task with the @bridge@ network mode and you specify a container port and not a host port, your container automatically receives a host port in the ephemeral port range. For more information, see @hostPort@ . Port mappings that are automatically assigned in this way do not count toward the 100 reserved ports limit of a container instance.
-- * 'hostPort' - The port number on the container instance to reserve for your container.
--
-- If you are using containers in a task with the @awsvpc@ or @host@ network mode, the @hostPort@ can either be left blank or set to the same value as the @containerPort@ .
-- If you are using containers in a task with the @bridge@ network mode, you can specify a non-reserved host port for your container port mapping, or you can omit the @hostPort@ (or set it to @0@ ) while specifying a @containerPort@ and your container automatically receives a port in the ephemeral port range for your container instance operating system and Docker version.
-- The default ephemeral port range for Docker version 1.6.0 and later is listed on the instance under @/proc/sys/net/ipv4/ip_local_port_range@ . If this kernel parameter is unavailable, the default ephemeral port range from 49153 through 65535 is used. Do not attempt to specify a host port in the ephemeral port range as these are reserved for automatic assignment. In general, ports below 32768 are outside of the ephemeral port range.
-- The default reserved ports are 22 for SSH, the Docker ports 2375 and 2376, and the Amazon ECS container agent ports 51678-51680. Any host port that was previously specified in a running task is also reserved while the task is running (after a task stops, the host port is released). The current reserved ports are displayed in the @remainingResources@ of 'DescribeContainerInstances' output. A container instance can have up to 100 reserved ports at a time, including the default reserved ports. Automatically assigned ports don't count toward the 100 reserved ports limit.
-- * 'protocol' - The protocol used for the port mapping. Valid values are @tcp@ and @udp@ . The default is @tcp@ .
mkPortMapping ::
  PortMapping
mkPortMapping =
  PortMapping'
    { protocol = Lude.Nothing,
      hostPort = Lude.Nothing,
      containerPort = Lude.Nothing
    }

-- | The protocol used for the port mapping. Valid values are @tcp@ and @udp@ . The default is @tcp@ .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmProtocol :: Lens.Lens' PortMapping (Lude.Maybe TransportProtocol)
pmProtocol = Lens.lens (protocol :: PortMapping -> Lude.Maybe TransportProtocol) (\s a -> s {protocol = a} :: PortMapping)
{-# DEPRECATED pmProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The port number on the container instance to reserve for your container.
--
-- If you are using containers in a task with the @awsvpc@ or @host@ network mode, the @hostPort@ can either be left blank or set to the same value as the @containerPort@ .
-- If you are using containers in a task with the @bridge@ network mode, you can specify a non-reserved host port for your container port mapping, or you can omit the @hostPort@ (or set it to @0@ ) while specifying a @containerPort@ and your container automatically receives a port in the ephemeral port range for your container instance operating system and Docker version.
-- The default ephemeral port range for Docker version 1.6.0 and later is listed on the instance under @/proc/sys/net/ipv4/ip_local_port_range@ . If this kernel parameter is unavailable, the default ephemeral port range from 49153 through 65535 is used. Do not attempt to specify a host port in the ephemeral port range as these are reserved for automatic assignment. In general, ports below 32768 are outside of the ephemeral port range.
-- The default reserved ports are 22 for SSH, the Docker ports 2375 and 2376, and the Amazon ECS container agent ports 51678-51680. Any host port that was previously specified in a running task is also reserved while the task is running (after a task stops, the host port is released). The current reserved ports are displayed in the @remainingResources@ of 'DescribeContainerInstances' output. A container instance can have up to 100 reserved ports at a time, including the default reserved ports. Automatically assigned ports don't count toward the 100 reserved ports limit.
--
-- /Note:/ Consider using 'hostPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmHostPort :: Lens.Lens' PortMapping (Lude.Maybe Lude.Int)
pmHostPort = Lens.lens (hostPort :: PortMapping -> Lude.Maybe Lude.Int) (\s a -> s {hostPort = a} :: PortMapping)
{-# DEPRECATED pmHostPort "Use generic-lens or generic-optics with 'hostPort' instead." #-}

-- | The port number on the container that is bound to the user-specified or automatically assigned host port.
--
-- If you are using containers in a task with the @awsvpc@ or @host@ network mode, exposed ports should be specified using @containerPort@ .
-- If you are using containers in a task with the @bridge@ network mode and you specify a container port and not a host port, your container automatically receives a host port in the ephemeral port range. For more information, see @hostPort@ . Port mappings that are automatically assigned in this way do not count toward the 100 reserved ports limit of a container instance.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmContainerPort :: Lens.Lens' PortMapping (Lude.Maybe Lude.Int)
pmContainerPort = Lens.lens (containerPort :: PortMapping -> Lude.Maybe Lude.Int) (\s a -> s {containerPort = a} :: PortMapping)
{-# DEPRECATED pmContainerPort "Use generic-lens or generic-optics with 'containerPort' instead." #-}

instance Lude.FromJSON PortMapping where
  parseJSON =
    Lude.withObject
      "PortMapping"
      ( \x ->
          PortMapping'
            Lude.<$> (x Lude..:? "protocol")
            Lude.<*> (x Lude..:? "hostPort")
            Lude.<*> (x Lude..:? "containerPort")
      )

instance Lude.ToJSON PortMapping where
  toJSON PortMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("protocol" Lude..=) Lude.<$> protocol,
            ("hostPort" Lude..=) Lude.<$> hostPort,
            ("containerPort" Lude..=) Lude.<$> containerPort
          ]
      )
