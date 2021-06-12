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
-- Module      : Network.AWS.ELB.Types.Listener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Listener where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about a listener.
--
-- For information about the protocols and the ports supported by Elastic
-- Load Balancing, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
--
-- /See:/ 'newListener' smart constructor.
data Listener = Listener'
  { -- | The protocol to use for routing traffic to instances: HTTP, HTTPS, TCP,
    -- or SSL.
    --
    -- If the front-end protocol is TCP or SSL, the back-end protocol must be
    -- TCP or SSL. If the front-end protocol is HTTP or HTTPS, the back-end
    -- protocol must be HTTP or HTTPS.
    --
    -- If there is another listener with the same @InstancePort@ whose
    -- @InstanceProtocol@ is secure, (HTTPS or SSL), the listener\'s
    -- @InstanceProtocol@ must also be secure.
    --
    -- If there is another listener with the same @InstancePort@ whose
    -- @InstanceProtocol@ is HTTP or TCP, the listener\'s @InstanceProtocol@
    -- must be HTTP or TCP.
    instanceProtocol :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the server certificate.
    sSLCertificateId :: Core.Maybe Core.Text,
    -- | The load balancer transport protocol to use for routing: HTTP, HTTPS,
    -- TCP, or SSL.
    protocol :: Core.Text,
    -- | The port on which the load balancer is listening. On EC2-VPC, you can
    -- specify any port from the range 1-65535. On EC2-Classic, you can specify
    -- any port from the following list: 25, 80, 443, 465, 587, 1024-65535.
    loadBalancerPort :: Core.Int,
    -- | The port on which the instance is listening.
    instancePort :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Listener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProtocol', 'listener_instanceProtocol' - The protocol to use for routing traffic to instances: HTTP, HTTPS, TCP,
-- or SSL.
--
-- If the front-end protocol is TCP or SSL, the back-end protocol must be
-- TCP or SSL. If the front-end protocol is HTTP or HTTPS, the back-end
-- protocol must be HTTP or HTTPS.
--
-- If there is another listener with the same @InstancePort@ whose
-- @InstanceProtocol@ is secure, (HTTPS or SSL), the listener\'s
-- @InstanceProtocol@ must also be secure.
--
-- If there is another listener with the same @InstancePort@ whose
-- @InstanceProtocol@ is HTTP or TCP, the listener\'s @InstanceProtocol@
-- must be HTTP or TCP.
--
-- 'sSLCertificateId', 'listener_sSLCertificateId' - The Amazon Resource Name (ARN) of the server certificate.
--
-- 'protocol', 'listener_protocol' - The load balancer transport protocol to use for routing: HTTP, HTTPS,
-- TCP, or SSL.
--
-- 'loadBalancerPort', 'listener_loadBalancerPort' - The port on which the load balancer is listening. On EC2-VPC, you can
-- specify any port from the range 1-65535. On EC2-Classic, you can specify
-- any port from the following list: 25, 80, 443, 465, 587, 1024-65535.
--
-- 'instancePort', 'listener_instancePort' - The port on which the instance is listening.
newListener ::
  -- | 'protocol'
  Core.Text ->
  -- | 'loadBalancerPort'
  Core.Int ->
  -- | 'instancePort'
  Core.Natural ->
  Listener
newListener
  pProtocol_
  pLoadBalancerPort_
  pInstancePort_ =
    Listener'
      { instanceProtocol = Core.Nothing,
        sSLCertificateId = Core.Nothing,
        protocol = pProtocol_,
        loadBalancerPort = pLoadBalancerPort_,
        instancePort = pInstancePort_
      }

-- | The protocol to use for routing traffic to instances: HTTP, HTTPS, TCP,
-- or SSL.
--
-- If the front-end protocol is TCP or SSL, the back-end protocol must be
-- TCP or SSL. If the front-end protocol is HTTP or HTTPS, the back-end
-- protocol must be HTTP or HTTPS.
--
-- If there is another listener with the same @InstancePort@ whose
-- @InstanceProtocol@ is secure, (HTTPS or SSL), the listener\'s
-- @InstanceProtocol@ must also be secure.
--
-- If there is another listener with the same @InstancePort@ whose
-- @InstanceProtocol@ is HTTP or TCP, the listener\'s @InstanceProtocol@
-- must be HTTP or TCP.
listener_instanceProtocol :: Lens.Lens' Listener (Core.Maybe Core.Text)
listener_instanceProtocol = Lens.lens (\Listener' {instanceProtocol} -> instanceProtocol) (\s@Listener' {} a -> s {instanceProtocol = a} :: Listener)

-- | The Amazon Resource Name (ARN) of the server certificate.
listener_sSLCertificateId :: Lens.Lens' Listener (Core.Maybe Core.Text)
listener_sSLCertificateId = Lens.lens (\Listener' {sSLCertificateId} -> sSLCertificateId) (\s@Listener' {} a -> s {sSLCertificateId = a} :: Listener)

-- | The load balancer transport protocol to use for routing: HTTP, HTTPS,
-- TCP, or SSL.
listener_protocol :: Lens.Lens' Listener Core.Text
listener_protocol = Lens.lens (\Listener' {protocol} -> protocol) (\s@Listener' {} a -> s {protocol = a} :: Listener)

-- | The port on which the load balancer is listening. On EC2-VPC, you can
-- specify any port from the range 1-65535. On EC2-Classic, you can specify
-- any port from the following list: 25, 80, 443, 465, 587, 1024-65535.
listener_loadBalancerPort :: Lens.Lens' Listener Core.Int
listener_loadBalancerPort = Lens.lens (\Listener' {loadBalancerPort} -> loadBalancerPort) (\s@Listener' {} a -> s {loadBalancerPort = a} :: Listener)

-- | The port on which the instance is listening.
listener_instancePort :: Lens.Lens' Listener Core.Natural
listener_instancePort = Lens.lens (\Listener' {instancePort} -> instancePort) (\s@Listener' {} a -> s {instancePort = a} :: Listener)

instance Core.FromXML Listener where
  parseXML x =
    Listener'
      Core.<$> (x Core..@? "InstanceProtocol")
      Core.<*> (x Core..@? "SSLCertificateId")
      Core.<*> (x Core..@ "Protocol")
      Core.<*> (x Core..@ "LoadBalancerPort")
      Core.<*> (x Core..@ "InstancePort")

instance Core.Hashable Listener

instance Core.NFData Listener

instance Core.ToQuery Listener where
  toQuery Listener' {..} =
    Core.mconcat
      [ "InstanceProtocol" Core.=: instanceProtocol,
        "SSLCertificateId" Core.=: sSLCertificateId,
        "Protocol" Core.=: protocol,
        "LoadBalancerPort" Core.=: loadBalancerPort,
        "InstancePort" Core.=: instancePort
      ]
