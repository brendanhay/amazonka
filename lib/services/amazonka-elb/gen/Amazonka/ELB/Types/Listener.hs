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
-- Module      : Amazonka.ELB.Types.Listener
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.Listener where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about a listener.
--
-- For information about the protocols and the ports supported by Elastic
-- Load Balancing, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
--
-- /See:/ 'newListener' smart constructor.
data Listener = Listener'
  { -- | The Amazon Resource Name (ARN) of the server certificate.
    sSLCertificateId :: Prelude.Maybe Prelude.Text,
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
    instanceProtocol :: Prelude.Maybe Prelude.Text,
    -- | The load balancer transport protocol to use for routing: HTTP, HTTPS,
    -- TCP, or SSL.
    protocol :: Prelude.Text,
    -- | The port on which the load balancer is listening. On EC2-VPC, you can
    -- specify any port from the range 1-65535. On EC2-Classic, you can specify
    -- any port from the following list: 25, 80, 443, 465, 587, 1024-65535.
    loadBalancerPort :: Prelude.Int,
    -- | The port on which the instance is listening.
    instancePort :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Listener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sSLCertificateId', 'listener_sSLCertificateId' - The Amazon Resource Name (ARN) of the server certificate.
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
  Prelude.Text ->
  -- | 'loadBalancerPort'
  Prelude.Int ->
  -- | 'instancePort'
  Prelude.Natural ->
  Listener
newListener
  pProtocol_
  pLoadBalancerPort_
  pInstancePort_ =
    Listener'
      { sSLCertificateId = Prelude.Nothing,
        instanceProtocol = Prelude.Nothing,
        protocol = pProtocol_,
        loadBalancerPort = pLoadBalancerPort_,
        instancePort = pInstancePort_
      }

-- | The Amazon Resource Name (ARN) of the server certificate.
listener_sSLCertificateId :: Lens.Lens' Listener (Prelude.Maybe Prelude.Text)
listener_sSLCertificateId = Lens.lens (\Listener' {sSLCertificateId} -> sSLCertificateId) (\s@Listener' {} a -> s {sSLCertificateId = a} :: Listener)

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
listener_instanceProtocol :: Lens.Lens' Listener (Prelude.Maybe Prelude.Text)
listener_instanceProtocol = Lens.lens (\Listener' {instanceProtocol} -> instanceProtocol) (\s@Listener' {} a -> s {instanceProtocol = a} :: Listener)

-- | The load balancer transport protocol to use for routing: HTTP, HTTPS,
-- TCP, or SSL.
listener_protocol :: Lens.Lens' Listener Prelude.Text
listener_protocol = Lens.lens (\Listener' {protocol} -> protocol) (\s@Listener' {} a -> s {protocol = a} :: Listener)

-- | The port on which the load balancer is listening. On EC2-VPC, you can
-- specify any port from the range 1-65535. On EC2-Classic, you can specify
-- any port from the following list: 25, 80, 443, 465, 587, 1024-65535.
listener_loadBalancerPort :: Lens.Lens' Listener Prelude.Int
listener_loadBalancerPort = Lens.lens (\Listener' {loadBalancerPort} -> loadBalancerPort) (\s@Listener' {} a -> s {loadBalancerPort = a} :: Listener)

-- | The port on which the instance is listening.
listener_instancePort :: Lens.Lens' Listener Prelude.Natural
listener_instancePort = Lens.lens (\Listener' {instancePort} -> instancePort) (\s@Listener' {} a -> s {instancePort = a} :: Listener)

instance Data.FromXML Listener where
  parseXML x =
    Listener'
      Prelude.<$> (x Data..@? "SSLCertificateId")
      Prelude.<*> (x Data..@? "InstanceProtocol")
      Prelude.<*> (x Data..@ "Protocol")
      Prelude.<*> (x Data..@ "LoadBalancerPort")
      Prelude.<*> (x Data..@ "InstancePort")

instance Prelude.Hashable Listener where
  hashWithSalt _salt Listener' {..} =
    _salt `Prelude.hashWithSalt` sSLCertificateId
      `Prelude.hashWithSalt` instanceProtocol
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` loadBalancerPort
      `Prelude.hashWithSalt` instancePort

instance Prelude.NFData Listener where
  rnf Listener' {..} =
    Prelude.rnf sSLCertificateId
      `Prelude.seq` Prelude.rnf instanceProtocol
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf loadBalancerPort
      `Prelude.seq` Prelude.rnf instancePort

instance Data.ToQuery Listener where
  toQuery Listener' {..} =
    Prelude.mconcat
      [ "SSLCertificateId" Data.=: sSLCertificateId,
        "InstanceProtocol" Data.=: instanceProtocol,
        "Protocol" Data.=: protocol,
        "LoadBalancerPort" Data.=: loadBalancerPort,
        "InstancePort" Data.=: instancePort
      ]
