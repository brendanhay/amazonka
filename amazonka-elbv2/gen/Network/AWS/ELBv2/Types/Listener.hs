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
-- Module      : Network.AWS.ELBv2.Types.Listener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Listener where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.Certificate
import Network.AWS.ELBv2.Types.ProtocolEnum
import qualified Network.AWS.Lens as Lens

-- | Information about a listener.
--
-- /See:/ 'newListener' smart constructor.
data Listener = Listener'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Maybe Core.Text,
    -- | [HTTPS or TLS listener] The security policy that defines which protocols
    -- and ciphers are supported.
    sslPolicy :: Core.Maybe Core.Text,
    -- | The port on which the load balancer is listening.
    port :: Core.Maybe Core.Natural,
    -- | The default actions for the listener.
    defaultActions :: Core.Maybe [Action],
    -- | The protocol for connections from clients to the load balancer.
    protocol :: Core.Maybe ProtocolEnum,
    -- | [HTTPS or TLS listener] The default certificate for the listener.
    certificates :: Core.Maybe [Certificate],
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Core.Maybe Core.Text,
    -- | [TLS listener] The name of the Application-Layer Protocol Negotiation
    -- (ALPN) policy.
    alpnPolicy :: Core.Maybe [Core.Text]
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
-- 'loadBalancerArn', 'listener_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'sslPolicy', 'listener_sslPolicy' - [HTTPS or TLS listener] The security policy that defines which protocols
-- and ciphers are supported.
--
-- 'port', 'listener_port' - The port on which the load balancer is listening.
--
-- 'defaultActions', 'listener_defaultActions' - The default actions for the listener.
--
-- 'protocol', 'listener_protocol' - The protocol for connections from clients to the load balancer.
--
-- 'certificates', 'listener_certificates' - [HTTPS or TLS listener] The default certificate for the listener.
--
-- 'listenerArn', 'listener_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'alpnPolicy', 'listener_alpnPolicy' - [TLS listener] The name of the Application-Layer Protocol Negotiation
-- (ALPN) policy.
newListener ::
  Listener
newListener =
  Listener'
    { loadBalancerArn = Core.Nothing,
      sslPolicy = Core.Nothing,
      port = Core.Nothing,
      defaultActions = Core.Nothing,
      protocol = Core.Nothing,
      certificates = Core.Nothing,
      listenerArn = Core.Nothing,
      alpnPolicy = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
listener_loadBalancerArn :: Lens.Lens' Listener (Core.Maybe Core.Text)
listener_loadBalancerArn = Lens.lens (\Listener' {loadBalancerArn} -> loadBalancerArn) (\s@Listener' {} a -> s {loadBalancerArn = a} :: Listener)

-- | [HTTPS or TLS listener] The security policy that defines which protocols
-- and ciphers are supported.
listener_sslPolicy :: Lens.Lens' Listener (Core.Maybe Core.Text)
listener_sslPolicy = Lens.lens (\Listener' {sslPolicy} -> sslPolicy) (\s@Listener' {} a -> s {sslPolicy = a} :: Listener)

-- | The port on which the load balancer is listening.
listener_port :: Lens.Lens' Listener (Core.Maybe Core.Natural)
listener_port = Lens.lens (\Listener' {port} -> port) (\s@Listener' {} a -> s {port = a} :: Listener)

-- | The default actions for the listener.
listener_defaultActions :: Lens.Lens' Listener (Core.Maybe [Action])
listener_defaultActions = Lens.lens (\Listener' {defaultActions} -> defaultActions) (\s@Listener' {} a -> s {defaultActions = a} :: Listener) Core.. Lens.mapping Lens._Coerce

-- | The protocol for connections from clients to the load balancer.
listener_protocol :: Lens.Lens' Listener (Core.Maybe ProtocolEnum)
listener_protocol = Lens.lens (\Listener' {protocol} -> protocol) (\s@Listener' {} a -> s {protocol = a} :: Listener)

-- | [HTTPS or TLS listener] The default certificate for the listener.
listener_certificates :: Lens.Lens' Listener (Core.Maybe [Certificate])
listener_certificates = Lens.lens (\Listener' {certificates} -> certificates) (\s@Listener' {} a -> s {certificates = a} :: Listener) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the listener.
listener_listenerArn :: Lens.Lens' Listener (Core.Maybe Core.Text)
listener_listenerArn = Lens.lens (\Listener' {listenerArn} -> listenerArn) (\s@Listener' {} a -> s {listenerArn = a} :: Listener)

-- | [TLS listener] The name of the Application-Layer Protocol Negotiation
-- (ALPN) policy.
listener_alpnPolicy :: Lens.Lens' Listener (Core.Maybe [Core.Text])
listener_alpnPolicy = Lens.lens (\Listener' {alpnPolicy} -> alpnPolicy) (\s@Listener' {} a -> s {alpnPolicy = a} :: Listener) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML Listener where
  parseXML x =
    Listener'
      Core.<$> (x Core..@? "LoadBalancerArn")
      Core.<*> (x Core..@? "SslPolicy")
      Core.<*> (x Core..@? "Port")
      Core.<*> ( x Core..@? "DefaultActions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Protocol")
      Core.<*> ( x Core..@? "Certificates" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "ListenerArn")
      Core.<*> ( x Core..@? "AlpnPolicy" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable Listener

instance Core.NFData Listener
