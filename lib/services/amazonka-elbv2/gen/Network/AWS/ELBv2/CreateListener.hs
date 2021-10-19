{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.CreateListener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a listener for the specified Application Load Balancer, Network
-- Load Balancer, or Gateway Load Balancer.
--
-- For more information, see the following:
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html Listeners for your Application Load Balancers>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-listeners.html Listeners for your Network Load Balancers>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/gateway-listeners.html Listeners for your Gateway Load Balancers>
--
-- This operation is idempotent, which means that it completes at most one
-- time. If you attempt to create multiple listeners with the same
-- settings, each call succeeds.
module Network.AWS.ELBv2.CreateListener
  ( -- * Creating a Request
    CreateListener (..),
    newCreateListener,

    -- * Request Lenses
    createListener_sslPolicy,
    createListener_protocol,
    createListener_certificates,
    createListener_alpnPolicy,
    createListener_tags,
    createListener_port,
    createListener_loadBalancerArn,
    createListener_defaultActions,

    -- * Destructuring the Response
    CreateListenerResponse (..),
    newCreateListenerResponse,

    -- * Response Lenses
    createListenerResponse_listeners,
    createListenerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateListener' smart constructor.
data CreateListener = CreateListener'
  { -- | [HTTPS and TLS listeners] The security policy that defines which
    -- protocols and ciphers are supported.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies>
    -- in the /Application Load Balancers Guide/ and
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies>
    -- in the /Network Load Balancers Guide/.
    sslPolicy :: Prelude.Maybe Prelude.Text,
    -- | The protocol for connections from clients to the load balancer. For
    -- Application Load Balancers, the supported protocols are HTTP and HTTPS.
    -- For Network Load Balancers, the supported protocols are TCP, TLS, UDP,
    -- and TCP_UDP. You can’t specify the UDP or TCP_UDP protocol if dual-stack
    -- mode is enabled. You cannot specify a protocol for a Gateway Load
    -- Balancer.
    protocol :: Prelude.Maybe ProtocolEnum,
    -- | [HTTPS and TLS listeners] The default certificate for the listener. You
    -- must provide exactly one certificate. Set @CertificateArn@ to the
    -- certificate ARN but do not set @IsDefault@.
    certificates :: Prelude.Maybe [Certificate],
    -- | [TLS listeners] The name of the Application-Layer Protocol Negotiation
    -- (ALPN) policy. You can specify one policy name. The following are the
    -- possible values:
    --
    -- -   @HTTP1Only@
    --
    -- -   @HTTP2Only@
    --
    -- -   @HTTP2Optional@
    --
    -- -   @HTTP2Preferred@
    --
    -- -   @None@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#alpn-policies ALPN policies>
    -- in the /Network Load Balancers Guide/.
    alpnPolicy :: Prelude.Maybe [Prelude.Text],
    -- | The tags to assign to the listener.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The port on which the load balancer is listening. You cannot specify a
    -- port for a Gateway Load Balancer.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Text,
    -- | The actions for the default rule.
    defaultActions :: [Action]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sslPolicy', 'createListener_sslPolicy' - [HTTPS and TLS listeners] The security policy that defines which
-- protocols and ciphers are supported.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies>
-- in the /Application Load Balancers Guide/ and
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies>
-- in the /Network Load Balancers Guide/.
--
-- 'protocol', 'createListener_protocol' - The protocol for connections from clients to the load balancer. For
-- Application Load Balancers, the supported protocols are HTTP and HTTPS.
-- For Network Load Balancers, the supported protocols are TCP, TLS, UDP,
-- and TCP_UDP. You can’t specify the UDP or TCP_UDP protocol if dual-stack
-- mode is enabled. You cannot specify a protocol for a Gateway Load
-- Balancer.
--
-- 'certificates', 'createListener_certificates' - [HTTPS and TLS listeners] The default certificate for the listener. You
-- must provide exactly one certificate. Set @CertificateArn@ to the
-- certificate ARN but do not set @IsDefault@.
--
-- 'alpnPolicy', 'createListener_alpnPolicy' - [TLS listeners] The name of the Application-Layer Protocol Negotiation
-- (ALPN) policy. You can specify one policy name. The following are the
-- possible values:
--
-- -   @HTTP1Only@
--
-- -   @HTTP2Only@
--
-- -   @HTTP2Optional@
--
-- -   @HTTP2Preferred@
--
-- -   @None@
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#alpn-policies ALPN policies>
-- in the /Network Load Balancers Guide/.
--
-- 'tags', 'createListener_tags' - The tags to assign to the listener.
--
-- 'port', 'createListener_port' - The port on which the load balancer is listening. You cannot specify a
-- port for a Gateway Load Balancer.
--
-- 'loadBalancerArn', 'createListener_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'defaultActions', 'createListener_defaultActions' - The actions for the default rule.
newCreateListener ::
  -- | 'loadBalancerArn'
  Prelude.Text ->
  CreateListener
newCreateListener pLoadBalancerArn_ =
  CreateListener'
    { sslPolicy = Prelude.Nothing,
      protocol = Prelude.Nothing,
      certificates = Prelude.Nothing,
      alpnPolicy = Prelude.Nothing,
      tags = Prelude.Nothing,
      port = Prelude.Nothing,
      loadBalancerArn = pLoadBalancerArn_,
      defaultActions = Prelude.mempty
    }

-- | [HTTPS and TLS listeners] The security policy that defines which
-- protocols and ciphers are supported.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies>
-- in the /Application Load Balancers Guide/ and
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies>
-- in the /Network Load Balancers Guide/.
createListener_sslPolicy :: Lens.Lens' CreateListener (Prelude.Maybe Prelude.Text)
createListener_sslPolicy = Lens.lens (\CreateListener' {sslPolicy} -> sslPolicy) (\s@CreateListener' {} a -> s {sslPolicy = a} :: CreateListener)

-- | The protocol for connections from clients to the load balancer. For
-- Application Load Balancers, the supported protocols are HTTP and HTTPS.
-- For Network Load Balancers, the supported protocols are TCP, TLS, UDP,
-- and TCP_UDP. You can’t specify the UDP or TCP_UDP protocol if dual-stack
-- mode is enabled. You cannot specify a protocol for a Gateway Load
-- Balancer.
createListener_protocol :: Lens.Lens' CreateListener (Prelude.Maybe ProtocolEnum)
createListener_protocol = Lens.lens (\CreateListener' {protocol} -> protocol) (\s@CreateListener' {} a -> s {protocol = a} :: CreateListener)

-- | [HTTPS and TLS listeners] The default certificate for the listener. You
-- must provide exactly one certificate. Set @CertificateArn@ to the
-- certificate ARN but do not set @IsDefault@.
createListener_certificates :: Lens.Lens' CreateListener (Prelude.Maybe [Certificate])
createListener_certificates = Lens.lens (\CreateListener' {certificates} -> certificates) (\s@CreateListener' {} a -> s {certificates = a} :: CreateListener) Prelude.. Lens.mapping Lens.coerced

-- | [TLS listeners] The name of the Application-Layer Protocol Negotiation
-- (ALPN) policy. You can specify one policy name. The following are the
-- possible values:
--
-- -   @HTTP1Only@
--
-- -   @HTTP2Only@
--
-- -   @HTTP2Optional@
--
-- -   @HTTP2Preferred@
--
-- -   @None@
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#alpn-policies ALPN policies>
-- in the /Network Load Balancers Guide/.
createListener_alpnPolicy :: Lens.Lens' CreateListener (Prelude.Maybe [Prelude.Text])
createListener_alpnPolicy = Lens.lens (\CreateListener' {alpnPolicy} -> alpnPolicy) (\s@CreateListener' {} a -> s {alpnPolicy = a} :: CreateListener) Prelude.. Lens.mapping Lens.coerced

-- | The tags to assign to the listener.
createListener_tags :: Lens.Lens' CreateListener (Prelude.Maybe (Prelude.NonEmpty Tag))
createListener_tags = Lens.lens (\CreateListener' {tags} -> tags) (\s@CreateListener' {} a -> s {tags = a} :: CreateListener) Prelude.. Lens.mapping Lens.coerced

-- | The port on which the load balancer is listening. You cannot specify a
-- port for a Gateway Load Balancer.
createListener_port :: Lens.Lens' CreateListener (Prelude.Maybe Prelude.Natural)
createListener_port = Lens.lens (\CreateListener' {port} -> port) (\s@CreateListener' {} a -> s {port = a} :: CreateListener)

-- | The Amazon Resource Name (ARN) of the load balancer.
createListener_loadBalancerArn :: Lens.Lens' CreateListener Prelude.Text
createListener_loadBalancerArn = Lens.lens (\CreateListener' {loadBalancerArn} -> loadBalancerArn) (\s@CreateListener' {} a -> s {loadBalancerArn = a} :: CreateListener)

-- | The actions for the default rule.
createListener_defaultActions :: Lens.Lens' CreateListener [Action]
createListener_defaultActions = Lens.lens (\CreateListener' {defaultActions} -> defaultActions) (\s@CreateListener' {} a -> s {defaultActions = a} :: CreateListener) Prelude.. Lens.coerced

instance Core.AWSRequest CreateListener where
  type
    AWSResponse CreateListener =
      CreateListenerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateListenerResult"
      ( \s h x ->
          CreateListenerResponse'
            Prelude.<$> ( x Core..@? "Listeners" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateListener

instance Prelude.NFData CreateListener

instance Core.ToHeaders CreateListener where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateListener where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateListener where
  toQuery CreateListener' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateListener" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "SslPolicy" Core.=: sslPolicy,
        "Protocol" Core.=: protocol,
        "Certificates"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> certificates),
        "AlpnPolicy"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> alpnPolicy),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "Port" Core.=: port,
        "LoadBalancerArn" Core.=: loadBalancerArn,
        "DefaultActions"
          Core.=: Core.toQueryList "member" defaultActions
      ]

-- | /See:/ 'newCreateListenerResponse' smart constructor.
data CreateListenerResponse = CreateListenerResponse'
  { -- | Information about the listener.
    listeners :: Prelude.Maybe [Listener],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listeners', 'createListenerResponse_listeners' - Information about the listener.
--
-- 'httpStatus', 'createListenerResponse_httpStatus' - The response's http status code.
newCreateListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateListenerResponse
newCreateListenerResponse pHttpStatus_ =
  CreateListenerResponse'
    { listeners =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the listener.
createListenerResponse_listeners :: Lens.Lens' CreateListenerResponse (Prelude.Maybe [Listener])
createListenerResponse_listeners = Lens.lens (\CreateListenerResponse' {listeners} -> listeners) (\s@CreateListenerResponse' {} a -> s {listeners = a} :: CreateListenerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createListenerResponse_httpStatus :: Lens.Lens' CreateListenerResponse Prelude.Int
createListenerResponse_httpStatus = Lens.lens (\CreateListenerResponse' {httpStatus} -> httpStatus) (\s@CreateListenerResponse' {} a -> s {httpStatus = a} :: CreateListenerResponse)

instance Prelude.NFData CreateListenerResponse
