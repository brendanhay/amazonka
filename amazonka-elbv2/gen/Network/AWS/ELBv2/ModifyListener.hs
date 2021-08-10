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
-- Module      : Network.AWS.ELBv2.ModifyListener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the specified properties of the specified listener. Any
-- properties that you do not specify remain unchanged.
--
-- Changing the protocol from HTTPS to HTTP, or from TLS to TCP, removes
-- the security policy and default certificate properties. If you change
-- the protocol from HTTP to HTTPS, or from TCP to TLS, you must add the
-- security policy and default certificate properties.
--
-- To add an item to a list, remove an item from a list, or update an item
-- in a list, you must provide the entire list. For example, to add an
-- action, specify a list with the current actions plus the new action.
module Network.AWS.ELBv2.ModifyListener
  ( -- * Creating a Request
    ModifyListener (..),
    newModifyListener,

    -- * Request Lenses
    modifyListener_sslPolicy,
    modifyListener_port,
    modifyListener_defaultActions,
    modifyListener_protocol,
    modifyListener_certificates,
    modifyListener_alpnPolicy,
    modifyListener_listenerArn,

    -- * Destructuring the Response
    ModifyListenerResponse (..),
    newModifyListenerResponse,

    -- * Response Lenses
    modifyListenerResponse_listeners,
    modifyListenerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyListener' smart constructor.
data ModifyListener = ModifyListener'
  { -- | [HTTPS and TLS listeners] The security policy that defines which
    -- protocols and ciphers are supported.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies>
    -- in the /Application Load Balancers Guide/ or
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies>
    -- in the /Network Load Balancers Guide/.
    sslPolicy :: Prelude.Maybe Prelude.Text,
    -- | The port for connections from clients to the load balancer. You cannot
    -- specify a port for a Gateway Load Balancer.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The actions for the default rule.
    defaultActions :: Prelude.Maybe [Action],
    -- | The protocol for connections from clients to the load balancer.
    -- Application Load Balancers support the HTTP and HTTPS protocols. Network
    -- Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You
    -- can’t change the protocol to UDP or TCP_UDP if dual-stack mode is
    -- enabled. You cannot specify a protocol for a Gateway Load Balancer.
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
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sslPolicy', 'modifyListener_sslPolicy' - [HTTPS and TLS listeners] The security policy that defines which
-- protocols and ciphers are supported.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies>
-- in the /Application Load Balancers Guide/ or
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies>
-- in the /Network Load Balancers Guide/.
--
-- 'port', 'modifyListener_port' - The port for connections from clients to the load balancer. You cannot
-- specify a port for a Gateway Load Balancer.
--
-- 'defaultActions', 'modifyListener_defaultActions' - The actions for the default rule.
--
-- 'protocol', 'modifyListener_protocol' - The protocol for connections from clients to the load balancer.
-- Application Load Balancers support the HTTP and HTTPS protocols. Network
-- Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You
-- can’t change the protocol to UDP or TCP_UDP if dual-stack mode is
-- enabled. You cannot specify a protocol for a Gateway Load Balancer.
--
-- 'certificates', 'modifyListener_certificates' - [HTTPS and TLS listeners] The default certificate for the listener. You
-- must provide exactly one certificate. Set @CertificateArn@ to the
-- certificate ARN but do not set @IsDefault@.
--
-- 'alpnPolicy', 'modifyListener_alpnPolicy' - [TLS listeners] The name of the Application-Layer Protocol Negotiation
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
-- 'listenerArn', 'modifyListener_listenerArn' - The Amazon Resource Name (ARN) of the listener.
newModifyListener ::
  -- | 'listenerArn'
  Prelude.Text ->
  ModifyListener
newModifyListener pListenerArn_ =
  ModifyListener'
    { sslPolicy = Prelude.Nothing,
      port = Prelude.Nothing,
      defaultActions = Prelude.Nothing,
      protocol = Prelude.Nothing,
      certificates = Prelude.Nothing,
      alpnPolicy = Prelude.Nothing,
      listenerArn = pListenerArn_
    }

-- | [HTTPS and TLS listeners] The security policy that defines which
-- protocols and ciphers are supported.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies>
-- in the /Application Load Balancers Guide/ or
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies>
-- in the /Network Load Balancers Guide/.
modifyListener_sslPolicy :: Lens.Lens' ModifyListener (Prelude.Maybe Prelude.Text)
modifyListener_sslPolicy = Lens.lens (\ModifyListener' {sslPolicy} -> sslPolicy) (\s@ModifyListener' {} a -> s {sslPolicy = a} :: ModifyListener)

-- | The port for connections from clients to the load balancer. You cannot
-- specify a port for a Gateway Load Balancer.
modifyListener_port :: Lens.Lens' ModifyListener (Prelude.Maybe Prelude.Natural)
modifyListener_port = Lens.lens (\ModifyListener' {port} -> port) (\s@ModifyListener' {} a -> s {port = a} :: ModifyListener)

-- | The actions for the default rule.
modifyListener_defaultActions :: Lens.Lens' ModifyListener (Prelude.Maybe [Action])
modifyListener_defaultActions = Lens.lens (\ModifyListener' {defaultActions} -> defaultActions) (\s@ModifyListener' {} a -> s {defaultActions = a} :: ModifyListener) Prelude.. Lens.mapping Lens._Coerce

-- | The protocol for connections from clients to the load balancer.
-- Application Load Balancers support the HTTP and HTTPS protocols. Network
-- Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You
-- can’t change the protocol to UDP or TCP_UDP if dual-stack mode is
-- enabled. You cannot specify a protocol for a Gateway Load Balancer.
modifyListener_protocol :: Lens.Lens' ModifyListener (Prelude.Maybe ProtocolEnum)
modifyListener_protocol = Lens.lens (\ModifyListener' {protocol} -> protocol) (\s@ModifyListener' {} a -> s {protocol = a} :: ModifyListener)

-- | [HTTPS and TLS listeners] The default certificate for the listener. You
-- must provide exactly one certificate. Set @CertificateArn@ to the
-- certificate ARN but do not set @IsDefault@.
modifyListener_certificates :: Lens.Lens' ModifyListener (Prelude.Maybe [Certificate])
modifyListener_certificates = Lens.lens (\ModifyListener' {certificates} -> certificates) (\s@ModifyListener' {} a -> s {certificates = a} :: ModifyListener) Prelude.. Lens.mapping Lens._Coerce

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
modifyListener_alpnPolicy :: Lens.Lens' ModifyListener (Prelude.Maybe [Prelude.Text])
modifyListener_alpnPolicy = Lens.lens (\ModifyListener' {alpnPolicy} -> alpnPolicy) (\s@ModifyListener' {} a -> s {alpnPolicy = a} :: ModifyListener) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the listener.
modifyListener_listenerArn :: Lens.Lens' ModifyListener Prelude.Text
modifyListener_listenerArn = Lens.lens (\ModifyListener' {listenerArn} -> listenerArn) (\s@ModifyListener' {} a -> s {listenerArn = a} :: ModifyListener)

instance Core.AWSRequest ModifyListener where
  type
    AWSResponse ModifyListener =
      ModifyListenerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyListenerResult"
      ( \s h x ->
          ModifyListenerResponse'
            Prelude.<$> ( x Core..@? "Listeners" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyListener

instance Prelude.NFData ModifyListener

instance Core.ToHeaders ModifyListener where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyListener where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyListener where
  toQuery ModifyListener' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyListener" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "SslPolicy" Core.=: sslPolicy,
        "Port" Core.=: port,
        "DefaultActions"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> defaultActions
            ),
        "Protocol" Core.=: protocol,
        "Certificates"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> certificates),
        "AlpnPolicy"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> alpnPolicy),
        "ListenerArn" Core.=: listenerArn
      ]

-- | /See:/ 'newModifyListenerResponse' smart constructor.
data ModifyListenerResponse = ModifyListenerResponse'
  { -- | Information about the modified listener.
    listeners :: Prelude.Maybe [Listener],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listeners', 'modifyListenerResponse_listeners' - Information about the modified listener.
--
-- 'httpStatus', 'modifyListenerResponse_httpStatus' - The response's http status code.
newModifyListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyListenerResponse
newModifyListenerResponse pHttpStatus_ =
  ModifyListenerResponse'
    { listeners =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the modified listener.
modifyListenerResponse_listeners :: Lens.Lens' ModifyListenerResponse (Prelude.Maybe [Listener])
modifyListenerResponse_listeners = Lens.lens (\ModifyListenerResponse' {listeners} -> listeners) (\s@ModifyListenerResponse' {} a -> s {listeners = a} :: ModifyListenerResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
modifyListenerResponse_httpStatus :: Lens.Lens' ModifyListenerResponse Prelude.Int
modifyListenerResponse_httpStatus = Lens.lens (\ModifyListenerResponse' {httpStatus} -> httpStatus) (\s@ModifyListenerResponse' {} a -> s {httpStatus = a} :: ModifyListenerResponse)

instance Prelude.NFData ModifyListenerResponse
