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
-- Module      : Amazonka.GlobalAccelerator.CreateListener
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a listener to process inbound connections from clients to an
-- accelerator. Connections arrive to assigned static IP addresses on a
-- port, port range, or list of port ranges that you specify.
module Amazonka.GlobalAccelerator.CreateListener
  ( -- * Creating a Request
    CreateListener (..),
    newCreateListener,

    -- * Request Lenses
    createListener_clientAffinity,
    createListener_acceleratorArn,
    createListener_portRanges,
    createListener_protocol,
    createListener_idempotencyToken,

    -- * Destructuring the Response
    CreateListenerResponse (..),
    newCreateListenerResponse,

    -- * Response Lenses
    createListenerResponse_listener,
    createListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateListener' smart constructor.
data CreateListener = CreateListener'
  { -- | Client affinity lets you direct all requests from a user to the same
    -- endpoint, if you have stateful applications, regardless of the port and
    -- protocol of the client request. Client affinity gives you control over
    -- whether to always route each client to the same specific endpoint.
    --
    -- Global Accelerator uses a consistent-flow hashing algorithm to choose
    -- the optimal endpoint for a connection. If client affinity is @NONE@,
    -- Global Accelerator uses the \"five-tuple\" (5-tuple) properties—source
    -- IP address, source port, destination IP address, destination port, and
    -- protocol—to select the hash value, and then chooses the best endpoint.
    -- However, with this setting, if someone uses different ports to connect
    -- to Global Accelerator, their connections might not be always routed to
    -- the same endpoint because the hash value changes.
    --
    -- If you want a given client to always be routed to the same endpoint, set
    -- client affinity to @SOURCE_IP@ instead. When you use the @SOURCE_IP@
    -- setting, Global Accelerator uses the \"two-tuple\" (2-tuple) properties—
    -- source (client) IP address and destination IP address—to select the hash
    -- value.
    --
    -- The default value is @NONE@.
    clientAffinity :: Prelude.Maybe ClientAffinity,
    -- | The Amazon Resource Name (ARN) of your accelerator.
    acceleratorArn :: Prelude.Text,
    -- | The list of port ranges to support for connections from clients to your
    -- accelerator.
    portRanges :: Prelude.NonEmpty PortRange,
    -- | The protocol for connections from clients to your accelerator.
    protocol :: Protocol,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency—that is, the uniqueness—of the request.
    idempotencyToken :: Prelude.Text
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
-- 'clientAffinity', 'createListener_clientAffinity' - Client affinity lets you direct all requests from a user to the same
-- endpoint, if you have stateful applications, regardless of the port and
-- protocol of the client request. Client affinity gives you control over
-- whether to always route each client to the same specific endpoint.
--
-- Global Accelerator uses a consistent-flow hashing algorithm to choose
-- the optimal endpoint for a connection. If client affinity is @NONE@,
-- Global Accelerator uses the \"five-tuple\" (5-tuple) properties—source
-- IP address, source port, destination IP address, destination port, and
-- protocol—to select the hash value, and then chooses the best endpoint.
-- However, with this setting, if someone uses different ports to connect
-- to Global Accelerator, their connections might not be always routed to
-- the same endpoint because the hash value changes.
--
-- If you want a given client to always be routed to the same endpoint, set
-- client affinity to @SOURCE_IP@ instead. When you use the @SOURCE_IP@
-- setting, Global Accelerator uses the \"two-tuple\" (2-tuple) properties—
-- source (client) IP address and destination IP address—to select the hash
-- value.
--
-- The default value is @NONE@.
--
-- 'acceleratorArn', 'createListener_acceleratorArn' - The Amazon Resource Name (ARN) of your accelerator.
--
-- 'portRanges', 'createListener_portRanges' - The list of port ranges to support for connections from clients to your
-- accelerator.
--
-- 'protocol', 'createListener_protocol' - The protocol for connections from clients to your accelerator.
--
-- 'idempotencyToken', 'createListener_idempotencyToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
newCreateListener ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  -- | 'portRanges'
  Prelude.NonEmpty PortRange ->
  -- | 'protocol'
  Protocol ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateListener
newCreateListener
  pAcceleratorArn_
  pPortRanges_
  pProtocol_
  pIdempotencyToken_ =
    CreateListener'
      { clientAffinity = Prelude.Nothing,
        acceleratorArn = pAcceleratorArn_,
        portRanges = Lens.coerced Lens.# pPortRanges_,
        protocol = pProtocol_,
        idempotencyToken = pIdempotencyToken_
      }

-- | Client affinity lets you direct all requests from a user to the same
-- endpoint, if you have stateful applications, regardless of the port and
-- protocol of the client request. Client affinity gives you control over
-- whether to always route each client to the same specific endpoint.
--
-- Global Accelerator uses a consistent-flow hashing algorithm to choose
-- the optimal endpoint for a connection. If client affinity is @NONE@,
-- Global Accelerator uses the \"five-tuple\" (5-tuple) properties—source
-- IP address, source port, destination IP address, destination port, and
-- protocol—to select the hash value, and then chooses the best endpoint.
-- However, with this setting, if someone uses different ports to connect
-- to Global Accelerator, their connections might not be always routed to
-- the same endpoint because the hash value changes.
--
-- If you want a given client to always be routed to the same endpoint, set
-- client affinity to @SOURCE_IP@ instead. When you use the @SOURCE_IP@
-- setting, Global Accelerator uses the \"two-tuple\" (2-tuple) properties—
-- source (client) IP address and destination IP address—to select the hash
-- value.
--
-- The default value is @NONE@.
createListener_clientAffinity :: Lens.Lens' CreateListener (Prelude.Maybe ClientAffinity)
createListener_clientAffinity = Lens.lens (\CreateListener' {clientAffinity} -> clientAffinity) (\s@CreateListener' {} a -> s {clientAffinity = a} :: CreateListener)

-- | The Amazon Resource Name (ARN) of your accelerator.
createListener_acceleratorArn :: Lens.Lens' CreateListener Prelude.Text
createListener_acceleratorArn = Lens.lens (\CreateListener' {acceleratorArn} -> acceleratorArn) (\s@CreateListener' {} a -> s {acceleratorArn = a} :: CreateListener)

-- | The list of port ranges to support for connections from clients to your
-- accelerator.
createListener_portRanges :: Lens.Lens' CreateListener (Prelude.NonEmpty PortRange)
createListener_portRanges = Lens.lens (\CreateListener' {portRanges} -> portRanges) (\s@CreateListener' {} a -> s {portRanges = a} :: CreateListener) Prelude.. Lens.coerced

-- | The protocol for connections from clients to your accelerator.
createListener_protocol :: Lens.Lens' CreateListener Protocol
createListener_protocol = Lens.lens (\CreateListener' {protocol} -> protocol) (\s@CreateListener' {} a -> s {protocol = a} :: CreateListener)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
createListener_idempotencyToken :: Lens.Lens' CreateListener Prelude.Text
createListener_idempotencyToken = Lens.lens (\CreateListener' {idempotencyToken} -> idempotencyToken) (\s@CreateListener' {} a -> s {idempotencyToken = a} :: CreateListener)

instance Core.AWSRequest CreateListener where
  type
    AWSResponse CreateListener =
      CreateListenerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateListenerResponse'
            Prelude.<$> (x Core..?> "Listener")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateListener where
  hashWithSalt _salt CreateListener' {..} =
    _salt `Prelude.hashWithSalt` clientAffinity
      `Prelude.hashWithSalt` acceleratorArn
      `Prelude.hashWithSalt` portRanges
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData CreateListener where
  rnf CreateListener' {..} =
    Prelude.rnf clientAffinity
      `Prelude.seq` Prelude.rnf acceleratorArn
      `Prelude.seq` Prelude.rnf portRanges
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Core.ToHeaders CreateListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.CreateListener" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateListener where
  toJSON CreateListener' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientAffinity" Core..=)
              Prelude.<$> clientAffinity,
            Prelude.Just
              ("AcceleratorArn" Core..= acceleratorArn),
            Prelude.Just ("PortRanges" Core..= portRanges),
            Prelude.Just ("Protocol" Core..= protocol),
            Prelude.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateListener where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateListenerResponse' smart constructor.
data CreateListenerResponse = CreateListenerResponse'
  { -- | The listener that you\'ve created.
    listener :: Prelude.Maybe Listener,
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
-- 'listener', 'createListenerResponse_listener' - The listener that you\'ve created.
--
-- 'httpStatus', 'createListenerResponse_httpStatus' - The response's http status code.
newCreateListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateListenerResponse
newCreateListenerResponse pHttpStatus_ =
  CreateListenerResponse'
    { listener = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The listener that you\'ve created.
createListenerResponse_listener :: Lens.Lens' CreateListenerResponse (Prelude.Maybe Listener)
createListenerResponse_listener = Lens.lens (\CreateListenerResponse' {listener} -> listener) (\s@CreateListenerResponse' {} a -> s {listener = a} :: CreateListenerResponse)

-- | The response's http status code.
createListenerResponse_httpStatus :: Lens.Lens' CreateListenerResponse Prelude.Int
createListenerResponse_httpStatus = Lens.lens (\CreateListenerResponse' {httpStatus} -> httpStatus) (\s@CreateListenerResponse' {} a -> s {httpStatus = a} :: CreateListenerResponse)

instance Prelude.NFData CreateListenerResponse where
  rnf CreateListenerResponse' {..} =
    Prelude.rnf listener
      `Prelude.seq` Prelude.rnf httpStatus
