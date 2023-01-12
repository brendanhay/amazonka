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
-- Module      : Amazonka.GlobalAccelerator.UpdateListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a listener.
module Amazonka.GlobalAccelerator.UpdateListener
  ( -- * Creating a Request
    UpdateListener (..),
    newUpdateListener,

    -- * Request Lenses
    updateListener_clientAffinity,
    updateListener_portRanges,
    updateListener_protocol,
    updateListener_listenerArn,

    -- * Destructuring the Response
    UpdateListenerResponse (..),
    newUpdateListenerResponse,

    -- * Response Lenses
    updateListenerResponse_listener,
    updateListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateListener' smart constructor.
data UpdateListener = UpdateListener'
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
    -- | The updated list of port ranges for the connections from clients to the
    -- accelerator.
    portRanges :: Prelude.Maybe (Prelude.NonEmpty PortRange),
    -- | The updated protocol for the connections from clients to the
    -- accelerator.
    protocol :: Prelude.Maybe Protocol,
    -- | The Amazon Resource Name (ARN) of the listener to update.
    listenerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientAffinity', 'updateListener_clientAffinity' - Client affinity lets you direct all requests from a user to the same
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
-- 'portRanges', 'updateListener_portRanges' - The updated list of port ranges for the connections from clients to the
-- accelerator.
--
-- 'protocol', 'updateListener_protocol' - The updated protocol for the connections from clients to the
-- accelerator.
--
-- 'listenerArn', 'updateListener_listenerArn' - The Amazon Resource Name (ARN) of the listener to update.
newUpdateListener ::
  -- | 'listenerArn'
  Prelude.Text ->
  UpdateListener
newUpdateListener pListenerArn_ =
  UpdateListener'
    { clientAffinity = Prelude.Nothing,
      portRanges = Prelude.Nothing,
      protocol = Prelude.Nothing,
      listenerArn = pListenerArn_
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
updateListener_clientAffinity :: Lens.Lens' UpdateListener (Prelude.Maybe ClientAffinity)
updateListener_clientAffinity = Lens.lens (\UpdateListener' {clientAffinity} -> clientAffinity) (\s@UpdateListener' {} a -> s {clientAffinity = a} :: UpdateListener)

-- | The updated list of port ranges for the connections from clients to the
-- accelerator.
updateListener_portRanges :: Lens.Lens' UpdateListener (Prelude.Maybe (Prelude.NonEmpty PortRange))
updateListener_portRanges = Lens.lens (\UpdateListener' {portRanges} -> portRanges) (\s@UpdateListener' {} a -> s {portRanges = a} :: UpdateListener) Prelude.. Lens.mapping Lens.coerced

-- | The updated protocol for the connections from clients to the
-- accelerator.
updateListener_protocol :: Lens.Lens' UpdateListener (Prelude.Maybe Protocol)
updateListener_protocol = Lens.lens (\UpdateListener' {protocol} -> protocol) (\s@UpdateListener' {} a -> s {protocol = a} :: UpdateListener)

-- | The Amazon Resource Name (ARN) of the listener to update.
updateListener_listenerArn :: Lens.Lens' UpdateListener Prelude.Text
updateListener_listenerArn = Lens.lens (\UpdateListener' {listenerArn} -> listenerArn) (\s@UpdateListener' {} a -> s {listenerArn = a} :: UpdateListener)

instance Core.AWSRequest UpdateListener where
  type
    AWSResponse UpdateListener =
      UpdateListenerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateListenerResponse'
            Prelude.<$> (x Data..?> "Listener")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateListener where
  hashWithSalt _salt UpdateListener' {..} =
    _salt `Prelude.hashWithSalt` clientAffinity
      `Prelude.hashWithSalt` portRanges
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` listenerArn

instance Prelude.NFData UpdateListener where
  rnf UpdateListener' {..} =
    Prelude.rnf clientAffinity
      `Prelude.seq` Prelude.rnf portRanges
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf listenerArn

instance Data.ToHeaders UpdateListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.UpdateListener" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateListener where
  toJSON UpdateListener' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientAffinity" Data..=)
              Prelude.<$> clientAffinity,
            ("PortRanges" Data..=) Prelude.<$> portRanges,
            ("Protocol" Data..=) Prelude.<$> protocol,
            Prelude.Just ("ListenerArn" Data..= listenerArn)
          ]
      )

instance Data.ToPath UpdateListener where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateListenerResponse' smart constructor.
data UpdateListenerResponse = UpdateListenerResponse'
  { -- | Information for the updated listener.
    listener :: Prelude.Maybe Listener,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listener', 'updateListenerResponse_listener' - Information for the updated listener.
--
-- 'httpStatus', 'updateListenerResponse_httpStatus' - The response's http status code.
newUpdateListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateListenerResponse
newUpdateListenerResponse pHttpStatus_ =
  UpdateListenerResponse'
    { listener = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information for the updated listener.
updateListenerResponse_listener :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe Listener)
updateListenerResponse_listener = Lens.lens (\UpdateListenerResponse' {listener} -> listener) (\s@UpdateListenerResponse' {} a -> s {listener = a} :: UpdateListenerResponse)

-- | The response's http status code.
updateListenerResponse_httpStatus :: Lens.Lens' UpdateListenerResponse Prelude.Int
updateListenerResponse_httpStatus = Lens.lens (\UpdateListenerResponse' {httpStatus} -> httpStatus) (\s@UpdateListenerResponse' {} a -> s {httpStatus = a} :: UpdateListenerResponse)

instance Prelude.NFData UpdateListenerResponse where
  rnf UpdateListenerResponse' {..} =
    Prelude.rnf listener
      `Prelude.seq` Prelude.rnf httpStatus
