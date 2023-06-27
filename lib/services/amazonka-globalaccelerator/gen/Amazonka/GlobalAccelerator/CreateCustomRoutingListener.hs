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
-- Module      : Amazonka.GlobalAccelerator.CreateCustomRoutingListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a listener to process inbound connections from clients to a
-- custom routing accelerator. Connections arrive to assigned static IP
-- addresses on the port range that you specify.
module Amazonka.GlobalAccelerator.CreateCustomRoutingListener
  ( -- * Creating a Request
    CreateCustomRoutingListener (..),
    newCreateCustomRoutingListener,

    -- * Request Lenses
    createCustomRoutingListener_acceleratorArn,
    createCustomRoutingListener_portRanges,
    createCustomRoutingListener_idempotencyToken,

    -- * Destructuring the Response
    CreateCustomRoutingListenerResponse (..),
    newCreateCustomRoutingListenerResponse,

    -- * Response Lenses
    createCustomRoutingListenerResponse_listener,
    createCustomRoutingListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomRoutingListener' smart constructor.
data CreateCustomRoutingListener = CreateCustomRoutingListener'
  { -- | The Amazon Resource Name (ARN) of the accelerator for a custom routing
    -- listener.
    acceleratorArn :: Prelude.Text,
    -- | The port range to support for connections from clients to your
    -- accelerator.
    --
    -- Separately, you set port ranges for endpoints. For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
    portRanges :: Prelude.NonEmpty PortRange,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency—that is, the uniqueness—of the request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomRoutingListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorArn', 'createCustomRoutingListener_acceleratorArn' - The Amazon Resource Name (ARN) of the accelerator for a custom routing
-- listener.
--
-- 'portRanges', 'createCustomRoutingListener_portRanges' - The port range to support for connections from clients to your
-- accelerator.
--
-- Separately, you set port ranges for endpoints. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
--
-- 'idempotencyToken', 'createCustomRoutingListener_idempotencyToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
newCreateCustomRoutingListener ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  -- | 'portRanges'
  Prelude.NonEmpty PortRange ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateCustomRoutingListener
newCreateCustomRoutingListener
  pAcceleratorArn_
  pPortRanges_
  pIdempotencyToken_ =
    CreateCustomRoutingListener'
      { acceleratorArn =
          pAcceleratorArn_,
        portRanges = Lens.coerced Lens.# pPortRanges_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The Amazon Resource Name (ARN) of the accelerator for a custom routing
-- listener.
createCustomRoutingListener_acceleratorArn :: Lens.Lens' CreateCustomRoutingListener Prelude.Text
createCustomRoutingListener_acceleratorArn = Lens.lens (\CreateCustomRoutingListener' {acceleratorArn} -> acceleratorArn) (\s@CreateCustomRoutingListener' {} a -> s {acceleratorArn = a} :: CreateCustomRoutingListener)

-- | The port range to support for connections from clients to your
-- accelerator.
--
-- Separately, you set port ranges for endpoints. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
createCustomRoutingListener_portRanges :: Lens.Lens' CreateCustomRoutingListener (Prelude.NonEmpty PortRange)
createCustomRoutingListener_portRanges = Lens.lens (\CreateCustomRoutingListener' {portRanges} -> portRanges) (\s@CreateCustomRoutingListener' {} a -> s {portRanges = a} :: CreateCustomRoutingListener) Prelude.. Lens.coerced

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
createCustomRoutingListener_idempotencyToken :: Lens.Lens' CreateCustomRoutingListener Prelude.Text
createCustomRoutingListener_idempotencyToken = Lens.lens (\CreateCustomRoutingListener' {idempotencyToken} -> idempotencyToken) (\s@CreateCustomRoutingListener' {} a -> s {idempotencyToken = a} :: CreateCustomRoutingListener)

instance Core.AWSRequest CreateCustomRoutingListener where
  type
    AWSResponse CreateCustomRoutingListener =
      CreateCustomRoutingListenerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomRoutingListenerResponse'
            Prelude.<$> (x Data..?> "Listener")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomRoutingListener where
  hashWithSalt _salt CreateCustomRoutingListener' {..} =
    _salt
      `Prelude.hashWithSalt` acceleratorArn
      `Prelude.hashWithSalt` portRanges
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData CreateCustomRoutingListener where
  rnf CreateCustomRoutingListener' {..} =
    Prelude.rnf acceleratorArn
      `Prelude.seq` Prelude.rnf portRanges
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Data.ToHeaders CreateCustomRoutingListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.CreateCustomRoutingListener" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCustomRoutingListener where
  toJSON CreateCustomRoutingListener' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AcceleratorArn" Data..= acceleratorArn),
            Prelude.Just ("PortRanges" Data..= portRanges),
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath CreateCustomRoutingListener where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCustomRoutingListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomRoutingListenerResponse' smart constructor.
data CreateCustomRoutingListenerResponse = CreateCustomRoutingListenerResponse'
  { -- | The listener that you\'ve created for a custom routing accelerator.
    listener :: Prelude.Maybe CustomRoutingListener,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomRoutingListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listener', 'createCustomRoutingListenerResponse_listener' - The listener that you\'ve created for a custom routing accelerator.
--
-- 'httpStatus', 'createCustomRoutingListenerResponse_httpStatus' - The response's http status code.
newCreateCustomRoutingListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomRoutingListenerResponse
newCreateCustomRoutingListenerResponse pHttpStatus_ =
  CreateCustomRoutingListenerResponse'
    { listener =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The listener that you\'ve created for a custom routing accelerator.
createCustomRoutingListenerResponse_listener :: Lens.Lens' CreateCustomRoutingListenerResponse (Prelude.Maybe CustomRoutingListener)
createCustomRoutingListenerResponse_listener = Lens.lens (\CreateCustomRoutingListenerResponse' {listener} -> listener) (\s@CreateCustomRoutingListenerResponse' {} a -> s {listener = a} :: CreateCustomRoutingListenerResponse)

-- | The response's http status code.
createCustomRoutingListenerResponse_httpStatus :: Lens.Lens' CreateCustomRoutingListenerResponse Prelude.Int
createCustomRoutingListenerResponse_httpStatus = Lens.lens (\CreateCustomRoutingListenerResponse' {httpStatus} -> httpStatus) (\s@CreateCustomRoutingListenerResponse' {} a -> s {httpStatus = a} :: CreateCustomRoutingListenerResponse)

instance
  Prelude.NFData
    CreateCustomRoutingListenerResponse
  where
  rnf CreateCustomRoutingListenerResponse' {..} =
    Prelude.rnf listener
      `Prelude.seq` Prelude.rnf httpStatus
