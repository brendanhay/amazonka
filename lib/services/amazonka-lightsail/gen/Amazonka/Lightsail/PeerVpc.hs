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
-- Module      : Amazonka.Lightsail.PeerVpc
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Peers the Lightsail VPC with the user\'s default VPC.
module Amazonka.Lightsail.PeerVpc
  ( -- * Creating a Request
    PeerVpc (..),
    newPeerVpc,

    -- * Destructuring the Response
    PeerVpcResponse (..),
    newPeerVpcResponse,

    -- * Response Lenses
    peerVpcResponse_operation,
    peerVpcResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPeerVpc' smart constructor.
data PeerVpc = PeerVpc'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PeerVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPeerVpc ::
  PeerVpc
newPeerVpc = PeerVpc'

instance Core.AWSRequest PeerVpc where
  type AWSResponse PeerVpc = PeerVpcResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PeerVpcResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PeerVpc where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData PeerVpc where
  rnf _ = ()

instance Data.ToHeaders PeerVpc where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Lightsail_20161128.PeerVpc" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PeerVpc where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath PeerVpc where
  toPath = Prelude.const "/"

instance Data.ToQuery PeerVpc where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPeerVpcResponse' smart constructor.
data PeerVpcResponse = PeerVpcResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PeerVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'peerVpcResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'peerVpcResponse_httpStatus' - The response's http status code.
newPeerVpcResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PeerVpcResponse
newPeerVpcResponse pHttpStatus_ =
  PeerVpcResponse'
    { operation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
peerVpcResponse_operation :: Lens.Lens' PeerVpcResponse (Prelude.Maybe Operation)
peerVpcResponse_operation = Lens.lens (\PeerVpcResponse' {operation} -> operation) (\s@PeerVpcResponse' {} a -> s {operation = a} :: PeerVpcResponse)

-- | The response's http status code.
peerVpcResponse_httpStatus :: Lens.Lens' PeerVpcResponse Prelude.Int
peerVpcResponse_httpStatus = Lens.lens (\PeerVpcResponse' {httpStatus} -> httpStatus) (\s@PeerVpcResponse' {} a -> s {httpStatus = a} :: PeerVpcResponse)

instance Prelude.NFData PeerVpcResponse where
  rnf PeerVpcResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
