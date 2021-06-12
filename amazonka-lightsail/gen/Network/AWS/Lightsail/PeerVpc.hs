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
-- Module      : Network.AWS.Lightsail.PeerVpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tries to peer the Lightsail VPC with the user\'s default VPC.
module Network.AWS.Lightsail.PeerVpc
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPeerVpc' smart constructor.
data PeerVpc = PeerVpc'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PeerVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPeerVpc ::
  PeerVpc
newPeerVpc = PeerVpc'

instance Core.AWSRequest PeerVpc where
  type AWSResponse PeerVpc = PeerVpcResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PeerVpcResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PeerVpc

instance Core.NFData PeerVpc

instance Core.ToHeaders PeerVpc where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.PeerVpc" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PeerVpc where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath PeerVpc where
  toPath = Core.const "/"

instance Core.ToQuery PeerVpc where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPeerVpcResponse' smart constructor.
data PeerVpcResponse = PeerVpcResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PeerVpcResponse
newPeerVpcResponse pHttpStatus_ =
  PeerVpcResponse'
    { operation = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
peerVpcResponse_operation :: Lens.Lens' PeerVpcResponse (Core.Maybe Operation)
peerVpcResponse_operation = Lens.lens (\PeerVpcResponse' {operation} -> operation) (\s@PeerVpcResponse' {} a -> s {operation = a} :: PeerVpcResponse)

-- | The response's http status code.
peerVpcResponse_httpStatus :: Lens.Lens' PeerVpcResponse Core.Int
peerVpcResponse_httpStatus = Lens.lens (\PeerVpcResponse' {httpStatus} -> httpStatus) (\s@PeerVpcResponse' {} a -> s {httpStatus = a} :: PeerVpcResponse)

instance Core.NFData PeerVpcResponse
