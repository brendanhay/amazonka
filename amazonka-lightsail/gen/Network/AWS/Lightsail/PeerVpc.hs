{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPeerVpc' smart constructor.
data PeerVpc = PeerVpc'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PeerVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPeerVpc ::
  PeerVpc
newPeerVpc = PeerVpc'

instance Prelude.AWSRequest PeerVpc where
  type Rs PeerVpc = PeerVpcResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PeerVpcResponse'
            Prelude.<$> (x Prelude..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PeerVpc

instance Prelude.NFData PeerVpc

instance Prelude.ToHeaders PeerVpc where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("Lightsail_20161128.PeerVpc" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PeerVpc where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath PeerVpc where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PeerVpc where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PeerVpcResponse
