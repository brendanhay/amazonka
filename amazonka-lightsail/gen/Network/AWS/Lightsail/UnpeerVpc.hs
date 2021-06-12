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
-- Module      : Network.AWS.Lightsail.UnpeerVpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to unpeer the Lightsail VPC from the user\'s default VPC.
module Network.AWS.Lightsail.UnpeerVpc
  ( -- * Creating a Request
    UnpeerVpc (..),
    newUnpeerVpc,

    -- * Destructuring the Response
    UnpeerVpcResponse (..),
    newUnpeerVpcResponse,

    -- * Response Lenses
    unpeerVpcResponse_operation,
    unpeerVpcResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUnpeerVpc' smart constructor.
data UnpeerVpc = UnpeerVpc'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnpeerVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnpeerVpc ::
  UnpeerVpc
newUnpeerVpc = UnpeerVpc'

instance Core.AWSRequest UnpeerVpc where
  type AWSResponse UnpeerVpc = UnpeerVpcResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UnpeerVpcResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UnpeerVpc

instance Core.NFData UnpeerVpc

instance Core.ToHeaders UnpeerVpc where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.UnpeerVpc" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UnpeerVpc where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath UnpeerVpc where
  toPath = Core.const "/"

instance Core.ToQuery UnpeerVpc where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUnpeerVpcResponse' smart constructor.
data UnpeerVpcResponse = UnpeerVpcResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnpeerVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'unpeerVpcResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'unpeerVpcResponse_httpStatus' - The response's http status code.
newUnpeerVpcResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UnpeerVpcResponse
newUnpeerVpcResponse pHttpStatus_ =
  UnpeerVpcResponse'
    { operation = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
unpeerVpcResponse_operation :: Lens.Lens' UnpeerVpcResponse (Core.Maybe Operation)
unpeerVpcResponse_operation = Lens.lens (\UnpeerVpcResponse' {operation} -> operation) (\s@UnpeerVpcResponse' {} a -> s {operation = a} :: UnpeerVpcResponse)

-- | The response's http status code.
unpeerVpcResponse_httpStatus :: Lens.Lens' UnpeerVpcResponse Core.Int
unpeerVpcResponse_httpStatus = Lens.lens (\UnpeerVpcResponse' {httpStatus} -> httpStatus) (\s@UnpeerVpcResponse' {} a -> s {httpStatus = a} :: UnpeerVpcResponse)

instance Core.NFData UnpeerVpcResponse
