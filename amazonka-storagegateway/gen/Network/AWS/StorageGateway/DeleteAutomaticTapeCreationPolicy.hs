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
-- Module      : Network.AWS.StorageGateway.DeleteAutomaticTapeCreationPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the automatic tape creation policy of a gateway. If you delete
-- this policy, new virtual tapes must be created manually. Use the Amazon
-- Resource Name (ARN) of the gateway in your request to remove the policy.
module Network.AWS.StorageGateway.DeleteAutomaticTapeCreationPolicy
  ( -- * Creating a Request
    DeleteAutomaticTapeCreationPolicy (..),
    newDeleteAutomaticTapeCreationPolicy,

    -- * Request Lenses
    deleteAutomaticTapeCreationPolicy_gatewayARN,

    -- * Destructuring the Response
    DeleteAutomaticTapeCreationPolicyResponse (..),
    newDeleteAutomaticTapeCreationPolicyResponse,

    -- * Response Lenses
    deleteAutomaticTapeCreationPolicyResponse_gatewayARN,
    deleteAutomaticTapeCreationPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDeleteAutomaticTapeCreationPolicy' smart constructor.
data DeleteAutomaticTapeCreationPolicy = DeleteAutomaticTapeCreationPolicy'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAutomaticTapeCreationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'deleteAutomaticTapeCreationPolicy_gatewayARN' - Undocumented member.
newDeleteAutomaticTapeCreationPolicy ::
  -- | 'gatewayARN'
  Core.Text ->
  DeleteAutomaticTapeCreationPolicy
newDeleteAutomaticTapeCreationPolicy pGatewayARN_ =
  DeleteAutomaticTapeCreationPolicy'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
deleteAutomaticTapeCreationPolicy_gatewayARN :: Lens.Lens' DeleteAutomaticTapeCreationPolicy Core.Text
deleteAutomaticTapeCreationPolicy_gatewayARN = Lens.lens (\DeleteAutomaticTapeCreationPolicy' {gatewayARN} -> gatewayARN) (\s@DeleteAutomaticTapeCreationPolicy' {} a -> s {gatewayARN = a} :: DeleteAutomaticTapeCreationPolicy)

instance
  Core.AWSRequest
    DeleteAutomaticTapeCreationPolicy
  where
  type
    AWSResponse DeleteAutomaticTapeCreationPolicy =
      DeleteAutomaticTapeCreationPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAutomaticTapeCreationPolicyResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteAutomaticTapeCreationPolicy

instance
  Core.NFData
    DeleteAutomaticTapeCreationPolicy

instance
  Core.ToHeaders
    DeleteAutomaticTapeCreationPolicy
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DeleteAutomaticTapeCreationPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DeleteAutomaticTapeCreationPolicy
  where
  toJSON DeleteAutomaticTapeCreationPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance
  Core.ToPath
    DeleteAutomaticTapeCreationPolicy
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteAutomaticTapeCreationPolicy
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAutomaticTapeCreationPolicyResponse' smart constructor.
data DeleteAutomaticTapeCreationPolicyResponse = DeleteAutomaticTapeCreationPolicyResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAutomaticTapeCreationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'deleteAutomaticTapeCreationPolicyResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'deleteAutomaticTapeCreationPolicyResponse_httpStatus' - The response's http status code.
newDeleteAutomaticTapeCreationPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteAutomaticTapeCreationPolicyResponse
newDeleteAutomaticTapeCreationPolicyResponse
  pHttpStatus_ =
    DeleteAutomaticTapeCreationPolicyResponse'
      { gatewayARN =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
deleteAutomaticTapeCreationPolicyResponse_gatewayARN :: Lens.Lens' DeleteAutomaticTapeCreationPolicyResponse (Core.Maybe Core.Text)
deleteAutomaticTapeCreationPolicyResponse_gatewayARN = Lens.lens (\DeleteAutomaticTapeCreationPolicyResponse' {gatewayARN} -> gatewayARN) (\s@DeleteAutomaticTapeCreationPolicyResponse' {} a -> s {gatewayARN = a} :: DeleteAutomaticTapeCreationPolicyResponse)

-- | The response's http status code.
deleteAutomaticTapeCreationPolicyResponse_httpStatus :: Lens.Lens' DeleteAutomaticTapeCreationPolicyResponse Core.Int
deleteAutomaticTapeCreationPolicyResponse_httpStatus = Lens.lens (\DeleteAutomaticTapeCreationPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteAutomaticTapeCreationPolicyResponse' {} a -> s {httpStatus = a} :: DeleteAutomaticTapeCreationPolicyResponse)

instance
  Core.NFData
    DeleteAutomaticTapeCreationPolicyResponse
