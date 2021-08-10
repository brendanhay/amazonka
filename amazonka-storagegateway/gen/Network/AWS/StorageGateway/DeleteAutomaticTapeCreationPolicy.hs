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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDeleteAutomaticTapeCreationPolicy' smart constructor.
data DeleteAutomaticTapeCreationPolicy = DeleteAutomaticTapeCreationPolicy'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteAutomaticTapeCreationPolicy
newDeleteAutomaticTapeCreationPolicy pGatewayARN_ =
  DeleteAutomaticTapeCreationPolicy'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
deleteAutomaticTapeCreationPolicy_gatewayARN :: Lens.Lens' DeleteAutomaticTapeCreationPolicy Prelude.Text
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
            Prelude.<$> (x Core..?> "GatewayARN")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAutomaticTapeCreationPolicy

instance
  Prelude.NFData
    DeleteAutomaticTapeCreationPolicy

instance
  Core.ToHeaders
    DeleteAutomaticTapeCreationPolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DeleteAutomaticTapeCreationPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeleteAutomaticTapeCreationPolicy
  where
  toJSON DeleteAutomaticTapeCreationPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance
  Core.ToPath
    DeleteAutomaticTapeCreationPolicy
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteAutomaticTapeCreationPolicy
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAutomaticTapeCreationPolicyResponse' smart constructor.
data DeleteAutomaticTapeCreationPolicyResponse = DeleteAutomaticTapeCreationPolicyResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteAutomaticTapeCreationPolicyResponse
newDeleteAutomaticTapeCreationPolicyResponse
  pHttpStatus_ =
    DeleteAutomaticTapeCreationPolicyResponse'
      { gatewayARN =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
deleteAutomaticTapeCreationPolicyResponse_gatewayARN :: Lens.Lens' DeleteAutomaticTapeCreationPolicyResponse (Prelude.Maybe Prelude.Text)
deleteAutomaticTapeCreationPolicyResponse_gatewayARN = Lens.lens (\DeleteAutomaticTapeCreationPolicyResponse' {gatewayARN} -> gatewayARN) (\s@DeleteAutomaticTapeCreationPolicyResponse' {} a -> s {gatewayARN = a} :: DeleteAutomaticTapeCreationPolicyResponse)

-- | The response's http status code.
deleteAutomaticTapeCreationPolicyResponse_httpStatus :: Lens.Lens' DeleteAutomaticTapeCreationPolicyResponse Prelude.Int
deleteAutomaticTapeCreationPolicyResponse_httpStatus = Lens.lens (\DeleteAutomaticTapeCreationPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteAutomaticTapeCreationPolicyResponse' {} a -> s {httpStatus = a} :: DeleteAutomaticTapeCreationPolicyResponse)

instance
  Prelude.NFData
    DeleteAutomaticTapeCreationPolicyResponse
