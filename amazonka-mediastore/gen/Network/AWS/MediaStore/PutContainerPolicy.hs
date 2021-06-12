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
-- Module      : Network.AWS.MediaStore.PutContainerPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an access policy for the specified container to restrict the
-- users and clients that can access it. For information about the data
-- that is included in an access policy, see the
-- <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide>.
--
-- For this release of the REST API, you can create only one policy for a
-- container. If you enter @PutContainerPolicy@ twice, the second command
-- modifies the existing policy.
module Network.AWS.MediaStore.PutContainerPolicy
  ( -- * Creating a Request
    PutContainerPolicy (..),
    newPutContainerPolicy,

    -- * Request Lenses
    putContainerPolicy_containerName,
    putContainerPolicy_policy,

    -- * Destructuring the Response
    PutContainerPolicyResponse (..),
    newPutContainerPolicyResponse,

    -- * Response Lenses
    putContainerPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutContainerPolicy' smart constructor.
data PutContainerPolicy = PutContainerPolicy'
  { -- | The name of the container.
    containerName :: Core.Text,
    -- | The contents of the policy, which includes the following:
    --
    -- -   One @Version@ tag
    --
    -- -   One @Statement@ tag that contains the standard tags for the policy.
    policy :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutContainerPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'putContainerPolicy_containerName' - The name of the container.
--
-- 'policy', 'putContainerPolicy_policy' - The contents of the policy, which includes the following:
--
-- -   One @Version@ tag
--
-- -   One @Statement@ tag that contains the standard tags for the policy.
newPutContainerPolicy ::
  -- | 'containerName'
  Core.Text ->
  -- | 'policy'
  Core.Text ->
  PutContainerPolicy
newPutContainerPolicy pContainerName_ pPolicy_ =
  PutContainerPolicy'
    { containerName =
        pContainerName_,
      policy = pPolicy_
    }

-- | The name of the container.
putContainerPolicy_containerName :: Lens.Lens' PutContainerPolicy Core.Text
putContainerPolicy_containerName = Lens.lens (\PutContainerPolicy' {containerName} -> containerName) (\s@PutContainerPolicy' {} a -> s {containerName = a} :: PutContainerPolicy)

-- | The contents of the policy, which includes the following:
--
-- -   One @Version@ tag
--
-- -   One @Statement@ tag that contains the standard tags for the policy.
putContainerPolicy_policy :: Lens.Lens' PutContainerPolicy Core.Text
putContainerPolicy_policy = Lens.lens (\PutContainerPolicy' {policy} -> policy) (\s@PutContainerPolicy' {} a -> s {policy = a} :: PutContainerPolicy)

instance Core.AWSRequest PutContainerPolicy where
  type
    AWSResponse PutContainerPolicy =
      PutContainerPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutContainerPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutContainerPolicy

instance Core.NFData PutContainerPolicy

instance Core.ToHeaders PutContainerPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.PutContainerPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutContainerPolicy where
  toJSON PutContainerPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContainerName" Core..= containerName),
            Core.Just ("Policy" Core..= policy)
          ]
      )

instance Core.ToPath PutContainerPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutContainerPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutContainerPolicyResponse' smart constructor.
data PutContainerPolicyResponse = PutContainerPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutContainerPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putContainerPolicyResponse_httpStatus' - The response's http status code.
newPutContainerPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutContainerPolicyResponse
newPutContainerPolicyResponse pHttpStatus_ =
  PutContainerPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putContainerPolicyResponse_httpStatus :: Lens.Lens' PutContainerPolicyResponse Core.Int
putContainerPolicyResponse_httpStatus = Lens.lens (\PutContainerPolicyResponse' {httpStatus} -> httpStatus) (\s@PutContainerPolicyResponse' {} a -> s {httpStatus = a} :: PutContainerPolicyResponse)

instance Core.NFData PutContainerPolicyResponse
