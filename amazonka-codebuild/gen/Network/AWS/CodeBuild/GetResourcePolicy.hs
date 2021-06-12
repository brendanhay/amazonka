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
-- Module      : Network.AWS.CodeBuild.GetResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a resource policy that is identified by its resource ARN.
module Network.AWS.CodeBuild.GetResourcePolicy
  ( -- * Creating a Request
    GetResourcePolicy (..),
    newGetResourcePolicy,

    -- * Request Lenses
    getResourcePolicy_resourceArn,

    -- * Destructuring the Response
    GetResourcePolicyResponse (..),
    newGetResourcePolicyResponse,

    -- * Response Lenses
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetResourcePolicy' smart constructor.
data GetResourcePolicy = GetResourcePolicy'
  { -- | The ARN of the resource that is associated with the resource policy.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getResourcePolicy_resourceArn' - The ARN of the resource that is associated with the resource policy.
newGetResourcePolicy ::
  -- | 'resourceArn'
  Core.Text ->
  GetResourcePolicy
newGetResourcePolicy pResourceArn_ =
  GetResourcePolicy' {resourceArn = pResourceArn_}

-- | The ARN of the resource that is associated with the resource policy.
getResourcePolicy_resourceArn :: Lens.Lens' GetResourcePolicy Core.Text
getResourcePolicy_resourceArn = Lens.lens (\GetResourcePolicy' {resourceArn} -> resourceArn) (\s@GetResourcePolicy' {} a -> s {resourceArn = a} :: GetResourcePolicy)

instance Core.AWSRequest GetResourcePolicy where
  type
    AWSResponse GetResourcePolicy =
      GetResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Core.<$> (x Core..?> "policy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetResourcePolicy

instance Core.NFData GetResourcePolicy

instance Core.ToHeaders GetResourcePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.GetResourcePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("resourceArn" Core..= resourceArn)]
      )

instance Core.ToPath GetResourcePolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetResourcePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The resource policy for the resource identified by the input ARN
    -- parameter.
    policy :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getResourcePolicyResponse_policy' - The resource policy for the resource identified by the input ARN
-- parameter.
--
-- 'httpStatus', 'getResourcePolicyResponse_httpStatus' - The response's http status code.
newGetResourcePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetResourcePolicyResponse
newGetResourcePolicyResponse pHttpStatus_ =
  GetResourcePolicyResponse'
    { policy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource policy for the resource identified by the input ARN
-- parameter.
getResourcePolicyResponse_policy :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Core.Text)
getResourcePolicyResponse_policy = Lens.lens (\GetResourcePolicyResponse' {policy} -> policy) (\s@GetResourcePolicyResponse' {} a -> s {policy = a} :: GetResourcePolicyResponse)

-- | The response's http status code.
getResourcePolicyResponse_httpStatus :: Lens.Lens' GetResourcePolicyResponse Core.Int
getResourcePolicyResponse_httpStatus = Lens.lens (\GetResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@GetResourcePolicyResponse' {} a -> s {httpStatus = a} :: GetResourcePolicyResponse)

instance Core.NFData GetResourcePolicyResponse
