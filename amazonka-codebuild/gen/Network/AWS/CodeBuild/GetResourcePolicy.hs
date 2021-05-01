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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetResourcePolicy' smart constructor.
data GetResourcePolicy = GetResourcePolicy'
  { -- | The ARN of the resource that is associated with the resource policy.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetResourcePolicy
newGetResourcePolicy pResourceArn_ =
  GetResourcePolicy' {resourceArn = pResourceArn_}

-- | The ARN of the resource that is associated with the resource policy.
getResourcePolicy_resourceArn :: Lens.Lens' GetResourcePolicy Prelude.Text
getResourcePolicy_resourceArn = Lens.lens (\GetResourcePolicy' {resourceArn} -> resourceArn) (\s@GetResourcePolicy' {} a -> s {resourceArn = a} :: GetResourcePolicy)

instance Prelude.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Prelude.<$> (x Prelude..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicy

instance Prelude.NFData GetResourcePolicy

instance Prelude.ToHeaders GetResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.GetResourcePolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("resourceArn" Prelude..= resourceArn)
          ]
      )

instance Prelude.ToPath GetResourcePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The resource policy for the resource identified by the input ARN
    -- parameter.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetResourcePolicyResponse
newGetResourcePolicyResponse pHttpStatus_ =
  GetResourcePolicyResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource policy for the resource identified by the input ARN
-- parameter.
getResourcePolicyResponse_policy :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_policy = Lens.lens (\GetResourcePolicyResponse' {policy} -> policy) (\s@GetResourcePolicyResponse' {} a -> s {policy = a} :: GetResourcePolicyResponse)

-- | The response's http status code.
getResourcePolicyResponse_httpStatus :: Lens.Lens' GetResourcePolicyResponse Prelude.Int
getResourcePolicyResponse_httpStatus = Lens.lens (\GetResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@GetResourcePolicyResponse' {} a -> s {httpStatus = a} :: GetResourcePolicyResponse)

instance Prelude.NFData GetResourcePolicyResponse
