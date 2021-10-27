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
-- Module      : Network.AWS.ImageBuilder.GetComponentPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a component policy.
module Network.AWS.ImageBuilder.GetComponentPolicy
  ( -- * Creating a Request
    GetComponentPolicy (..),
    newGetComponentPolicy,

    -- * Request Lenses
    getComponentPolicy_componentArn,

    -- * Destructuring the Response
    GetComponentPolicyResponse (..),
    newGetComponentPolicyResponse,

    -- * Response Lenses
    getComponentPolicyResponse_requestId,
    getComponentPolicyResponse_policy,
    getComponentPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ImageBuilder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetComponentPolicy' smart constructor.
data GetComponentPolicy = GetComponentPolicy'
  { -- | The Amazon Resource Name (ARN) of the component whose policy you want to
    -- retrieve.
    componentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponentPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentArn', 'getComponentPolicy_componentArn' - The Amazon Resource Name (ARN) of the component whose policy you want to
-- retrieve.
newGetComponentPolicy ::
  -- | 'componentArn'
  Prelude.Text ->
  GetComponentPolicy
newGetComponentPolicy pComponentArn_ =
  GetComponentPolicy' {componentArn = pComponentArn_}

-- | The Amazon Resource Name (ARN) of the component whose policy you want to
-- retrieve.
getComponentPolicy_componentArn :: Lens.Lens' GetComponentPolicy Prelude.Text
getComponentPolicy_componentArn = Lens.lens (\GetComponentPolicy' {componentArn} -> componentArn) (\s@GetComponentPolicy' {} a -> s {componentArn = a} :: GetComponentPolicy)

instance Core.AWSRequest GetComponentPolicy where
  type
    AWSResponse GetComponentPolicy =
      GetComponentPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComponentPolicyResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComponentPolicy

instance Prelude.NFData GetComponentPolicy

instance Core.ToHeaders GetComponentPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetComponentPolicy where
  toPath = Prelude.const "/GetComponentPolicy"

instance Core.ToQuery GetComponentPolicy where
  toQuery GetComponentPolicy' {..} =
    Prelude.mconcat
      ["componentArn" Core.=: componentArn]

-- | /See:/ 'newGetComponentPolicyResponse' smart constructor.
data GetComponentPolicyResponse = GetComponentPolicyResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The component policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponentPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'getComponentPolicyResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'policy', 'getComponentPolicyResponse_policy' - The component policy.
--
-- 'httpStatus', 'getComponentPolicyResponse_httpStatus' - The response's http status code.
newGetComponentPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComponentPolicyResponse
newGetComponentPolicyResponse pHttpStatus_ =
  GetComponentPolicyResponse'
    { requestId =
        Prelude.Nothing,
      policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
getComponentPolicyResponse_requestId :: Lens.Lens' GetComponentPolicyResponse (Prelude.Maybe Prelude.Text)
getComponentPolicyResponse_requestId = Lens.lens (\GetComponentPolicyResponse' {requestId} -> requestId) (\s@GetComponentPolicyResponse' {} a -> s {requestId = a} :: GetComponentPolicyResponse)

-- | The component policy.
getComponentPolicyResponse_policy :: Lens.Lens' GetComponentPolicyResponse (Prelude.Maybe Prelude.Text)
getComponentPolicyResponse_policy = Lens.lens (\GetComponentPolicyResponse' {policy} -> policy) (\s@GetComponentPolicyResponse' {} a -> s {policy = a} :: GetComponentPolicyResponse)

-- | The response's http status code.
getComponentPolicyResponse_httpStatus :: Lens.Lens' GetComponentPolicyResponse Prelude.Int
getComponentPolicyResponse_httpStatus = Lens.lens (\GetComponentPolicyResponse' {httpStatus} -> httpStatus) (\s@GetComponentPolicyResponse' {} a -> s {httpStatus = a} :: GetComponentPolicyResponse)

instance Prelude.NFData GetComponentPolicyResponse
