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
-- Module      : Amazonka.ImageBuilder.GetComponentPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a component policy.
module Amazonka.ImageBuilder.GetComponentPolicy
  ( -- * Creating a Request
    GetComponentPolicy (..),
    newGetComponentPolicy,

    -- * Request Lenses
    getComponentPolicy_componentArn,

    -- * Destructuring the Response
    GetComponentPolicyResponse (..),
    newGetComponentPolicyResponse,

    -- * Response Lenses
    getComponentPolicyResponse_policy,
    getComponentPolicyResponse_requestId,
    getComponentPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComponentPolicyResponse'
            Prelude.<$> (x Data..?> "policy")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComponentPolicy where
  hashWithSalt _salt GetComponentPolicy' {..} =
    _salt `Prelude.hashWithSalt` componentArn

instance Prelude.NFData GetComponentPolicy where
  rnf GetComponentPolicy' {..} =
    Prelude.rnf componentArn

instance Data.ToHeaders GetComponentPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetComponentPolicy where
  toPath = Prelude.const "/GetComponentPolicy"

instance Data.ToQuery GetComponentPolicy where
  toQuery GetComponentPolicy' {..} =
    Prelude.mconcat
      ["componentArn" Data.=: componentArn]

-- | /See:/ 'newGetComponentPolicyResponse' smart constructor.
data GetComponentPolicyResponse = GetComponentPolicyResponse'
  { -- | The component policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
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
-- 'policy', 'getComponentPolicyResponse_policy' - The component policy.
--
-- 'requestId', 'getComponentPolicyResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'getComponentPolicyResponse_httpStatus' - The response's http status code.
newGetComponentPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComponentPolicyResponse
newGetComponentPolicyResponse pHttpStatus_ =
  GetComponentPolicyResponse'
    { policy =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The component policy.
getComponentPolicyResponse_policy :: Lens.Lens' GetComponentPolicyResponse (Prelude.Maybe Prelude.Text)
getComponentPolicyResponse_policy = Lens.lens (\GetComponentPolicyResponse' {policy} -> policy) (\s@GetComponentPolicyResponse' {} a -> s {policy = a} :: GetComponentPolicyResponse)

-- | The request ID that uniquely identifies this request.
getComponentPolicyResponse_requestId :: Lens.Lens' GetComponentPolicyResponse (Prelude.Maybe Prelude.Text)
getComponentPolicyResponse_requestId = Lens.lens (\GetComponentPolicyResponse' {requestId} -> requestId) (\s@GetComponentPolicyResponse' {} a -> s {requestId = a} :: GetComponentPolicyResponse)

-- | The response's http status code.
getComponentPolicyResponse_httpStatus :: Lens.Lens' GetComponentPolicyResponse Prelude.Int
getComponentPolicyResponse_httpStatus = Lens.lens (\GetComponentPolicyResponse' {httpStatus} -> httpStatus) (\s@GetComponentPolicyResponse' {} a -> s {httpStatus = a} :: GetComponentPolicyResponse)

instance Prelude.NFData GetComponentPolicyResponse where
  rnf GetComponentPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
