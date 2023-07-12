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
-- Module      : Amazonka.FMS.GetPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Firewall Manager policy.
module Amazonka.FMS.GetPolicy
  ( -- * Creating a Request
    GetPolicy (..),
    newGetPolicy,

    -- * Request Lenses
    getPolicy_policyId,

    -- * Destructuring the Response
    GetPolicyResponse (..),
    newGetPolicyResponse,

    -- * Response Lenses
    getPolicyResponse_policy,
    getPolicyResponse_policyArn,
    getPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { -- | The ID of the Firewall Manager policy that you want the details for.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'getPolicy_policyId' - The ID of the Firewall Manager policy that you want the details for.
newGetPolicy ::
  -- | 'policyId'
  Prelude.Text ->
  GetPolicy
newGetPolicy pPolicyId_ =
  GetPolicy' {policyId = pPolicyId_}

-- | The ID of the Firewall Manager policy that you want the details for.
getPolicy_policyId :: Lens.Lens' GetPolicy Prelude.Text
getPolicy_policyId = Lens.lens (\GetPolicy' {policyId} -> policyId) (\s@GetPolicy' {} a -> s {policyId = a} :: GetPolicy)

instance Core.AWSRequest GetPolicy where
  type AWSResponse GetPolicy = GetPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (x Data..?> "PolicyArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPolicy where
  hashWithSalt _salt GetPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyId

instance Prelude.NFData GetPolicy where
  rnf GetPolicy' {..} = Prelude.rnf policyId

instance Data.ToHeaders GetPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSFMS_20180101.GetPolicy" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPolicy where
  toJSON GetPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PolicyId" Data..= policyId)]
      )

instance Data.ToPath GetPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | Information about the specified Firewall Manager policy.
    policy :: Prelude.Maybe Policy,
    -- | The Amazon Resource Name (ARN) of the specified policy.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getPolicyResponse_policy' - Information about the specified Firewall Manager policy.
--
-- 'policyArn', 'getPolicyResponse_policyArn' - The Amazon Resource Name (ARN) of the specified policy.
--
-- 'httpStatus', 'getPolicyResponse_httpStatus' - The response's http status code.
newGetPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPolicyResponse
newGetPolicyResponse pHttpStatus_ =
  GetPolicyResponse'
    { policy = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified Firewall Manager policy.
getPolicyResponse_policy :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Policy)
getPolicyResponse_policy = Lens.lens (\GetPolicyResponse' {policy} -> policy) (\s@GetPolicyResponse' {} a -> s {policy = a} :: GetPolicyResponse)

-- | The Amazon Resource Name (ARN) of the specified policy.
getPolicyResponse_policyArn :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.Text)
getPolicyResponse_policyArn = Lens.lens (\GetPolicyResponse' {policyArn} -> policyArn) (\s@GetPolicyResponse' {} a -> s {policyArn = a} :: GetPolicyResponse)

-- | The response's http status code.
getPolicyResponse_httpStatus :: Lens.Lens' GetPolicyResponse Prelude.Int
getPolicyResponse_httpStatus = Lens.lens (\GetPolicyResponse' {httpStatus} -> httpStatus) (\s@GetPolicyResponse' {} a -> s {httpStatus = a} :: GetPolicyResponse)

instance Prelude.NFData GetPolicyResponse where
  rnf GetPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf httpStatus
