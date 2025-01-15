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
-- Module      : Amazonka.SSMIncidents.PutResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a resource policy to the specified response plan. The resource
-- policy is used to share the response plan using Resource Access Manager
-- (RAM). For more information about cross-account sharing, see
-- <https://docs.aws.amazon.com/incident-manager/latest/userguide/incident-manager-cross-account-cross-region.html Cross-Region and cross-account incident management>.
module Amazonka.SSMIncidents.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_httpStatus,
    putResourcePolicyResponse_policyId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | Details of the resource policy.
    policy :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the response plan to add the resource
    -- policy to.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putResourcePolicy_policy' - Details of the resource policy.
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the response plan to add the resource
-- policy to.
newPutResourcePolicy ::
  -- | 'policy'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pPolicy_ pResourceArn_ =
  PutResourcePolicy'
    { policy = pPolicy_,
      resourceArn = pResourceArn_
    }

-- | Details of the resource policy.
putResourcePolicy_policy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_policy = Lens.lens (\PutResourcePolicy' {policy} -> policy) (\s@PutResourcePolicy' {} a -> s {policy = a} :: PutResourcePolicy)

-- | The Amazon Resource Name (ARN) of the response plan to add the resource
-- policy to.
putResourcePolicy_resourceArn :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourceArn = Lens.lens (\PutResourcePolicy' {resourceArn} -> resourceArn) (\s@PutResourcePolicy' {} a -> s {resourceArn = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyId")
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf policy `Prelude.seq`
      Prelude.rnf resourceArn

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("policy" Data..= policy),
            Prelude.Just ("resourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/putResourcePolicy"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the resource policy.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
--
-- 'policyId', 'putResourcePolicyResponse_policyId' - The ID of the resource policy.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyId'
  Prelude.Text ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ pPolicyId_ =
  PutResourcePolicyResponse'
    { httpStatus =
        pHttpStatus_,
      policyId = pPolicyId_
    }

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

-- | The ID of the resource policy.
putResourcePolicyResponse_policyId :: Lens.Lens' PutResourcePolicyResponse Prelude.Text
putResourcePolicyResponse_policyId = Lens.lens (\PutResourcePolicyResponse' {policyId} -> policyId) (\s@PutResourcePolicyResponse' {} a -> s {policyId = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf policyId
