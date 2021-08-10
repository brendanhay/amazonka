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
-- Module      : Network.AWS.Glacier.GetDataRetrievalPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current data retrieval policy for the account
-- and region specified in the GET request. For more information about data
-- retrieval policies, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies>.
module Network.AWS.Glacier.GetDataRetrievalPolicy
  ( -- * Creating a Request
    GetDataRetrievalPolicy (..),
    newGetDataRetrievalPolicy,

    -- * Request Lenses
    getDataRetrievalPolicy_accountId,

    -- * Destructuring the Response
    GetDataRetrievalPolicyResponse (..),
    newGetDataRetrievalPolicyResponse,

    -- * Response Lenses
    getDataRetrievalPolicyResponse_policy,
    getDataRetrievalPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input for GetDataRetrievalPolicy.
--
-- /See:/ 'newGetDataRetrievalPolicy' smart constructor.
data GetDataRetrievalPolicy = GetDataRetrievalPolicy'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the
    -- AWS account ID associated with the credentials used to sign the request.
    -- You can either specify an AWS account ID or optionally a single \'@-@\'
    -- (hyphen), in which case Amazon Glacier uses the AWS account ID
    -- associated with the credentials used to sign the request. If you specify
    -- your account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataRetrievalPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getDataRetrievalPolicy_accountId' - The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
newGetDataRetrievalPolicy ::
  -- | 'accountId'
  Prelude.Text ->
  GetDataRetrievalPolicy
newGetDataRetrievalPolicy pAccountId_ =
  GetDataRetrievalPolicy' {accountId = pAccountId_}

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
getDataRetrievalPolicy_accountId :: Lens.Lens' GetDataRetrievalPolicy Prelude.Text
getDataRetrievalPolicy_accountId = Lens.lens (\GetDataRetrievalPolicy' {accountId} -> accountId) (\s@GetDataRetrievalPolicy' {} a -> s {accountId = a} :: GetDataRetrievalPolicy)

instance Core.AWSRequest GetDataRetrievalPolicy where
  type
    AWSResponse GetDataRetrievalPolicy =
      GetDataRetrievalPolicyResponse
  request =
    Request.glacierVersionHeader (Core._serviceVersion defaultService)
      Prelude.. Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataRetrievalPolicyResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataRetrievalPolicy

instance Prelude.NFData GetDataRetrievalPolicy

instance Core.ToHeaders GetDataRetrievalPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetDataRetrievalPolicy where
  toPath GetDataRetrievalPolicy' {..} =
    Prelude.mconcat
      [ "/",
        Core.toBS accountId,
        "/policies/data-retrieval"
      ]

instance Core.ToQuery GetDataRetrievalPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the Amazon S3 Glacier response to the @GetDataRetrievalPolicy@
-- request.
--
-- /See:/ 'newGetDataRetrievalPolicyResponse' smart constructor.
data GetDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse'
  { -- | Contains the returned data retrieval policy in JSON format.
    policy :: Prelude.Maybe DataRetrievalPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataRetrievalPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getDataRetrievalPolicyResponse_policy' - Contains the returned data retrieval policy in JSON format.
--
-- 'httpStatus', 'getDataRetrievalPolicyResponse_httpStatus' - The response's http status code.
newGetDataRetrievalPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataRetrievalPolicyResponse
newGetDataRetrievalPolicyResponse pHttpStatus_ =
  GetDataRetrievalPolicyResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the returned data retrieval policy in JSON format.
getDataRetrievalPolicyResponse_policy :: Lens.Lens' GetDataRetrievalPolicyResponse (Prelude.Maybe DataRetrievalPolicy)
getDataRetrievalPolicyResponse_policy = Lens.lens (\GetDataRetrievalPolicyResponse' {policy} -> policy) (\s@GetDataRetrievalPolicyResponse' {} a -> s {policy = a} :: GetDataRetrievalPolicyResponse)

-- | The response's http status code.
getDataRetrievalPolicyResponse_httpStatus :: Lens.Lens' GetDataRetrievalPolicyResponse Prelude.Int
getDataRetrievalPolicyResponse_httpStatus = Lens.lens (\GetDataRetrievalPolicyResponse' {httpStatus} -> httpStatus) (\s@GetDataRetrievalPolicyResponse' {} a -> s {httpStatus = a} :: GetDataRetrievalPolicyResponse)

instance
  Prelude.NFData
    GetDataRetrievalPolicyResponse
