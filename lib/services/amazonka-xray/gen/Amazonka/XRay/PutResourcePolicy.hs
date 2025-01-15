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
-- Module      : Amazonka.XRay.PutResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the resource policy to grant one or more Amazon Web Services
-- services and accounts permissions to access X-Ray. Each resource policy
-- will be associated with a specific Amazon Web Services account. Each
-- Amazon Web Services account can have a maximum of 5 resource policies,
-- and each policy name must be unique within that account. The maximum
-- size of each resource policy is 5KB.
module Amazonka.XRay.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_bypassPolicyLockoutCheck,
    putResourcePolicy_policyRevisionId,
    putResourcePolicy_policyName,
    putResourcePolicy_policyDocument,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | A flag to indicate whether to bypass the resource policy lockout safety
    -- check.
    --
    -- Setting this value to true increases the risk that the policy becomes
    -- unmanageable. Do not set this value to true indiscriminately.
    --
    -- Use this parameter only when you include a policy in the request and you
    -- intend to prevent the principal that is making the request from making a
    -- subsequent @PutResourcePolicy@ request.
    --
    -- The default value is false.
    bypassPolicyLockoutCheck :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a specific policy revision, to ensure an atomic create
    -- operation. By default the resource policy is created if it does not
    -- exist, or updated with an incremented revision id. The revision id is
    -- unique to each policy in the account.
    --
    -- If the policy revision id does not match the latest revision id, the
    -- operation will fail with an @InvalidPolicyRevisionIdException@
    -- exception. You can also provide a @PolicyRevisionId@ of 0. In this case,
    -- the operation will fail with an @InvalidPolicyRevisionIdException@
    -- exception if a resource policy with the same name already exists.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource policy. Must be unique within a specific Amazon
    -- Web Services account.
    policyName :: Prelude.Text,
    -- | The resource policy document, which can be up to 5kb in size.
    policyDocument :: Prelude.Text
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
-- 'bypassPolicyLockoutCheck', 'putResourcePolicy_bypassPolicyLockoutCheck' - A flag to indicate whether to bypass the resource policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the policy becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- Use this parameter only when you include a policy in the request and you
-- intend to prevent the principal that is making the request from making a
-- subsequent @PutResourcePolicy@ request.
--
-- The default value is false.
--
-- 'policyRevisionId', 'putResourcePolicy_policyRevisionId' - Specifies a specific policy revision, to ensure an atomic create
-- operation. By default the resource policy is created if it does not
-- exist, or updated with an incremented revision id. The revision id is
-- unique to each policy in the account.
--
-- If the policy revision id does not match the latest revision id, the
-- operation will fail with an @InvalidPolicyRevisionIdException@
-- exception. You can also provide a @PolicyRevisionId@ of 0. In this case,
-- the operation will fail with an @InvalidPolicyRevisionIdException@
-- exception if a resource policy with the same name already exists.
--
-- 'policyName', 'putResourcePolicy_policyName' - The name of the resource policy. Must be unique within a specific Amazon
-- Web Services account.
--
-- 'policyDocument', 'putResourcePolicy_policyDocument' - The resource policy document, which can be up to 5kb in size.
newPutResourcePolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pPolicyName_ pPolicyDocument_ =
  PutResourcePolicy'
    { bypassPolicyLockoutCheck =
        Prelude.Nothing,
      policyRevisionId = Prelude.Nothing,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | A flag to indicate whether to bypass the resource policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the policy becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- Use this parameter only when you include a policy in the request and you
-- intend to prevent the principal that is making the request from making a
-- subsequent @PutResourcePolicy@ request.
--
-- The default value is false.
putResourcePolicy_bypassPolicyLockoutCheck :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Bool)
putResourcePolicy_bypassPolicyLockoutCheck = Lens.lens (\PutResourcePolicy' {bypassPolicyLockoutCheck} -> bypassPolicyLockoutCheck) (\s@PutResourcePolicy' {} a -> s {bypassPolicyLockoutCheck = a} :: PutResourcePolicy)

-- | Specifies a specific policy revision, to ensure an atomic create
-- operation. By default the resource policy is created if it does not
-- exist, or updated with an incremented revision id. The revision id is
-- unique to each policy in the account.
--
-- If the policy revision id does not match the latest revision id, the
-- operation will fail with an @InvalidPolicyRevisionIdException@
-- exception. You can also provide a @PolicyRevisionId@ of 0. In this case,
-- the operation will fail with an @InvalidPolicyRevisionIdException@
-- exception if a resource policy with the same name already exists.
putResourcePolicy_policyRevisionId :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyRevisionId = Lens.lens (\PutResourcePolicy' {policyRevisionId} -> policyRevisionId) (\s@PutResourcePolicy' {} a -> s {policyRevisionId = a} :: PutResourcePolicy)

-- | The name of the resource policy. Must be unique within a specific Amazon
-- Web Services account.
putResourcePolicy_policyName :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_policyName = Lens.lens (\PutResourcePolicy' {policyName} -> policyName) (\s@PutResourcePolicy' {} a -> s {policyName = a} :: PutResourcePolicy)

-- | The resource policy document, which can be up to 5kb in size.
putResourcePolicy_policyDocument :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_policyDocument = Lens.lens (\PutResourcePolicy' {policyDocument} -> policyDocument) (\s@PutResourcePolicy' {} a -> s {policyDocument = a} :: PutResourcePolicy)

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
            Prelude.<$> (x Data..?> "ResourcePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` bypassPolicyLockoutCheck
      `Prelude.hashWithSalt` policyRevisionId
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf bypassPolicyLockoutCheck `Prelude.seq`
      Prelude.rnf policyRevisionId `Prelude.seq`
        Prelude.rnf policyName `Prelude.seq`
          Prelude.rnf policyDocument

instance Data.ToHeaders PutResourcePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BypassPolicyLockoutCheck" Data..=)
              Prelude.<$> bypassPolicyLockoutCheck,
            ("PolicyRevisionId" Data..=)
              Prelude.<$> policyRevisionId,
            Prelude.Just ("PolicyName" Data..= policyName),
            Prelude.Just
              ("PolicyDocument" Data..= policyDocument)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/PutResourcePolicy"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The resource policy document, as provided in the
    -- @PutResourcePolicyRequest@.
    resourcePolicy :: Prelude.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'resourcePolicy', 'putResourcePolicyResponse_resourcePolicy' - The resource policy document, as provided in the
-- @PutResourcePolicyRequest@.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { resourcePolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource policy document, as provided in the
-- @PutResourcePolicyRequest@.
putResourcePolicyResponse_resourcePolicy :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe ResourcePolicy)
putResourcePolicyResponse_resourcePolicy = Lens.lens (\PutResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf resourcePolicy `Prelude.seq`
      Prelude.rnf httpStatus
