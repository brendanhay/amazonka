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
-- Module      : Amazonka.Glue.PutResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Data Catalog resource policy for access control.
module Amazonka.Glue.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_enableHybrid,
    putResourcePolicy_policyExistsCondition,
    putResourcePolicy_policyHashCondition,
    putResourcePolicy_resourceArn,
    putResourcePolicy_policyInJson,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_policyHash,
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | If @\'TRUE\'@, indicates that you are using both methods to grant
    -- cross-account access to Data Catalog resources:
    --
    -- -   By directly updating the resource policy with @PutResourePolicy@
    --
    -- -   By using the __Grant permissions__ command on the Amazon Web
    --     Services Management Console.
    --
    -- Must be set to @\'TRUE\'@ if you have already used the Management
    -- Console to grant cross-account access, otherwise the call fails. Default
    -- is \'FALSE\'.
    enableHybrid :: Prelude.Maybe EnableHybridValues,
    -- | A value of @MUST_EXIST@ is used to update a policy. A value of
    -- @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a
    -- null value is used, the call does not depend on the existence of a
    -- policy.
    policyExistsCondition :: Prelude.Maybe ExistCondition,
    -- | The hash value returned when the previous policy was set using
    -- @PutResourcePolicy@. Its purpose is to prevent concurrent modifications
    -- of a policy. Do not use this parameter if no previous policy has been
    -- set.
    policyHashCondition :: Prelude.Maybe Prelude.Text,
    -- | Do not use. For internal use only.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Contains the policy document to set, in JSON format.
    policyInJson :: Prelude.Text
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
-- 'enableHybrid', 'putResourcePolicy_enableHybrid' - If @\'TRUE\'@, indicates that you are using both methods to grant
-- cross-account access to Data Catalog resources:
--
-- -   By directly updating the resource policy with @PutResourePolicy@
--
-- -   By using the __Grant permissions__ command on the Amazon Web
--     Services Management Console.
--
-- Must be set to @\'TRUE\'@ if you have already used the Management
-- Console to grant cross-account access, otherwise the call fails. Default
-- is \'FALSE\'.
--
-- 'policyExistsCondition', 'putResourcePolicy_policyExistsCondition' - A value of @MUST_EXIST@ is used to update a policy. A value of
-- @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a
-- null value is used, the call does not depend on the existence of a
-- policy.
--
-- 'policyHashCondition', 'putResourcePolicy_policyHashCondition' - The hash value returned when the previous policy was set using
-- @PutResourcePolicy@. Its purpose is to prevent concurrent modifications
-- of a policy. Do not use this parameter if no previous policy has been
-- set.
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - Do not use. For internal use only.
--
-- 'policyInJson', 'putResourcePolicy_policyInJson' - Contains the policy document to set, in JSON format.
newPutResourcePolicy ::
  -- | 'policyInJson'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pPolicyInJson_ =
  PutResourcePolicy'
    { enableHybrid = Prelude.Nothing,
      policyExistsCondition = Prelude.Nothing,
      policyHashCondition = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      policyInJson = pPolicyInJson_
    }

-- | If @\'TRUE\'@, indicates that you are using both methods to grant
-- cross-account access to Data Catalog resources:
--
-- -   By directly updating the resource policy with @PutResourePolicy@
--
-- -   By using the __Grant permissions__ command on the Amazon Web
--     Services Management Console.
--
-- Must be set to @\'TRUE\'@ if you have already used the Management
-- Console to grant cross-account access, otherwise the call fails. Default
-- is \'FALSE\'.
putResourcePolicy_enableHybrid :: Lens.Lens' PutResourcePolicy (Prelude.Maybe EnableHybridValues)
putResourcePolicy_enableHybrid = Lens.lens (\PutResourcePolicy' {enableHybrid} -> enableHybrid) (\s@PutResourcePolicy' {} a -> s {enableHybrid = a} :: PutResourcePolicy)

-- | A value of @MUST_EXIST@ is used to update a policy. A value of
-- @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a
-- null value is used, the call does not depend on the existence of a
-- policy.
putResourcePolicy_policyExistsCondition :: Lens.Lens' PutResourcePolicy (Prelude.Maybe ExistCondition)
putResourcePolicy_policyExistsCondition = Lens.lens (\PutResourcePolicy' {policyExistsCondition} -> policyExistsCondition) (\s@PutResourcePolicy' {} a -> s {policyExistsCondition = a} :: PutResourcePolicy)

-- | The hash value returned when the previous policy was set using
-- @PutResourcePolicy@. Its purpose is to prevent concurrent modifications
-- of a policy. Do not use this parameter if no previous policy has been
-- set.
putResourcePolicy_policyHashCondition :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyHashCondition = Lens.lens (\PutResourcePolicy' {policyHashCondition} -> policyHashCondition) (\s@PutResourcePolicy' {} a -> s {policyHashCondition = a} :: PutResourcePolicy)

-- | Do not use. For internal use only.
putResourcePolicy_resourceArn :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_resourceArn = Lens.lens (\PutResourcePolicy' {resourceArn} -> resourceArn) (\s@PutResourcePolicy' {} a -> s {resourceArn = a} :: PutResourcePolicy)

-- | Contains the policy document to set, in JSON format.
putResourcePolicy_policyInJson :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_policyInJson = Lens.lens (\PutResourcePolicy' {policyInJson} -> policyInJson) (\s@PutResourcePolicy' {} a -> s {policyInJson = a} :: PutResourcePolicy)

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
            Prelude.<$> (x Data..?> "PolicyHash")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` enableHybrid
      `Prelude.hashWithSalt` policyExistsCondition
      `Prelude.hashWithSalt` policyHashCondition
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` policyInJson

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf enableHybrid `Prelude.seq`
      Prelude.rnf policyExistsCondition `Prelude.seq`
        Prelude.rnf policyHashCondition `Prelude.seq`
          Prelude.rnf resourceArn `Prelude.seq`
            Prelude.rnf policyInJson

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.PutResourcePolicy" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableHybrid" Data..=) Prelude.<$> enableHybrid,
            ("PolicyExistsCondition" Data..=)
              Prelude.<$> policyExistsCondition,
            ("PolicyHashCondition" Data..=)
              Prelude.<$> policyHashCondition,
            ("ResourceArn" Data..=) Prelude.<$> resourceArn,
            Prelude.Just ("PolicyInJson" Data..= policyInJson)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | A hash of the policy that has just been set. This must be included in a
    -- subsequent call that overwrites or updates this policy.
    policyHash :: Prelude.Maybe Prelude.Text,
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
-- 'policyHash', 'putResourcePolicyResponse_policyHash' - A hash of the policy that has just been set. This must be included in a
-- subsequent call that overwrites or updates this policy.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { policyHash =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A hash of the policy that has just been set. This must be included in a
-- subsequent call that overwrites or updates this policy.
putResourcePolicyResponse_policyHash :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_policyHash = Lens.lens (\PutResourcePolicyResponse' {policyHash} -> policyHash) (\s@PutResourcePolicyResponse' {} a -> s {policyHash = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf policyHash `Prelude.seq`
      Prelude.rnf httpStatus
