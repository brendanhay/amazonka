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
-- Module      : Amazonka.RedshiftServerLess.PutResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a resource policy. Currently, you can use policies to
-- share snapshots across Amazon Web Services accounts.
module Amazonka.RedshiftServerLess.PutResourcePolicy
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
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | The policy to create or update. For example, the following policy grants
    -- a user authorization to restore a snapshot.
    --
    -- @\"{\\\"Version\\\": \\\"2012-10-17\\\", \\\"Statement\\\" : [{ \\\"Sid\\\": \\\"AllowUserRestoreFromSnapshot\\\", \\\"Principal\\\":{\\\"AWS\\\": [\\\"739247239426\\\"]}, \\\"Action\\\": [\\\"redshift-serverless:RestoreFromSnapshot\\\"] , \\\"Effect\\\": \\\"Allow\\\" }]}\"@
    policy :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the account to create or update a
    -- resource policy for.
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
-- 'policy', 'putResourcePolicy_policy' - The policy to create or update. For example, the following policy grants
-- a user authorization to restore a snapshot.
--
-- @\"{\\\"Version\\\": \\\"2012-10-17\\\", \\\"Statement\\\" : [{ \\\"Sid\\\": \\\"AllowUserRestoreFromSnapshot\\\", \\\"Principal\\\":{\\\"AWS\\\": [\\\"739247239426\\\"]}, \\\"Action\\\": [\\\"redshift-serverless:RestoreFromSnapshot\\\"] , \\\"Effect\\\": \\\"Allow\\\" }]}\"@
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the account to create or update a
-- resource policy for.
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

-- | The policy to create or update. For example, the following policy grants
-- a user authorization to restore a snapshot.
--
-- @\"{\\\"Version\\\": \\\"2012-10-17\\\", \\\"Statement\\\" : [{ \\\"Sid\\\": \\\"AllowUserRestoreFromSnapshot\\\", \\\"Principal\\\":{\\\"AWS\\\": [\\\"739247239426\\\"]}, \\\"Action\\\": [\\\"redshift-serverless:RestoreFromSnapshot\\\"] , \\\"Effect\\\": \\\"Allow\\\" }]}\"@
putResourcePolicy_policy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_policy = Lens.lens (\PutResourcePolicy' {policy} -> policy) (\s@PutResourcePolicy' {} a -> s {policy = a} :: PutResourcePolicy)

-- | The Amazon Resource Name (ARN) of the account to create or update a
-- resource policy for.
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
            Prelude.<$> (x Data..?> "resourcePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.PutResourcePolicy" ::
                          Prelude.ByteString
                      ),
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
          [ Prelude.Just ("policy" Data..= policy),
            Prelude.Just ("resourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The policy that was created or updated.
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
-- 'resourcePolicy', 'putResourcePolicyResponse_resourcePolicy' - The policy that was created or updated.
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

-- | The policy that was created or updated.
putResourcePolicyResponse_resourcePolicy :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe ResourcePolicy)
putResourcePolicyResponse_resourcePolicy = Lens.lens (\PutResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf resourcePolicy
      `Prelude.seq` Prelude.rnf httpStatus
