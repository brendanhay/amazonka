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
-- Module      : Amazonka.Comprehend.DescribeResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a resource-based policy that is attached to a custom
-- model, including the JSON body of the policy.
module Amazonka.Comprehend.DescribeResourcePolicy
  ( -- * Creating a Request
    DescribeResourcePolicy (..),
    newDescribeResourcePolicy,

    -- * Request Lenses
    describeResourcePolicy_resourceArn,

    -- * Destructuring the Response
    DescribeResourcePolicyResponse (..),
    newDescribeResourcePolicyResponse,

    -- * Response Lenses
    describeResourcePolicyResponse_creationTime,
    describeResourcePolicyResponse_lastModifiedTime,
    describeResourcePolicyResponse_policyRevisionId,
    describeResourcePolicyResponse_resourcePolicy,
    describeResourcePolicyResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeResourcePolicy' smart constructor.
data DescribeResourcePolicy = DescribeResourcePolicy'
  { -- | The Amazon Resource Name (ARN) of the custom model version that has the
    -- resource policy.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'describeResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the custom model version that has the
-- resource policy.
newDescribeResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  DescribeResourcePolicy
newDescribeResourcePolicy pResourceArn_ =
  DescribeResourcePolicy'
    { resourceArn =
        pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the custom model version that has the
-- resource policy.
describeResourcePolicy_resourceArn :: Lens.Lens' DescribeResourcePolicy Prelude.Text
describeResourcePolicy_resourceArn = Lens.lens (\DescribeResourcePolicy' {resourceArn} -> resourceArn) (\s@DescribeResourcePolicy' {} a -> s {resourceArn = a} :: DescribeResourcePolicy)

instance Core.AWSRequest DescribeResourcePolicy where
  type
    AWSResponse DescribeResourcePolicy =
      DescribeResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePolicyResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "PolicyRevisionId")
            Prelude.<*> (x Data..?> "ResourcePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResourcePolicy where
  hashWithSalt _salt DescribeResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DescribeResourcePolicy where
  rnf DescribeResourcePolicy' {..} =
    Prelude.rnf resourceArn

instance Data.ToHeaders DescribeResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DescribeResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeResourcePolicy where
  toJSON DescribeResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath DescribeResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourcePolicyResponse' smart constructor.
data DescribeResourcePolicyResponse = DescribeResourcePolicyResponse'
  { -- | The time at which the policy was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time at which the policy was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The revision ID of the policy. Each time you modify a policy, Amazon
    -- Comprehend assigns a new revision ID, and it deletes the prior version
    -- of the policy.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The JSON body of the resource-based policy.
    resourcePolicy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeResourcePolicyResponse_creationTime' - The time at which the policy was created.
--
-- 'lastModifiedTime', 'describeResourcePolicyResponse_lastModifiedTime' - The time at which the policy was last modified.
--
-- 'policyRevisionId', 'describeResourcePolicyResponse_policyRevisionId' - The revision ID of the policy. Each time you modify a policy, Amazon
-- Comprehend assigns a new revision ID, and it deletes the prior version
-- of the policy.
--
-- 'resourcePolicy', 'describeResourcePolicyResponse_resourcePolicy' - The JSON body of the resource-based policy.
--
-- 'httpStatus', 'describeResourcePolicyResponse_httpStatus' - The response's http status code.
newDescribeResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourcePolicyResponse
newDescribeResourcePolicyResponse pHttpStatus_ =
  DescribeResourcePolicyResponse'
    { creationTime =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      policyRevisionId = Prelude.Nothing,
      resourcePolicy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the policy was created.
describeResourcePolicyResponse_creationTime :: Lens.Lens' DescribeResourcePolicyResponse (Prelude.Maybe Prelude.UTCTime)
describeResourcePolicyResponse_creationTime = Lens.lens (\DescribeResourcePolicyResponse' {creationTime} -> creationTime) (\s@DescribeResourcePolicyResponse' {} a -> s {creationTime = a} :: DescribeResourcePolicyResponse) Prelude.. Lens.mapping Data._Time

-- | The time at which the policy was last modified.
describeResourcePolicyResponse_lastModifiedTime :: Lens.Lens' DescribeResourcePolicyResponse (Prelude.Maybe Prelude.UTCTime)
describeResourcePolicyResponse_lastModifiedTime = Lens.lens (\DescribeResourcePolicyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeResourcePolicyResponse' {} a -> s {lastModifiedTime = a} :: DescribeResourcePolicyResponse) Prelude.. Lens.mapping Data._Time

-- | The revision ID of the policy. Each time you modify a policy, Amazon
-- Comprehend assigns a new revision ID, and it deletes the prior version
-- of the policy.
describeResourcePolicyResponse_policyRevisionId :: Lens.Lens' DescribeResourcePolicyResponse (Prelude.Maybe Prelude.Text)
describeResourcePolicyResponse_policyRevisionId = Lens.lens (\DescribeResourcePolicyResponse' {policyRevisionId} -> policyRevisionId) (\s@DescribeResourcePolicyResponse' {} a -> s {policyRevisionId = a} :: DescribeResourcePolicyResponse)

-- | The JSON body of the resource-based policy.
describeResourcePolicyResponse_resourcePolicy :: Lens.Lens' DescribeResourcePolicyResponse (Prelude.Maybe Prelude.Text)
describeResourcePolicyResponse_resourcePolicy = Lens.lens (\DescribeResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@DescribeResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: DescribeResourcePolicyResponse)

-- | The response's http status code.
describeResourcePolicyResponse_httpStatus :: Lens.Lens' DescribeResourcePolicyResponse Prelude.Int
describeResourcePolicyResponse_httpStatus = Lens.lens (\DescribeResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePolicyResponse' {} a -> s {httpStatus = a} :: DescribeResourcePolicyResponse)

instance
  Prelude.NFData
    DescribeResourcePolicyResponse
  where
  rnf DescribeResourcePolicyResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf resourcePolicy
      `Prelude.seq` Prelude.rnf httpStatus
