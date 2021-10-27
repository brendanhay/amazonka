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
-- Module      : Network.AWS.LexV2Models.DescribeResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the resource policy and policy revision for a bot or bot alias.
module Network.AWS.LexV2Models.DescribeResourcePolicy
  ( -- * Creating a Request
    DescribeResourcePolicy (..),
    newDescribeResourcePolicy,

    -- * Request Lenses
    describeResourcePolicy_resourceArn,

    -- * Destructuring the Response
    DescribeResourcePolicyResponse (..),
    newDescribeResourcePolicyResponse,

    -- * Response Lenses
    describeResourcePolicyResponse_resourceArn,
    describeResourcePolicyResponse_policy,
    describeResourcePolicyResponse_revisionId,
    describeResourcePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeResourcePolicy' smart constructor.
data DescribeResourcePolicy = DescribeResourcePolicy'
  { -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy is attached to.
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
-- 'resourceArn', 'describeResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
newDescribeResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  DescribeResourcePolicy
newDescribeResourcePolicy pResourceArn_ =
  DescribeResourcePolicy'
    { resourceArn =
        pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
describeResourcePolicy_resourceArn :: Lens.Lens' DescribeResourcePolicy Prelude.Text
describeResourcePolicy_resourceArn = Lens.lens (\DescribeResourcePolicy' {resourceArn} -> resourceArn) (\s@DescribeResourcePolicy' {} a -> s {resourceArn = a} :: DescribeResourcePolicy)

instance Core.AWSRequest DescribeResourcePolicy where
  type
    AWSResponse DescribeResourcePolicy =
      DescribeResourcePolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePolicyResponse'
            Prelude.<$> (x Core..?> "resourceArn")
            Prelude.<*> (x Core..?> "policy")
            Prelude.<*> (x Core..?> "revisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResourcePolicy

instance Prelude.NFData DescribeResourcePolicy

instance Core.ToHeaders DescribeResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeResourcePolicy where
  toPath DescribeResourcePolicy' {..} =
    Prelude.mconcat
      ["/policy/", Core.toBS resourceArn, "/"]

instance Core.ToQuery DescribeResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourcePolicyResponse' smart constructor.
data DescribeResourcePolicyResponse = DescribeResourcePolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy is attached to.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON structure that contains the resource policy. For more
    -- information about the contents of a JSON policy document, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>
    -- .
    policy :: Prelude.Maybe Prelude.Text,
    -- | The current revision of the resource policy. Use the revision ID to make
    -- sure that you are updating the most current version of a resource policy
    -- when you add a policy statement to a resource, delete a resource, or
    -- update a resource.
    revisionId :: Prelude.Maybe Prelude.Text,
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
-- 'resourceArn', 'describeResourcePolicyResponse_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
--
-- 'policy', 'describeResourcePolicyResponse_policy' - The JSON structure that contains the resource policy. For more
-- information about the contents of a JSON policy document, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>
-- .
--
-- 'revisionId', 'describeResourcePolicyResponse_revisionId' - The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
--
-- 'httpStatus', 'describeResourcePolicyResponse_httpStatus' - The response's http status code.
newDescribeResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourcePolicyResponse
newDescribeResourcePolicyResponse pHttpStatus_ =
  DescribeResourcePolicyResponse'
    { resourceArn =
        Prelude.Nothing,
      policy = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
describeResourcePolicyResponse_resourceArn :: Lens.Lens' DescribeResourcePolicyResponse (Prelude.Maybe Prelude.Text)
describeResourcePolicyResponse_resourceArn = Lens.lens (\DescribeResourcePolicyResponse' {resourceArn} -> resourceArn) (\s@DescribeResourcePolicyResponse' {} a -> s {resourceArn = a} :: DescribeResourcePolicyResponse)

-- | The JSON structure that contains the resource policy. For more
-- information about the contents of a JSON policy document, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>
-- .
describeResourcePolicyResponse_policy :: Lens.Lens' DescribeResourcePolicyResponse (Prelude.Maybe Prelude.Text)
describeResourcePolicyResponse_policy = Lens.lens (\DescribeResourcePolicyResponse' {policy} -> policy) (\s@DescribeResourcePolicyResponse' {} a -> s {policy = a} :: DescribeResourcePolicyResponse)

-- | The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
describeResourcePolicyResponse_revisionId :: Lens.Lens' DescribeResourcePolicyResponse (Prelude.Maybe Prelude.Text)
describeResourcePolicyResponse_revisionId = Lens.lens (\DescribeResourcePolicyResponse' {revisionId} -> revisionId) (\s@DescribeResourcePolicyResponse' {} a -> s {revisionId = a} :: DescribeResourcePolicyResponse)

-- | The response's http status code.
describeResourcePolicyResponse_httpStatus :: Lens.Lens' DescribeResourcePolicyResponse Prelude.Int
describeResourcePolicyResponse_httpStatus = Lens.lens (\DescribeResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePolicyResponse' {} a -> s {httpStatus = a} :: DescribeResourcePolicyResponse)

instance
  Prelude.NFData
    DescribeResourcePolicyResponse
