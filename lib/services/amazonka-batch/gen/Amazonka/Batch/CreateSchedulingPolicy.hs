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
-- Module      : Amazonka.Batch.CreateSchedulingPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Batch scheduling policy.
module Amazonka.Batch.CreateSchedulingPolicy
  ( -- * Creating a Request
    CreateSchedulingPolicy (..),
    newCreateSchedulingPolicy,

    -- * Request Lenses
    createSchedulingPolicy_tags,
    createSchedulingPolicy_fairsharePolicy,
    createSchedulingPolicy_name,

    -- * Destructuring the Response
    CreateSchedulingPolicyResponse (..),
    newCreateSchedulingPolicyResponse,

    -- * Response Lenses
    createSchedulingPolicyResponse_httpStatus,
    createSchedulingPolicyResponse_name,
    createSchedulingPolicyResponse_arn,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @CreateSchedulingPolicy@.
--
-- /See:/ 'newCreateSchedulingPolicy' smart constructor.
data CreateSchedulingPolicy = CreateSchedulingPolicy'
  { -- | The tags that you apply to the scheduling policy to help you categorize
    -- and organize your resources. Each tag consists of a key and an optional
    -- value. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in /Amazon Web Services General Reference/.
    --
    -- These tags can be updated or removed using the
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
    -- and
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
    -- API operations.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The fair share policy of the scheduling policy.
    fairsharePolicy :: Prelude.Maybe FairsharePolicy,
    -- | The name of the scheduling policy. It can be up to 128 letters long. It
    -- can contain uppercase and lowercase letters, numbers, hyphens (-), and
    -- underscores (_).
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSchedulingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSchedulingPolicy_tags' - The tags that you apply to the scheduling policy to help you categorize
-- and organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in /Amazon Web Services General Reference/.
--
-- These tags can be updated or removed using the
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
-- and
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
-- API operations.
--
-- 'fairsharePolicy', 'createSchedulingPolicy_fairsharePolicy' - The fair share policy of the scheduling policy.
--
-- 'name', 'createSchedulingPolicy_name' - The name of the scheduling policy. It can be up to 128 letters long. It
-- can contain uppercase and lowercase letters, numbers, hyphens (-), and
-- underscores (_).
newCreateSchedulingPolicy ::
  -- | 'name'
  Prelude.Text ->
  CreateSchedulingPolicy
newCreateSchedulingPolicy pName_ =
  CreateSchedulingPolicy'
    { tags = Prelude.Nothing,
      fairsharePolicy = Prelude.Nothing,
      name = pName_
    }

-- | The tags that you apply to the scheduling policy to help you categorize
-- and organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in /Amazon Web Services General Reference/.
--
-- These tags can be updated or removed using the
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
-- and
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
-- API operations.
createSchedulingPolicy_tags :: Lens.Lens' CreateSchedulingPolicy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSchedulingPolicy_tags = Lens.lens (\CreateSchedulingPolicy' {tags} -> tags) (\s@CreateSchedulingPolicy' {} a -> s {tags = a} :: CreateSchedulingPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The fair share policy of the scheduling policy.
createSchedulingPolicy_fairsharePolicy :: Lens.Lens' CreateSchedulingPolicy (Prelude.Maybe FairsharePolicy)
createSchedulingPolicy_fairsharePolicy = Lens.lens (\CreateSchedulingPolicy' {fairsharePolicy} -> fairsharePolicy) (\s@CreateSchedulingPolicy' {} a -> s {fairsharePolicy = a} :: CreateSchedulingPolicy)

-- | The name of the scheduling policy. It can be up to 128 letters long. It
-- can contain uppercase and lowercase letters, numbers, hyphens (-), and
-- underscores (_).
createSchedulingPolicy_name :: Lens.Lens' CreateSchedulingPolicy Prelude.Text
createSchedulingPolicy_name = Lens.lens (\CreateSchedulingPolicy' {name} -> name) (\s@CreateSchedulingPolicy' {} a -> s {name = a} :: CreateSchedulingPolicy)

instance Core.AWSRequest CreateSchedulingPolicy where
  type
    AWSResponse CreateSchedulingPolicy =
      CreateSchedulingPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSchedulingPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable CreateSchedulingPolicy where
  hashWithSalt _salt CreateSchedulingPolicy' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` fairsharePolicy
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSchedulingPolicy where
  rnf CreateSchedulingPolicy' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fairsharePolicy
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateSchedulingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSchedulingPolicy where
  toJSON CreateSchedulingPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("fairsharePolicy" Data..=)
              Prelude.<$> fairsharePolicy,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateSchedulingPolicy where
  toPath = Prelude.const "/v1/createschedulingpolicy"

instance Data.ToQuery CreateSchedulingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSchedulingPolicyResponse' smart constructor.
data CreateSchedulingPolicyResponse = CreateSchedulingPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the scheduling policy.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the scheduling policy. The format is
    -- @aws:Partition:batch:Region:Account:scheduling-policy\/Name @. For
    -- example,
    -- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSchedulingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSchedulingPolicyResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createSchedulingPolicyResponse_name' - The name of the scheduling policy.
--
-- 'arn', 'createSchedulingPolicyResponse_arn' - The Amazon Resource Name (ARN) of the scheduling policy. The format is
-- @aws:Partition:batch:Region:Account:scheduling-policy\/Name @. For
-- example,
-- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
newCreateSchedulingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateSchedulingPolicyResponse
newCreateSchedulingPolicyResponse
  pHttpStatus_
  pName_
  pArn_ =
    CreateSchedulingPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
createSchedulingPolicyResponse_httpStatus :: Lens.Lens' CreateSchedulingPolicyResponse Prelude.Int
createSchedulingPolicyResponse_httpStatus = Lens.lens (\CreateSchedulingPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateSchedulingPolicyResponse' {} a -> s {httpStatus = a} :: CreateSchedulingPolicyResponse)

-- | The name of the scheduling policy.
createSchedulingPolicyResponse_name :: Lens.Lens' CreateSchedulingPolicyResponse Prelude.Text
createSchedulingPolicyResponse_name = Lens.lens (\CreateSchedulingPolicyResponse' {name} -> name) (\s@CreateSchedulingPolicyResponse' {} a -> s {name = a} :: CreateSchedulingPolicyResponse)

-- | The Amazon Resource Name (ARN) of the scheduling policy. The format is
-- @aws:Partition:batch:Region:Account:scheduling-policy\/Name @. For
-- example,
-- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
createSchedulingPolicyResponse_arn :: Lens.Lens' CreateSchedulingPolicyResponse Prelude.Text
createSchedulingPolicyResponse_arn = Lens.lens (\CreateSchedulingPolicyResponse' {arn} -> arn) (\s@CreateSchedulingPolicyResponse' {} a -> s {arn = a} :: CreateSchedulingPolicyResponse)

instance
  Prelude.NFData
    CreateSchedulingPolicyResponse
  where
  rnf CreateSchedulingPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
