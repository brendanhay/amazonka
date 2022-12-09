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
-- Module      : Amazonka.Organizations.PutResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a resource policy.
--
-- You can only call this operation from the organization\'s management
-- account.
module Amazonka.Organizations.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_tags,
    putResourcePolicy_content,

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
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | Updates the list of tags that you want to attach to the newly-created
    -- resource policy. For each tag in the list, you must specify both a tag
    -- key and a value. You can set the value to an empty string, but you
    -- can\'t set it to @null@. For more information about tagging, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
    -- in the Organizations User Guide.
    --
    -- Calls with tags apply to the initial creation of the resource policy,
    -- otherwise an exception is thrown. If any one of the tags is invalid or
    -- if you exceed the allowed number of tags for the resource policy, then
    -- the entire request fails and the resource policy is not created.
    tags :: Prelude.Maybe [Tag],
    -- | If provided, the new content for the resource policy. The text must be
    -- correctly formatted JSON that complies with the syntax for the resource
    -- policy\'s type. For more information, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
    -- in the /Organizations User Guide./
    content :: Prelude.Text
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
-- 'tags', 'putResourcePolicy_tags' - Updates the list of tags that you want to attach to the newly-created
-- resource policy. For each tag in the list, you must specify both a tag
-- key and a value. You can set the value to an empty string, but you
-- can\'t set it to @null@. For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
-- in the Organizations User Guide.
--
-- Calls with tags apply to the initial creation of the resource policy,
-- otherwise an exception is thrown. If any one of the tags is invalid or
-- if you exceed the allowed number of tags for the resource policy, then
-- the entire request fails and the resource policy is not created.
--
-- 'content', 'putResourcePolicy_content' - If provided, the new content for the resource policy. The text must be
-- correctly formatted JSON that complies with the syntax for the resource
-- policy\'s type. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
-- in the /Organizations User Guide./
newPutResourcePolicy ::
  -- | 'content'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pContent_ =
  PutResourcePolicy'
    { tags = Prelude.Nothing,
      content = pContent_
    }

-- | Updates the list of tags that you want to attach to the newly-created
-- resource policy. For each tag in the list, you must specify both a tag
-- key and a value. You can set the value to an empty string, but you
-- can\'t set it to @null@. For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
-- in the Organizations User Guide.
--
-- Calls with tags apply to the initial creation of the resource policy,
-- otherwise an exception is thrown. If any one of the tags is invalid or
-- if you exceed the allowed number of tags for the resource policy, then
-- the entire request fails and the resource policy is not created.
putResourcePolicy_tags :: Lens.Lens' PutResourcePolicy (Prelude.Maybe [Tag])
putResourcePolicy_tags = Lens.lens (\PutResourcePolicy' {tags} -> tags) (\s@PutResourcePolicy' {} a -> s {tags = a} :: PutResourcePolicy) Prelude.. Lens.mapping Lens.coerced

-- | If provided, the new content for the resource policy. The text must be
-- correctly formatted JSON that complies with the syntax for the resource
-- policy\'s type. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
-- in the /Organizations User Guide./
putResourcePolicy_content :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_content = Lens.lens (\PutResourcePolicy' {content} -> content) (\s@PutResourcePolicy' {} a -> s {content = a} :: PutResourcePolicy)

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
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` content

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf content

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.PutResourcePolicy" ::
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
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Content" Data..= content)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | A structure that contains details about the resource policy.
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
-- 'resourcePolicy', 'putResourcePolicyResponse_resourcePolicy' - A structure that contains details about the resource policy.
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

-- | A structure that contains details about the resource policy.
putResourcePolicyResponse_resourcePolicy :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe ResourcePolicy)
putResourcePolicyResponse_resourcePolicy = Lens.lens (\PutResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf resourcePolicy
      `Prelude.seq` Prelude.rnf httpStatus
