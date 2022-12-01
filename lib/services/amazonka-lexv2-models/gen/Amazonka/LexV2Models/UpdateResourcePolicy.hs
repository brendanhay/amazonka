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
-- Module      : Amazonka.LexV2Models.UpdateResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the existing resource policy for a bot or bot alias with a new
-- one. If the policy doesn\'t exist, Amazon Lex returns an exception.
module Amazonka.LexV2Models.UpdateResourcePolicy
  ( -- * Creating a Request
    UpdateResourcePolicy (..),
    newUpdateResourcePolicy,

    -- * Request Lenses
    updateResourcePolicy_expectedRevisionId,
    updateResourcePolicy_resourceArn,
    updateResourcePolicy_policy,

    -- * Destructuring the Response
    UpdateResourcePolicyResponse (..),
    newUpdateResourcePolicyResponse,

    -- * Response Lenses
    updateResourcePolicyResponse_revisionId,
    updateResourcePolicyResponse_resourceArn,
    updateResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResourcePolicy' smart constructor.
data UpdateResourcePolicy = UpdateResourcePolicy'
  { -- | The identifier of the revision of the policy to update. If this revision
    -- ID doesn\'t match the current revision ID, Amazon Lex throws an
    -- exception.
    --
    -- If you don\'t specify a revision, Amazon Lex overwrites the contents of
    -- the policy with the new values.
    expectedRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy is attached to.
    resourceArn :: Prelude.Text,
    -- | A resource policy to add to the resource. The policy is a JSON structure
    -- that contains one or more statements that define the policy. The policy
    -- must follow the IAM syntax. For more information about the contents of a
    -- JSON policy document, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>
    -- .
    --
    -- If the policy isn\'t valid, Amazon Lex returns a validation exception.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedRevisionId', 'updateResourcePolicy_expectedRevisionId' - The identifier of the revision of the policy to update. If this revision
-- ID doesn\'t match the current revision ID, Amazon Lex throws an
-- exception.
--
-- If you don\'t specify a revision, Amazon Lex overwrites the contents of
-- the policy with the new values.
--
-- 'resourceArn', 'updateResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
--
-- 'policy', 'updateResourcePolicy_policy' - A resource policy to add to the resource. The policy is a JSON structure
-- that contains one or more statements that define the policy. The policy
-- must follow the IAM syntax. For more information about the contents of a
-- JSON policy document, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>
-- .
--
-- If the policy isn\'t valid, Amazon Lex returns a validation exception.
newUpdateResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  UpdateResourcePolicy
newUpdateResourcePolicy pResourceArn_ pPolicy_ =
  UpdateResourcePolicy'
    { expectedRevisionId =
        Prelude.Nothing,
      resourceArn = pResourceArn_,
      policy = pPolicy_
    }

-- | The identifier of the revision of the policy to update. If this revision
-- ID doesn\'t match the current revision ID, Amazon Lex throws an
-- exception.
--
-- If you don\'t specify a revision, Amazon Lex overwrites the contents of
-- the policy with the new values.
updateResourcePolicy_expectedRevisionId :: Lens.Lens' UpdateResourcePolicy (Prelude.Maybe Prelude.Text)
updateResourcePolicy_expectedRevisionId = Lens.lens (\UpdateResourcePolicy' {expectedRevisionId} -> expectedRevisionId) (\s@UpdateResourcePolicy' {} a -> s {expectedRevisionId = a} :: UpdateResourcePolicy)

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
updateResourcePolicy_resourceArn :: Lens.Lens' UpdateResourcePolicy Prelude.Text
updateResourcePolicy_resourceArn = Lens.lens (\UpdateResourcePolicy' {resourceArn} -> resourceArn) (\s@UpdateResourcePolicy' {} a -> s {resourceArn = a} :: UpdateResourcePolicy)

-- | A resource policy to add to the resource. The policy is a JSON structure
-- that contains one or more statements that define the policy. The policy
-- must follow the IAM syntax. For more information about the contents of a
-- JSON policy document, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON policy reference>
-- .
--
-- If the policy isn\'t valid, Amazon Lex returns a validation exception.
updateResourcePolicy_policy :: Lens.Lens' UpdateResourcePolicy Prelude.Text
updateResourcePolicy_policy = Lens.lens (\UpdateResourcePolicy' {policy} -> policy) (\s@UpdateResourcePolicy' {} a -> s {policy = a} :: UpdateResourcePolicy)

instance Core.AWSRequest UpdateResourcePolicy where
  type
    AWSResponse UpdateResourcePolicy =
      UpdateResourcePolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourcePolicyResponse'
            Prelude.<$> (x Core..?> "revisionId")
            Prelude.<*> (x Core..?> "resourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourcePolicy where
  hashWithSalt _salt UpdateResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` expectedRevisionId
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData UpdateResourcePolicy where
  rnf UpdateResourcePolicy' {..} =
    Prelude.rnf expectedRevisionId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf policy

instance Core.ToHeaders UpdateResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateResourcePolicy where
  toJSON UpdateResourcePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("policy" Core..= policy)]
      )

instance Core.ToPath UpdateResourcePolicy where
  toPath UpdateResourcePolicy' {..} =
    Prelude.mconcat
      ["/policy/", Core.toBS resourceArn, "/"]

instance Core.ToQuery UpdateResourcePolicy where
  toQuery UpdateResourcePolicy' {..} =
    Prelude.mconcat
      ["expectedRevisionId" Core.=: expectedRevisionId]

-- | /See:/ 'newUpdateResourcePolicyResponse' smart constructor.
data UpdateResourcePolicyResponse = UpdateResourcePolicyResponse'
  { -- | The current revision of the resource policy. Use the revision ID to make
    -- sure that you are updating the most current version of a resource policy
    -- when you add a policy statement to a resource, delete a resource, or
    -- update a resource.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy is attached to.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'updateResourcePolicyResponse_revisionId' - The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
--
-- 'resourceArn', 'updateResourcePolicyResponse_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
--
-- 'httpStatus', 'updateResourcePolicyResponse_httpStatus' - The response's http status code.
newUpdateResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourcePolicyResponse
newUpdateResourcePolicyResponse pHttpStatus_ =
  UpdateResourcePolicyResponse'
    { revisionId =
        Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
updateResourcePolicyResponse_revisionId :: Lens.Lens' UpdateResourcePolicyResponse (Prelude.Maybe Prelude.Text)
updateResourcePolicyResponse_revisionId = Lens.lens (\UpdateResourcePolicyResponse' {revisionId} -> revisionId) (\s@UpdateResourcePolicyResponse' {} a -> s {revisionId = a} :: UpdateResourcePolicyResponse)

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
updateResourcePolicyResponse_resourceArn :: Lens.Lens' UpdateResourcePolicyResponse (Prelude.Maybe Prelude.Text)
updateResourcePolicyResponse_resourceArn = Lens.lens (\UpdateResourcePolicyResponse' {resourceArn} -> resourceArn) (\s@UpdateResourcePolicyResponse' {} a -> s {resourceArn = a} :: UpdateResourcePolicyResponse)

-- | The response's http status code.
updateResourcePolicyResponse_httpStatus :: Lens.Lens' UpdateResourcePolicyResponse Prelude.Int
updateResourcePolicyResponse_httpStatus = Lens.lens (\UpdateResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateResourcePolicyResponse' {} a -> s {httpStatus = a} :: UpdateResourcePolicyResponse)

instance Prelude.NFData UpdateResourcePolicyResponse where
  rnf UpdateResourcePolicyResponse' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf httpStatus
