{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.CreatePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT policy.
--
-- The created policy is the default version for the policy. This operation
-- creates a policy version with a version identifier of __1__ and sets
-- __1__ as the policy\'s default version.
module Network.AWS.IoT.CreatePolicy
  ( -- * Creating a Request
    CreatePolicy (..),
    newCreatePolicy,

    -- * Request Lenses
    createPolicy_tags,
    createPolicy_policyName,
    createPolicy_policyDocument,

    -- * Destructuring the Response
    CreatePolicyResponse (..),
    newCreatePolicyResponse,

    -- * Response Lenses
    createPolicyResponse_policyVersionId,
    createPolicyResponse_policyName,
    createPolicyResponse_policyDocument,
    createPolicyResponse_policyArn,
    createPolicyResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreatePolicy operation.
--
-- /See:/ 'newCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { -- | Metadata which can be used to manage the policy.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Prelude.Maybe [Tag],
    -- | The policy name.
    policyName :: Prelude.Text,
    -- | The JSON document that describes the policy. __policyDocument__ must
    -- have a minimum length of 1, with a maximum length of 2048, excluding
    -- whitespace.
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPolicy_tags' - Metadata which can be used to manage the policy.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'policyName', 'createPolicy_policyName' - The policy name.
--
-- 'policyDocument', 'createPolicy_policyDocument' - The JSON document that describes the policy. __policyDocument__ must
-- have a minimum length of 1, with a maximum length of 2048, excluding
-- whitespace.
newCreatePolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  CreatePolicy
newCreatePolicy pPolicyName_ pPolicyDocument_ =
  CreatePolicy'
    { tags = Prelude.Nothing,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | Metadata which can be used to manage the policy.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createPolicy_tags :: Lens.Lens' CreatePolicy (Prelude.Maybe [Tag])
createPolicy_tags = Lens.lens (\CreatePolicy' {tags} -> tags) (\s@CreatePolicy' {} a -> s {tags = a} :: CreatePolicy) Prelude.. Lens.mapping Prelude._Coerce

-- | The policy name.
createPolicy_policyName :: Lens.Lens' CreatePolicy Prelude.Text
createPolicy_policyName = Lens.lens (\CreatePolicy' {policyName} -> policyName) (\s@CreatePolicy' {} a -> s {policyName = a} :: CreatePolicy)

-- | The JSON document that describes the policy. __policyDocument__ must
-- have a minimum length of 1, with a maximum length of 2048, excluding
-- whitespace.
createPolicy_policyDocument :: Lens.Lens' CreatePolicy Prelude.Text
createPolicy_policyDocument = Lens.lens (\CreatePolicy' {policyDocument} -> policyDocument) (\s@CreatePolicy' {} a -> s {policyDocument = a} :: CreatePolicy)

instance Prelude.AWSRequest CreatePolicy where
  type Rs CreatePolicy = CreatePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyResponse'
            Prelude.<$> (x Prelude..?> "policyVersionId")
            Prelude.<*> (x Prelude..?> "policyName")
            Prelude.<*> (x Prelude..?> "policyDocument")
            Prelude.<*> (x Prelude..?> "policyArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePolicy

instance Prelude.NFData CreatePolicy

instance Prelude.ToHeaders CreatePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreatePolicy where
  toJSON CreatePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ("policyDocument" Prelude..= policyDocument)
          ]
      )

instance Prelude.ToPath CreatePolicy where
  toPath CreatePolicy' {..} =
    Prelude.mconcat
      ["/policies/", Prelude.toBS policyName]

instance Prelude.ToQuery CreatePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the CreatePolicy operation.
--
-- /See:/ 'newCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { -- | The policy version ID.
    policyVersionId :: Prelude.Maybe Prelude.Text,
    -- | The policy name.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The JSON document that describes the policy.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The policy ARN.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyVersionId', 'createPolicyResponse_policyVersionId' - The policy version ID.
--
-- 'policyName', 'createPolicyResponse_policyName' - The policy name.
--
-- 'policyDocument', 'createPolicyResponse_policyDocument' - The JSON document that describes the policy.
--
-- 'policyArn', 'createPolicyResponse_policyArn' - The policy ARN.
--
-- 'httpStatus', 'createPolicyResponse_httpStatus' - The response's http status code.
newCreatePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePolicyResponse
newCreatePolicyResponse pHttpStatus_ =
  CreatePolicyResponse'
    { policyVersionId =
        Prelude.Nothing,
      policyName = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy version ID.
createPolicyResponse_policyVersionId :: Lens.Lens' CreatePolicyResponse (Prelude.Maybe Prelude.Text)
createPolicyResponse_policyVersionId = Lens.lens (\CreatePolicyResponse' {policyVersionId} -> policyVersionId) (\s@CreatePolicyResponse' {} a -> s {policyVersionId = a} :: CreatePolicyResponse)

-- | The policy name.
createPolicyResponse_policyName :: Lens.Lens' CreatePolicyResponse (Prelude.Maybe Prelude.Text)
createPolicyResponse_policyName = Lens.lens (\CreatePolicyResponse' {policyName} -> policyName) (\s@CreatePolicyResponse' {} a -> s {policyName = a} :: CreatePolicyResponse)

-- | The JSON document that describes the policy.
createPolicyResponse_policyDocument :: Lens.Lens' CreatePolicyResponse (Prelude.Maybe Prelude.Text)
createPolicyResponse_policyDocument = Lens.lens (\CreatePolicyResponse' {policyDocument} -> policyDocument) (\s@CreatePolicyResponse' {} a -> s {policyDocument = a} :: CreatePolicyResponse)

-- | The policy ARN.
createPolicyResponse_policyArn :: Lens.Lens' CreatePolicyResponse (Prelude.Maybe Prelude.Text)
createPolicyResponse_policyArn = Lens.lens (\CreatePolicyResponse' {policyArn} -> policyArn) (\s@CreatePolicyResponse' {} a -> s {policyArn = a} :: CreatePolicyResponse)

-- | The response's http status code.
createPolicyResponse_httpStatus :: Lens.Lens' CreatePolicyResponse Prelude.Int
createPolicyResponse_httpStatus = Lens.lens (\CreatePolicyResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyResponse' {} a -> s {httpStatus = a} :: CreatePolicyResponse)

instance Prelude.NFData CreatePolicyResponse
