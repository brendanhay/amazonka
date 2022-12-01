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
-- Module      : Amazonka.IoT.CreatePolicyVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified IoT policy. To update a policy,
-- create a new policy version. A managed policy can have up to five
-- versions. If the policy has five versions, you must use
-- DeletePolicyVersion to delete an existing version before you create a
-- new one.
--
-- Optionally, you can set the new version as the policy\'s default
-- version. The default version is the operative version (that is, the
-- version that is in effect for the certificates to which the policy is
-- attached).
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreatePolicyVersion>
-- action.
module Amazonka.IoT.CreatePolicyVersion
  ( -- * Creating a Request
    CreatePolicyVersion (..),
    newCreatePolicyVersion,

    -- * Request Lenses
    createPolicyVersion_setAsDefault,
    createPolicyVersion_policyName,
    createPolicyVersion_policyDocument,

    -- * Destructuring the Response
    CreatePolicyVersionResponse (..),
    newCreatePolicyVersionResponse,

    -- * Response Lenses
    createPolicyVersionResponse_isDefaultVersion,
    createPolicyVersionResponse_policyVersionId,
    createPolicyVersionResponse_policyDocument,
    createPolicyVersionResponse_policyArn,
    createPolicyVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the CreatePolicyVersion operation.
--
-- /See:/ 'newCreatePolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { -- | Specifies whether the policy version is set as the default. When this
    -- parameter is true, the new policy version becomes the operative version
    -- (that is, the version that is in effect for the certificates to which
    -- the policy is attached).
    setAsDefault :: Prelude.Maybe Prelude.Bool,
    -- | The policy name.
    policyName :: Prelude.Text,
    -- | The JSON document that describes the policy. Minimum length of 1.
    -- Maximum length of 2048, excluding whitespace.
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setAsDefault', 'createPolicyVersion_setAsDefault' - Specifies whether the policy version is set as the default. When this
-- parameter is true, the new policy version becomes the operative version
-- (that is, the version that is in effect for the certificates to which
-- the policy is attached).
--
-- 'policyName', 'createPolicyVersion_policyName' - The policy name.
--
-- 'policyDocument', 'createPolicyVersion_policyDocument' - The JSON document that describes the policy. Minimum length of 1.
-- Maximum length of 2048, excluding whitespace.
newCreatePolicyVersion ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  CreatePolicyVersion
newCreatePolicyVersion pPolicyName_ pPolicyDocument_ =
  CreatePolicyVersion'
    { setAsDefault =
        Prelude.Nothing,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | Specifies whether the policy version is set as the default. When this
-- parameter is true, the new policy version becomes the operative version
-- (that is, the version that is in effect for the certificates to which
-- the policy is attached).
createPolicyVersion_setAsDefault :: Lens.Lens' CreatePolicyVersion (Prelude.Maybe Prelude.Bool)
createPolicyVersion_setAsDefault = Lens.lens (\CreatePolicyVersion' {setAsDefault} -> setAsDefault) (\s@CreatePolicyVersion' {} a -> s {setAsDefault = a} :: CreatePolicyVersion)

-- | The policy name.
createPolicyVersion_policyName :: Lens.Lens' CreatePolicyVersion Prelude.Text
createPolicyVersion_policyName = Lens.lens (\CreatePolicyVersion' {policyName} -> policyName) (\s@CreatePolicyVersion' {} a -> s {policyName = a} :: CreatePolicyVersion)

-- | The JSON document that describes the policy. Minimum length of 1.
-- Maximum length of 2048, excluding whitespace.
createPolicyVersion_policyDocument :: Lens.Lens' CreatePolicyVersion Prelude.Text
createPolicyVersion_policyDocument = Lens.lens (\CreatePolicyVersion' {policyDocument} -> policyDocument) (\s@CreatePolicyVersion' {} a -> s {policyDocument = a} :: CreatePolicyVersion)

instance Core.AWSRequest CreatePolicyVersion where
  type
    AWSResponse CreatePolicyVersion =
      CreatePolicyVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyVersionResponse'
            Prelude.<$> (x Core..?> "isDefaultVersion")
            Prelude.<*> (x Core..?> "policyVersionId")
            Prelude.<*> (x Core..?> "policyDocument")
            Prelude.<*> (x Core..?> "policyArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePolicyVersion where
  hashWithSalt _salt CreatePolicyVersion' {..} =
    _salt `Prelude.hashWithSalt` setAsDefault
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData CreatePolicyVersion where
  rnf CreatePolicyVersion' {..} =
    Prelude.rnf setAsDefault
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyDocument

instance Core.ToHeaders CreatePolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreatePolicyVersion where
  toJSON CreatePolicyVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyDocument" Core..= policyDocument)
          ]
      )

instance Core.ToPath CreatePolicyVersion where
  toPath CreatePolicyVersion' {..} =
    Prelude.mconcat
      ["/policies/", Core.toBS policyName, "/version"]

instance Core.ToQuery CreatePolicyVersion where
  toQuery CreatePolicyVersion' {..} =
    Prelude.mconcat
      ["setAsDefault" Core.=: setAsDefault]

-- | The output of the CreatePolicyVersion operation.
--
-- /See:/ 'newCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The policy version ID.
    policyVersionId :: Prelude.Maybe Prelude.Text,
    -- | The JSON document that describes the policy.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The policy ARN.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefaultVersion', 'createPolicyVersionResponse_isDefaultVersion' - Specifies whether the policy version is the default.
--
-- 'policyVersionId', 'createPolicyVersionResponse_policyVersionId' - The policy version ID.
--
-- 'policyDocument', 'createPolicyVersionResponse_policyDocument' - The JSON document that describes the policy.
--
-- 'policyArn', 'createPolicyVersionResponse_policyArn' - The policy ARN.
--
-- 'httpStatus', 'createPolicyVersionResponse_httpStatus' - The response's http status code.
newCreatePolicyVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePolicyVersionResponse
newCreatePolicyVersionResponse pHttpStatus_ =
  CreatePolicyVersionResponse'
    { isDefaultVersion =
        Prelude.Nothing,
      policyVersionId = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies whether the policy version is the default.
createPolicyVersionResponse_isDefaultVersion :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe Prelude.Bool)
createPolicyVersionResponse_isDefaultVersion = Lens.lens (\CreatePolicyVersionResponse' {isDefaultVersion} -> isDefaultVersion) (\s@CreatePolicyVersionResponse' {} a -> s {isDefaultVersion = a} :: CreatePolicyVersionResponse)

-- | The policy version ID.
createPolicyVersionResponse_policyVersionId :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe Prelude.Text)
createPolicyVersionResponse_policyVersionId = Lens.lens (\CreatePolicyVersionResponse' {policyVersionId} -> policyVersionId) (\s@CreatePolicyVersionResponse' {} a -> s {policyVersionId = a} :: CreatePolicyVersionResponse)

-- | The JSON document that describes the policy.
createPolicyVersionResponse_policyDocument :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe Prelude.Text)
createPolicyVersionResponse_policyDocument = Lens.lens (\CreatePolicyVersionResponse' {policyDocument} -> policyDocument) (\s@CreatePolicyVersionResponse' {} a -> s {policyDocument = a} :: CreatePolicyVersionResponse)

-- | The policy ARN.
createPolicyVersionResponse_policyArn :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe Prelude.Text)
createPolicyVersionResponse_policyArn = Lens.lens (\CreatePolicyVersionResponse' {policyArn} -> policyArn) (\s@CreatePolicyVersionResponse' {} a -> s {policyArn = a} :: CreatePolicyVersionResponse)

-- | The response's http status code.
createPolicyVersionResponse_httpStatus :: Lens.Lens' CreatePolicyVersionResponse Prelude.Int
createPolicyVersionResponse_httpStatus = Lens.lens (\CreatePolicyVersionResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyVersionResponse' {} a -> s {httpStatus = a} :: CreatePolicyVersionResponse)

instance Prelude.NFData CreatePolicyVersionResponse where
  rnf CreatePolicyVersionResponse' {..} =
    Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf policyVersionId
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf httpStatus
