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
-- Module      : Network.AWS.IoT.CreatePolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified AWS IoT policy. To update a
-- policy, create a new policy version. A managed policy can have up to
-- five versions. If the policy has five versions, you must use
-- DeletePolicyVersion to delete an existing version before you create a
-- new one.
--
-- Optionally, you can set the new version as the policy\'s default
-- version. The default version is the operative version (that is, the
-- version that is in effect for the certificates to which the policy is
-- attached).
module Network.AWS.IoT.CreatePolicyVersion
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
    createPolicyVersionResponse_policyVersionId,
    createPolicyVersionResponse_policyDocument,
    createPolicyVersionResponse_isDefaultVersion,
    createPolicyVersionResponse_policyArn,
    createPolicyVersionResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CreatePolicyVersion where
  type
    Rs CreatePolicyVersion =
      CreatePolicyVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyVersionResponse'
            Prelude.<$> (x Prelude..?> "policyVersionId")
            Prelude.<*> (x Prelude..?> "policyDocument")
            Prelude.<*> (x Prelude..?> "isDefaultVersion")
            Prelude.<*> (x Prelude..?> "policyArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePolicyVersion

instance Prelude.NFData CreatePolicyVersion

instance Prelude.ToHeaders CreatePolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreatePolicyVersion where
  toJSON CreatePolicyVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyDocument" Prelude..= policyDocument)
          ]
      )

instance Prelude.ToPath CreatePolicyVersion where
  toPath CreatePolicyVersion' {..} =
    Prelude.mconcat
      ["/policies/", Prelude.toBS policyName, "/version"]

instance Prelude.ToQuery CreatePolicyVersion where
  toQuery CreatePolicyVersion' {..} =
    Prelude.mconcat
      ["setAsDefault" Prelude.=: setAsDefault]

-- | The output of the CreatePolicyVersion operation.
--
-- /See:/ 'newCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { -- | The policy version ID.
    policyVersionId :: Prelude.Maybe Prelude.Text,
    -- | The JSON document that describes the policy.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The policy ARN.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyVersionId', 'createPolicyVersionResponse_policyVersionId' - The policy version ID.
--
-- 'policyDocument', 'createPolicyVersionResponse_policyDocument' - The JSON document that describes the policy.
--
-- 'isDefaultVersion', 'createPolicyVersionResponse_isDefaultVersion' - Specifies whether the policy version is the default.
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
    { policyVersionId =
        Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy version ID.
createPolicyVersionResponse_policyVersionId :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe Prelude.Text)
createPolicyVersionResponse_policyVersionId = Lens.lens (\CreatePolicyVersionResponse' {policyVersionId} -> policyVersionId) (\s@CreatePolicyVersionResponse' {} a -> s {policyVersionId = a} :: CreatePolicyVersionResponse)

-- | The JSON document that describes the policy.
createPolicyVersionResponse_policyDocument :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe Prelude.Text)
createPolicyVersionResponse_policyDocument = Lens.lens (\CreatePolicyVersionResponse' {policyDocument} -> policyDocument) (\s@CreatePolicyVersionResponse' {} a -> s {policyDocument = a} :: CreatePolicyVersionResponse)

-- | Specifies whether the policy version is the default.
createPolicyVersionResponse_isDefaultVersion :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe Prelude.Bool)
createPolicyVersionResponse_isDefaultVersion = Lens.lens (\CreatePolicyVersionResponse' {isDefaultVersion} -> isDefaultVersion) (\s@CreatePolicyVersionResponse' {} a -> s {isDefaultVersion = a} :: CreatePolicyVersionResponse)

-- | The policy ARN.
createPolicyVersionResponse_policyArn :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe Prelude.Text)
createPolicyVersionResponse_policyArn = Lens.lens (\CreatePolicyVersionResponse' {policyArn} -> policyArn) (\s@CreatePolicyVersionResponse' {} a -> s {policyArn = a} :: CreatePolicyVersionResponse)

-- | The response's http status code.
createPolicyVersionResponse_httpStatus :: Lens.Lens' CreatePolicyVersionResponse Prelude.Int
createPolicyVersionResponse_httpStatus = Lens.lens (\CreatePolicyVersionResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyVersionResponse' {} a -> s {httpStatus = a} :: CreatePolicyVersionResponse)

instance Prelude.NFData CreatePolicyVersionResponse
