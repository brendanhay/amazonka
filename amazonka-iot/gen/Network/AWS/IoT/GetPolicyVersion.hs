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
-- Module      : Network.AWS.IoT.GetPolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified policy version.
module Network.AWS.IoT.GetPolicyVersion
  ( -- * Creating a Request
    GetPolicyVersion (..),
    newGetPolicyVersion,

    -- * Request Lenses
    getPolicyVersion_policyName,
    getPolicyVersion_policyVersionId,

    -- * Destructuring the Response
    GetPolicyVersionResponse (..),
    newGetPolicyVersionResponse,

    -- * Response Lenses
    getPolicyVersionResponse_policyVersionId,
    getPolicyVersionResponse_lastModifiedDate,
    getPolicyVersionResponse_policyName,
    getPolicyVersionResponse_policyDocument,
    getPolicyVersionResponse_creationDate,
    getPolicyVersionResponse_generationId,
    getPolicyVersionResponse_isDefaultVersion,
    getPolicyVersionResponse_policyArn,
    getPolicyVersionResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetPolicyVersion operation.
--
-- /See:/ 'newGetPolicyVersion' smart constructor.
data GetPolicyVersion = GetPolicyVersion'
  { -- | The name of the policy.
    policyName :: Prelude.Text,
    -- | The policy version ID.
    policyVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'getPolicyVersion_policyName' - The name of the policy.
--
-- 'policyVersionId', 'getPolicyVersion_policyVersionId' - The policy version ID.
newGetPolicyVersion ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyVersionId'
  Prelude.Text ->
  GetPolicyVersion
newGetPolicyVersion pPolicyName_ pPolicyVersionId_ =
  GetPolicyVersion'
    { policyName = pPolicyName_,
      policyVersionId = pPolicyVersionId_
    }

-- | The name of the policy.
getPolicyVersion_policyName :: Lens.Lens' GetPolicyVersion Prelude.Text
getPolicyVersion_policyName = Lens.lens (\GetPolicyVersion' {policyName} -> policyName) (\s@GetPolicyVersion' {} a -> s {policyName = a} :: GetPolicyVersion)

-- | The policy version ID.
getPolicyVersion_policyVersionId :: Lens.Lens' GetPolicyVersion Prelude.Text
getPolicyVersion_policyVersionId = Lens.lens (\GetPolicyVersion' {policyVersionId} -> policyVersionId) (\s@GetPolicyVersion' {} a -> s {policyVersionId = a} :: GetPolicyVersion)

instance Prelude.AWSRequest GetPolicyVersion where
  type Rs GetPolicyVersion = GetPolicyVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyVersionResponse'
            Prelude.<$> (x Prelude..?> "policyVersionId")
            Prelude.<*> (x Prelude..?> "lastModifiedDate")
            Prelude.<*> (x Prelude..?> "policyName")
            Prelude.<*> (x Prelude..?> "policyDocument")
            Prelude.<*> (x Prelude..?> "creationDate")
            Prelude.<*> (x Prelude..?> "generationId")
            Prelude.<*> (x Prelude..?> "isDefaultVersion")
            Prelude.<*> (x Prelude..?> "policyArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPolicyVersion

instance Prelude.NFData GetPolicyVersion

instance Prelude.ToHeaders GetPolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetPolicyVersion where
  toPath GetPolicyVersion' {..} =
    Prelude.mconcat
      [ "/policies/",
        Prelude.toBS policyName,
        "/version/",
        Prelude.toBS policyVersionId
      ]

instance Prelude.ToQuery GetPolicyVersion where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the GetPolicyVersion operation.
--
-- /See:/ 'newGetPolicyVersionResponse' smart constructor.
data GetPolicyVersionResponse = GetPolicyVersionResponse'
  { -- | The policy version ID.
    policyVersionId :: Prelude.Maybe Prelude.Text,
    -- | The date the policy was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The policy name.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The JSON document that describes the policy.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The date the policy was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The generation ID of the policy version.
    generationId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The policy ARN.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyVersionId', 'getPolicyVersionResponse_policyVersionId' - The policy version ID.
--
-- 'lastModifiedDate', 'getPolicyVersionResponse_lastModifiedDate' - The date the policy was last modified.
--
-- 'policyName', 'getPolicyVersionResponse_policyName' - The policy name.
--
-- 'policyDocument', 'getPolicyVersionResponse_policyDocument' - The JSON document that describes the policy.
--
-- 'creationDate', 'getPolicyVersionResponse_creationDate' - The date the policy was created.
--
-- 'generationId', 'getPolicyVersionResponse_generationId' - The generation ID of the policy version.
--
-- 'isDefaultVersion', 'getPolicyVersionResponse_isDefaultVersion' - Specifies whether the policy version is the default.
--
-- 'policyArn', 'getPolicyVersionResponse_policyArn' - The policy ARN.
--
-- 'httpStatus', 'getPolicyVersionResponse_httpStatus' - The response's http status code.
newGetPolicyVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPolicyVersionResponse
newGetPolicyVersionResponse pHttpStatus_ =
  GetPolicyVersionResponse'
    { policyVersionId =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      policyName = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      generationId = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy version ID.
getPolicyVersionResponse_policyVersionId :: Lens.Lens' GetPolicyVersionResponse (Prelude.Maybe Prelude.Text)
getPolicyVersionResponse_policyVersionId = Lens.lens (\GetPolicyVersionResponse' {policyVersionId} -> policyVersionId) (\s@GetPolicyVersionResponse' {} a -> s {policyVersionId = a} :: GetPolicyVersionResponse)

-- | The date the policy was last modified.
getPolicyVersionResponse_lastModifiedDate :: Lens.Lens' GetPolicyVersionResponse (Prelude.Maybe Prelude.UTCTime)
getPolicyVersionResponse_lastModifiedDate = Lens.lens (\GetPolicyVersionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@GetPolicyVersionResponse' {} a -> s {lastModifiedDate = a} :: GetPolicyVersionResponse) Prelude.. Lens.mapping Prelude._Time

-- | The policy name.
getPolicyVersionResponse_policyName :: Lens.Lens' GetPolicyVersionResponse (Prelude.Maybe Prelude.Text)
getPolicyVersionResponse_policyName = Lens.lens (\GetPolicyVersionResponse' {policyName} -> policyName) (\s@GetPolicyVersionResponse' {} a -> s {policyName = a} :: GetPolicyVersionResponse)

-- | The JSON document that describes the policy.
getPolicyVersionResponse_policyDocument :: Lens.Lens' GetPolicyVersionResponse (Prelude.Maybe Prelude.Text)
getPolicyVersionResponse_policyDocument = Lens.lens (\GetPolicyVersionResponse' {policyDocument} -> policyDocument) (\s@GetPolicyVersionResponse' {} a -> s {policyDocument = a} :: GetPolicyVersionResponse)

-- | The date the policy was created.
getPolicyVersionResponse_creationDate :: Lens.Lens' GetPolicyVersionResponse (Prelude.Maybe Prelude.UTCTime)
getPolicyVersionResponse_creationDate = Lens.lens (\GetPolicyVersionResponse' {creationDate} -> creationDate) (\s@GetPolicyVersionResponse' {} a -> s {creationDate = a} :: GetPolicyVersionResponse) Prelude.. Lens.mapping Prelude._Time

-- | The generation ID of the policy version.
getPolicyVersionResponse_generationId :: Lens.Lens' GetPolicyVersionResponse (Prelude.Maybe Prelude.Text)
getPolicyVersionResponse_generationId = Lens.lens (\GetPolicyVersionResponse' {generationId} -> generationId) (\s@GetPolicyVersionResponse' {} a -> s {generationId = a} :: GetPolicyVersionResponse)

-- | Specifies whether the policy version is the default.
getPolicyVersionResponse_isDefaultVersion :: Lens.Lens' GetPolicyVersionResponse (Prelude.Maybe Prelude.Bool)
getPolicyVersionResponse_isDefaultVersion = Lens.lens (\GetPolicyVersionResponse' {isDefaultVersion} -> isDefaultVersion) (\s@GetPolicyVersionResponse' {} a -> s {isDefaultVersion = a} :: GetPolicyVersionResponse)

-- | The policy ARN.
getPolicyVersionResponse_policyArn :: Lens.Lens' GetPolicyVersionResponse (Prelude.Maybe Prelude.Text)
getPolicyVersionResponse_policyArn = Lens.lens (\GetPolicyVersionResponse' {policyArn} -> policyArn) (\s@GetPolicyVersionResponse' {} a -> s {policyArn = a} :: GetPolicyVersionResponse)

-- | The response's http status code.
getPolicyVersionResponse_httpStatus :: Lens.Lens' GetPolicyVersionResponse Prelude.Int
getPolicyVersionResponse_httpStatus = Lens.lens (\GetPolicyVersionResponse' {httpStatus} -> httpStatus) (\s@GetPolicyVersionResponse' {} a -> s {httpStatus = a} :: GetPolicyVersionResponse)

instance Prelude.NFData GetPolicyVersionResponse
