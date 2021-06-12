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
-- Module      : Network.AWS.IoT.GetPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified policy with the policy document of
-- the default version.
module Network.AWS.IoT.GetPolicy
  ( -- * Creating a Request
    GetPolicy (..),
    newGetPolicy,

    -- * Request Lenses
    getPolicy_policyName,

    -- * Destructuring the Response
    GetPolicyResponse (..),
    newGetPolicyResponse,

    -- * Response Lenses
    getPolicyResponse_lastModifiedDate,
    getPolicyResponse_policyName,
    getPolicyResponse_policyDocument,
    getPolicyResponse_creationDate,
    getPolicyResponse_defaultVersionId,
    getPolicyResponse_generationId,
    getPolicyResponse_policyArn,
    getPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetPolicy operation.
--
-- /See:/ 'newGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { -- | The name of the policy.
    policyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'getPolicy_policyName' - The name of the policy.
newGetPolicy ::
  -- | 'policyName'
  Core.Text ->
  GetPolicy
newGetPolicy pPolicyName_ =
  GetPolicy' {policyName = pPolicyName_}

-- | The name of the policy.
getPolicy_policyName :: Lens.Lens' GetPolicy Core.Text
getPolicy_policyName = Lens.lens (\GetPolicy' {policyName} -> policyName) (\s@GetPolicy' {} a -> s {policyName = a} :: GetPolicy)

instance Core.AWSRequest GetPolicy where
  type AWSResponse GetPolicy = GetPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Core.<$> (x Core..?> "lastModifiedDate")
            Core.<*> (x Core..?> "policyName")
            Core.<*> (x Core..?> "policyDocument")
            Core.<*> (x Core..?> "creationDate")
            Core.<*> (x Core..?> "defaultVersionId")
            Core.<*> (x Core..?> "generationId")
            Core.<*> (x Core..?> "policyArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPolicy

instance Core.NFData GetPolicy

instance Core.ToHeaders GetPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetPolicy where
  toPath GetPolicy' {..} =
    Core.mconcat ["/policies/", Core.toBS policyName]

instance Core.ToQuery GetPolicy where
  toQuery = Core.const Core.mempty

-- | The output from the GetPolicy operation.
--
-- /See:/ 'newGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | The date the policy was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The policy name.
    policyName :: Core.Maybe Core.Text,
    -- | The JSON document that describes the policy.
    policyDocument :: Core.Maybe Core.Text,
    -- | The date the policy was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The default policy version ID.
    defaultVersionId :: Core.Maybe Core.Text,
    -- | The generation ID of the policy.
    generationId :: Core.Maybe Core.Text,
    -- | The policy ARN.
    policyArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'getPolicyResponse_lastModifiedDate' - The date the policy was last modified.
--
-- 'policyName', 'getPolicyResponse_policyName' - The policy name.
--
-- 'policyDocument', 'getPolicyResponse_policyDocument' - The JSON document that describes the policy.
--
-- 'creationDate', 'getPolicyResponse_creationDate' - The date the policy was created.
--
-- 'defaultVersionId', 'getPolicyResponse_defaultVersionId' - The default policy version ID.
--
-- 'generationId', 'getPolicyResponse_generationId' - The generation ID of the policy.
--
-- 'policyArn', 'getPolicyResponse_policyArn' - The policy ARN.
--
-- 'httpStatus', 'getPolicyResponse_httpStatus' - The response's http status code.
newGetPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPolicyResponse
newGetPolicyResponse pHttpStatus_ =
  GetPolicyResponse'
    { lastModifiedDate = Core.Nothing,
      policyName = Core.Nothing,
      policyDocument = Core.Nothing,
      creationDate = Core.Nothing,
      defaultVersionId = Core.Nothing,
      generationId = Core.Nothing,
      policyArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date the policy was last modified.
getPolicyResponse_lastModifiedDate :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.UTCTime)
getPolicyResponse_lastModifiedDate = Lens.lens (\GetPolicyResponse' {lastModifiedDate} -> lastModifiedDate) (\s@GetPolicyResponse' {} a -> s {lastModifiedDate = a} :: GetPolicyResponse) Core.. Lens.mapping Core._Time

-- | The policy name.
getPolicyResponse_policyName :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
getPolicyResponse_policyName = Lens.lens (\GetPolicyResponse' {policyName} -> policyName) (\s@GetPolicyResponse' {} a -> s {policyName = a} :: GetPolicyResponse)

-- | The JSON document that describes the policy.
getPolicyResponse_policyDocument :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
getPolicyResponse_policyDocument = Lens.lens (\GetPolicyResponse' {policyDocument} -> policyDocument) (\s@GetPolicyResponse' {} a -> s {policyDocument = a} :: GetPolicyResponse)

-- | The date the policy was created.
getPolicyResponse_creationDate :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.UTCTime)
getPolicyResponse_creationDate = Lens.lens (\GetPolicyResponse' {creationDate} -> creationDate) (\s@GetPolicyResponse' {} a -> s {creationDate = a} :: GetPolicyResponse) Core.. Lens.mapping Core._Time

-- | The default policy version ID.
getPolicyResponse_defaultVersionId :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
getPolicyResponse_defaultVersionId = Lens.lens (\GetPolicyResponse' {defaultVersionId} -> defaultVersionId) (\s@GetPolicyResponse' {} a -> s {defaultVersionId = a} :: GetPolicyResponse)

-- | The generation ID of the policy.
getPolicyResponse_generationId :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
getPolicyResponse_generationId = Lens.lens (\GetPolicyResponse' {generationId} -> generationId) (\s@GetPolicyResponse' {} a -> s {generationId = a} :: GetPolicyResponse)

-- | The policy ARN.
getPolicyResponse_policyArn :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
getPolicyResponse_policyArn = Lens.lens (\GetPolicyResponse' {policyArn} -> policyArn) (\s@GetPolicyResponse' {} a -> s {policyArn = a} :: GetPolicyResponse)

-- | The response's http status code.
getPolicyResponse_httpStatus :: Lens.Lens' GetPolicyResponse Core.Int
getPolicyResponse_httpStatus = Lens.lens (\GetPolicyResponse' {httpStatus} -> httpStatus) (\s@GetPolicyResponse' {} a -> s {httpStatus = a} :: GetPolicyResponse)

instance Core.NFData GetPolicyResponse
