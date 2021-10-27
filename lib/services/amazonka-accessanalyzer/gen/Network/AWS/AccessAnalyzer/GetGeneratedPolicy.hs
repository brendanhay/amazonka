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
-- Module      : Network.AWS.AccessAnalyzer.GetGeneratedPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy that was generated using @StartPolicyGeneration@.
module Network.AWS.AccessAnalyzer.GetGeneratedPolicy
  ( -- * Creating a Request
    GetGeneratedPolicy (..),
    newGetGeneratedPolicy,

    -- * Request Lenses
    getGeneratedPolicy_includeServiceLevelTemplate,
    getGeneratedPolicy_includeResourcePlaceholders,
    getGeneratedPolicy_jobId,

    -- * Destructuring the Response
    GetGeneratedPolicyResponse (..),
    newGetGeneratedPolicyResponse,

    -- * Response Lenses
    getGeneratedPolicyResponse_httpStatus,
    getGeneratedPolicyResponse_generatedPolicyResult,
    getGeneratedPolicyResponse_jobDetails,
  )
where

import Network.AWS.AccessAnalyzer.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGeneratedPolicy' smart constructor.
data GetGeneratedPolicy = GetGeneratedPolicy'
  { -- | The level of detail that you want to generate. You can specify whether
    -- to generate service-level policies.
    --
    -- IAM Access Analyzer uses @iam:servicelastaccessed@ to identify services
    -- that have been used recently to create this service-level template.
    includeServiceLevelTemplate :: Prelude.Maybe Prelude.Bool,
    -- | The level of detail that you want to generate. You can specify whether
    -- to generate policies with placeholders for resource ARNs for actions
    -- that support resource level granularity in policies.
    --
    -- For example, in the resource section of a policy, you can receive a
    -- placeholder such as @\"Resource\":\"arn:aws:s3:::${BucketName}\"@
    -- instead of @\"*\"@.
    includeResourcePlaceholders :: Prelude.Maybe Prelude.Bool,
    -- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
    -- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
    -- generated policies or used with @CancelPolicyGeneration@ to cancel the
    -- policy generation request.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeneratedPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeServiceLevelTemplate', 'getGeneratedPolicy_includeServiceLevelTemplate' - The level of detail that you want to generate. You can specify whether
-- to generate service-level policies.
--
-- IAM Access Analyzer uses @iam:servicelastaccessed@ to identify services
-- that have been used recently to create this service-level template.
--
-- 'includeResourcePlaceholders', 'getGeneratedPolicy_includeResourcePlaceholders' - The level of detail that you want to generate. You can specify whether
-- to generate policies with placeholders for resource ARNs for actions
-- that support resource level granularity in policies.
--
-- For example, in the resource section of a policy, you can receive a
-- placeholder such as @\"Resource\":\"arn:aws:s3:::${BucketName}\"@
-- instead of @\"*\"@.
--
-- 'jobId', 'getGeneratedPolicy_jobId' - The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
-- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
-- generated policies or used with @CancelPolicyGeneration@ to cancel the
-- policy generation request.
newGetGeneratedPolicy ::
  -- | 'jobId'
  Prelude.Text ->
  GetGeneratedPolicy
newGetGeneratedPolicy pJobId_ =
  GetGeneratedPolicy'
    { includeServiceLevelTemplate =
        Prelude.Nothing,
      includeResourcePlaceholders = Prelude.Nothing,
      jobId = pJobId_
    }

-- | The level of detail that you want to generate. You can specify whether
-- to generate service-level policies.
--
-- IAM Access Analyzer uses @iam:servicelastaccessed@ to identify services
-- that have been used recently to create this service-level template.
getGeneratedPolicy_includeServiceLevelTemplate :: Lens.Lens' GetGeneratedPolicy (Prelude.Maybe Prelude.Bool)
getGeneratedPolicy_includeServiceLevelTemplate = Lens.lens (\GetGeneratedPolicy' {includeServiceLevelTemplate} -> includeServiceLevelTemplate) (\s@GetGeneratedPolicy' {} a -> s {includeServiceLevelTemplate = a} :: GetGeneratedPolicy)

-- | The level of detail that you want to generate. You can specify whether
-- to generate policies with placeholders for resource ARNs for actions
-- that support resource level granularity in policies.
--
-- For example, in the resource section of a policy, you can receive a
-- placeholder such as @\"Resource\":\"arn:aws:s3:::${BucketName}\"@
-- instead of @\"*\"@.
getGeneratedPolicy_includeResourcePlaceholders :: Lens.Lens' GetGeneratedPolicy (Prelude.Maybe Prelude.Bool)
getGeneratedPolicy_includeResourcePlaceholders = Lens.lens (\GetGeneratedPolicy' {includeResourcePlaceholders} -> includeResourcePlaceholders) (\s@GetGeneratedPolicy' {} a -> s {includeResourcePlaceholders = a} :: GetGeneratedPolicy)

-- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
-- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
-- generated policies or used with @CancelPolicyGeneration@ to cancel the
-- policy generation request.
getGeneratedPolicy_jobId :: Lens.Lens' GetGeneratedPolicy Prelude.Text
getGeneratedPolicy_jobId = Lens.lens (\GetGeneratedPolicy' {jobId} -> jobId) (\s@GetGeneratedPolicy' {} a -> s {jobId = a} :: GetGeneratedPolicy)

instance Core.AWSRequest GetGeneratedPolicy where
  type
    AWSResponse GetGeneratedPolicy =
      GetGeneratedPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGeneratedPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "generatedPolicyResult")
            Prelude.<*> (x Core..:> "jobDetails")
      )

instance Prelude.Hashable GetGeneratedPolicy

instance Prelude.NFData GetGeneratedPolicy

instance Core.ToHeaders GetGeneratedPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetGeneratedPolicy where
  toPath GetGeneratedPolicy' {..} =
    Prelude.mconcat
      ["/policy/generation/", Core.toBS jobId]

instance Core.ToQuery GetGeneratedPolicy where
  toQuery GetGeneratedPolicy' {..} =
    Prelude.mconcat
      [ "includeServiceLevelTemplate"
          Core.=: includeServiceLevelTemplate,
        "includeResourcePlaceholders"
          Core.=: includeResourcePlaceholders
      ]

-- | /See:/ 'newGetGeneratedPolicyResponse' smart constructor.
data GetGeneratedPolicyResponse = GetGeneratedPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A @GeneratedPolicyResult@ object that contains the generated policies
    -- and associated details.
    generatedPolicyResult :: GeneratedPolicyResult,
    -- | A @GeneratedPolicyDetails@ object that contains details about the
    -- generated policy.
    jobDetails :: JobDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeneratedPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getGeneratedPolicyResponse_httpStatus' - The response's http status code.
--
-- 'generatedPolicyResult', 'getGeneratedPolicyResponse_generatedPolicyResult' - A @GeneratedPolicyResult@ object that contains the generated policies
-- and associated details.
--
-- 'jobDetails', 'getGeneratedPolicyResponse_jobDetails' - A @GeneratedPolicyDetails@ object that contains details about the
-- generated policy.
newGetGeneratedPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'generatedPolicyResult'
  GeneratedPolicyResult ->
  -- | 'jobDetails'
  JobDetails ->
  GetGeneratedPolicyResponse
newGetGeneratedPolicyResponse
  pHttpStatus_
  pGeneratedPolicyResult_
  pJobDetails_ =
    GetGeneratedPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        generatedPolicyResult = pGeneratedPolicyResult_,
        jobDetails = pJobDetails_
      }

-- | The response's http status code.
getGeneratedPolicyResponse_httpStatus :: Lens.Lens' GetGeneratedPolicyResponse Prelude.Int
getGeneratedPolicyResponse_httpStatus = Lens.lens (\GetGeneratedPolicyResponse' {httpStatus} -> httpStatus) (\s@GetGeneratedPolicyResponse' {} a -> s {httpStatus = a} :: GetGeneratedPolicyResponse)

-- | A @GeneratedPolicyResult@ object that contains the generated policies
-- and associated details.
getGeneratedPolicyResponse_generatedPolicyResult :: Lens.Lens' GetGeneratedPolicyResponse GeneratedPolicyResult
getGeneratedPolicyResponse_generatedPolicyResult = Lens.lens (\GetGeneratedPolicyResponse' {generatedPolicyResult} -> generatedPolicyResult) (\s@GetGeneratedPolicyResponse' {} a -> s {generatedPolicyResult = a} :: GetGeneratedPolicyResponse)

-- | A @GeneratedPolicyDetails@ object that contains details about the
-- generated policy.
getGeneratedPolicyResponse_jobDetails :: Lens.Lens' GetGeneratedPolicyResponse JobDetails
getGeneratedPolicyResponse_jobDetails = Lens.lens (\GetGeneratedPolicyResponse' {jobDetails} -> jobDetails) (\s@GetGeneratedPolicyResponse' {} a -> s {jobDetails = a} :: GetGeneratedPolicyResponse)

instance Prelude.NFData GetGeneratedPolicyResponse
