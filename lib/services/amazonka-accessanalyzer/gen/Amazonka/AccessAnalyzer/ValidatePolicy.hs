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
-- Module      : Amazonka.AccessAnalyzer.ValidatePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the validation of a policy and returns a list of findings. The
-- findings help you identify issues and provide actionable recommendations
-- to resolve the issue and enable you to author functional policies that
-- meet security best practices.
--
-- This operation returns paginated results.
module Amazonka.AccessAnalyzer.ValidatePolicy
  ( -- * Creating a Request
    ValidatePolicy (..),
    newValidatePolicy,

    -- * Request Lenses
    validatePolicy_locale,
    validatePolicy_nextToken,
    validatePolicy_maxResults,
    validatePolicy_policyDocument,
    validatePolicy_policyType,

    -- * Destructuring the Response
    ValidatePolicyResponse (..),
    newValidatePolicyResponse,

    -- * Response Lenses
    validatePolicyResponse_nextToken,
    validatePolicyResponse_httpStatus,
    validatePolicyResponse_findings,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newValidatePolicy' smart constructor.
data ValidatePolicy = ValidatePolicy'
  { -- | The locale to use for localizing the findings.
    locale :: Prelude.Maybe Locale,
    -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The JSON policy document to use as the content for the policy.
    policyDocument :: Prelude.Text,
    -- | The type of policy to validate. Identity policies grant permissions to
    -- IAM principals. Identity policies include managed and inline policies
    -- for IAM roles, users, and groups. They also include service-control
    -- policies (SCPs) that are attached to an Amazon Web Services
    -- organization, organizational unit (OU), or an account.
    --
    -- Resource policies grant permissions on Amazon Web Services resources.
    -- Resource policies include trust policies for IAM roles and bucket
    -- policies for Amazon S3 buckets. You can provide a generic input such as
    -- identity policy or resource policy or a specific input such as managed
    -- policy or Amazon S3 bucket policy.
    policyType :: PolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'validatePolicy_locale' - The locale to use for localizing the findings.
--
-- 'nextToken', 'validatePolicy_nextToken' - A token used for pagination of results returned.
--
-- 'maxResults', 'validatePolicy_maxResults' - The maximum number of results to return in the response.
--
-- 'policyDocument', 'validatePolicy_policyDocument' - The JSON policy document to use as the content for the policy.
--
-- 'policyType', 'validatePolicy_policyType' - The type of policy to validate. Identity policies grant permissions to
-- IAM principals. Identity policies include managed and inline policies
-- for IAM roles, users, and groups. They also include service-control
-- policies (SCPs) that are attached to an Amazon Web Services
-- organization, organizational unit (OU), or an account.
--
-- Resource policies grant permissions on Amazon Web Services resources.
-- Resource policies include trust policies for IAM roles and bucket
-- policies for Amazon S3 buckets. You can provide a generic input such as
-- identity policy or resource policy or a specific input such as managed
-- policy or Amazon S3 bucket policy.
newValidatePolicy ::
  -- | 'policyDocument'
  Prelude.Text ->
  -- | 'policyType'
  PolicyType ->
  ValidatePolicy
newValidatePolicy pPolicyDocument_ pPolicyType_ =
  ValidatePolicy'
    { locale = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      policyDocument = pPolicyDocument_,
      policyType = pPolicyType_
    }

-- | The locale to use for localizing the findings.
validatePolicy_locale :: Lens.Lens' ValidatePolicy (Prelude.Maybe Locale)
validatePolicy_locale = Lens.lens (\ValidatePolicy' {locale} -> locale) (\s@ValidatePolicy' {} a -> s {locale = a} :: ValidatePolicy)

-- | A token used for pagination of results returned.
validatePolicy_nextToken :: Lens.Lens' ValidatePolicy (Prelude.Maybe Prelude.Text)
validatePolicy_nextToken = Lens.lens (\ValidatePolicy' {nextToken} -> nextToken) (\s@ValidatePolicy' {} a -> s {nextToken = a} :: ValidatePolicy)

-- | The maximum number of results to return in the response.
validatePolicy_maxResults :: Lens.Lens' ValidatePolicy (Prelude.Maybe Prelude.Int)
validatePolicy_maxResults = Lens.lens (\ValidatePolicy' {maxResults} -> maxResults) (\s@ValidatePolicy' {} a -> s {maxResults = a} :: ValidatePolicy)

-- | The JSON policy document to use as the content for the policy.
validatePolicy_policyDocument :: Lens.Lens' ValidatePolicy Prelude.Text
validatePolicy_policyDocument = Lens.lens (\ValidatePolicy' {policyDocument} -> policyDocument) (\s@ValidatePolicy' {} a -> s {policyDocument = a} :: ValidatePolicy)

-- | The type of policy to validate. Identity policies grant permissions to
-- IAM principals. Identity policies include managed and inline policies
-- for IAM roles, users, and groups. They also include service-control
-- policies (SCPs) that are attached to an Amazon Web Services
-- organization, organizational unit (OU), or an account.
--
-- Resource policies grant permissions on Amazon Web Services resources.
-- Resource policies include trust policies for IAM roles and bucket
-- policies for Amazon S3 buckets. You can provide a generic input such as
-- identity policy or resource policy or a specific input such as managed
-- policy or Amazon S3 bucket policy.
validatePolicy_policyType :: Lens.Lens' ValidatePolicy PolicyType
validatePolicy_policyType = Lens.lens (\ValidatePolicy' {policyType} -> policyType) (\s@ValidatePolicy' {} a -> s {policyType = a} :: ValidatePolicy)

instance Core.AWSPager ValidatePolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? validatePolicyResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. validatePolicyResponse_findings) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& validatePolicy_nextToken
          Lens..~ rs
          Lens.^? validatePolicyResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ValidatePolicy where
  type
    AWSResponse ValidatePolicy =
      ValidatePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidatePolicyResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "findings" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ValidatePolicy where
  hashWithSalt salt' ValidatePolicy' {..} =
    salt' `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` locale

instance Prelude.NFData ValidatePolicy where
  rnf ValidatePolicy' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ValidatePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ValidatePolicy where
  toJSON ValidatePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("locale" Core..=) Prelude.<$> locale,
            Prelude.Just
              ("policyDocument" Core..= policyDocument),
            Prelude.Just ("policyType" Core..= policyType)
          ]
      )

instance Core.ToPath ValidatePolicy where
  toPath = Prelude.const "/policy/validation"

instance Core.ToQuery ValidatePolicy where
  toQuery ValidatePolicy' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newValidatePolicyResponse' smart constructor.
data ValidatePolicyResponse = ValidatePolicyResponse'
  { -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of findings in a policy returned by IAM Access Analyzer based
    -- on its suite of policy checks.
    findings :: [ValidatePolicyFinding]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidatePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'validatePolicyResponse_nextToken' - A token used for pagination of results returned.
--
-- 'httpStatus', 'validatePolicyResponse_httpStatus' - The response's http status code.
--
-- 'findings', 'validatePolicyResponse_findings' - The list of findings in a policy returned by IAM Access Analyzer based
-- on its suite of policy checks.
newValidatePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidatePolicyResponse
newValidatePolicyResponse pHttpStatus_ =
  ValidatePolicyResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      findings = Prelude.mempty
    }

-- | A token used for pagination of results returned.
validatePolicyResponse_nextToken :: Lens.Lens' ValidatePolicyResponse (Prelude.Maybe Prelude.Text)
validatePolicyResponse_nextToken = Lens.lens (\ValidatePolicyResponse' {nextToken} -> nextToken) (\s@ValidatePolicyResponse' {} a -> s {nextToken = a} :: ValidatePolicyResponse)

-- | The response's http status code.
validatePolicyResponse_httpStatus :: Lens.Lens' ValidatePolicyResponse Prelude.Int
validatePolicyResponse_httpStatus = Lens.lens (\ValidatePolicyResponse' {httpStatus} -> httpStatus) (\s@ValidatePolicyResponse' {} a -> s {httpStatus = a} :: ValidatePolicyResponse)

-- | The list of findings in a policy returned by IAM Access Analyzer based
-- on its suite of policy checks.
validatePolicyResponse_findings :: Lens.Lens' ValidatePolicyResponse [ValidatePolicyFinding]
validatePolicyResponse_findings = Lens.lens (\ValidatePolicyResponse' {findings} -> findings) (\s@ValidatePolicyResponse' {} a -> s {findings = a} :: ValidatePolicyResponse) Prelude.. Lens.coerced

instance Prelude.NFData ValidatePolicyResponse where
  rnf ValidatePolicyResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf findings
      `Prelude.seq` Prelude.rnf httpStatus
