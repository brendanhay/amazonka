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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    validatePolicy_maxResults,
    validatePolicy_nextToken,
    validatePolicy_validatePolicyResourceType,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newValidatePolicy' smart constructor.
data ValidatePolicy = ValidatePolicy'
  { -- | The locale to use for localizing the findings.
    locale :: Prelude.Maybe Locale,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of resource to attach to your resource policy. Specify a value
    -- for the policy validation resource type only if the policy type is
    -- @RESOURCE_POLICY@. For example, to validate a resource policy to attach
    -- to an Amazon S3 bucket, you can choose @AWS::S3::Bucket@ for the policy
    -- validation resource type.
    --
    -- For resource types not supported as valid values, IAM Access Analyzer
    -- runs policy checks that apply to all resource policies. For example, to
    -- validate a resource policy to attach to a KMS key, do not specify a
    -- value for the policy validation resource type and IAM Access Analyzer
    -- will run policy checks that apply to all resource policies.
    validatePolicyResourceType :: Prelude.Maybe ValidatePolicyResourceType,
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
-- 'maxResults', 'validatePolicy_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'validatePolicy_nextToken' - A token used for pagination of results returned.
--
-- 'validatePolicyResourceType', 'validatePolicy_validatePolicyResourceType' - The type of resource to attach to your resource policy. Specify a value
-- for the policy validation resource type only if the policy type is
-- @RESOURCE_POLICY@. For example, to validate a resource policy to attach
-- to an Amazon S3 bucket, you can choose @AWS::S3::Bucket@ for the policy
-- validation resource type.
--
-- For resource types not supported as valid values, IAM Access Analyzer
-- runs policy checks that apply to all resource policies. For example, to
-- validate a resource policy to attach to a KMS key, do not specify a
-- value for the policy validation resource type and IAM Access Analyzer
-- will run policy checks that apply to all resource policies.
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
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      validatePolicyResourceType = Prelude.Nothing,
      policyDocument = pPolicyDocument_,
      policyType = pPolicyType_
    }

-- | The locale to use for localizing the findings.
validatePolicy_locale :: Lens.Lens' ValidatePolicy (Prelude.Maybe Locale)
validatePolicy_locale = Lens.lens (\ValidatePolicy' {locale} -> locale) (\s@ValidatePolicy' {} a -> s {locale = a} :: ValidatePolicy)

-- | The maximum number of results to return in the response.
validatePolicy_maxResults :: Lens.Lens' ValidatePolicy (Prelude.Maybe Prelude.Int)
validatePolicy_maxResults = Lens.lens (\ValidatePolicy' {maxResults} -> maxResults) (\s@ValidatePolicy' {} a -> s {maxResults = a} :: ValidatePolicy)

-- | A token used for pagination of results returned.
validatePolicy_nextToken :: Lens.Lens' ValidatePolicy (Prelude.Maybe Prelude.Text)
validatePolicy_nextToken = Lens.lens (\ValidatePolicy' {nextToken} -> nextToken) (\s@ValidatePolicy' {} a -> s {nextToken = a} :: ValidatePolicy)

-- | The type of resource to attach to your resource policy. Specify a value
-- for the policy validation resource type only if the policy type is
-- @RESOURCE_POLICY@. For example, to validate a resource policy to attach
-- to an Amazon S3 bucket, you can choose @AWS::S3::Bucket@ for the policy
-- validation resource type.
--
-- For resource types not supported as valid values, IAM Access Analyzer
-- runs policy checks that apply to all resource policies. For example, to
-- validate a resource policy to attach to a KMS key, do not specify a
-- value for the policy validation resource type and IAM Access Analyzer
-- will run policy checks that apply to all resource policies.
validatePolicy_validatePolicyResourceType :: Lens.Lens' ValidatePolicy (Prelude.Maybe ValidatePolicyResourceType)
validatePolicy_validatePolicyResourceType = Lens.lens (\ValidatePolicy' {validatePolicyResourceType} -> validatePolicyResourceType) (\s@ValidatePolicy' {} a -> s {validatePolicyResourceType = a} :: ValidatePolicy)

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
              Lens.^? validatePolicyResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ValidatePolicy where
  type
    AWSResponse ValidatePolicy =
      ValidatePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidatePolicyResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "findings" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ValidatePolicy where
  hashWithSalt _salt ValidatePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` validatePolicyResourceType
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData ValidatePolicy where
  rnf ValidatePolicy' {..} =
    Prelude.rnf locale `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf validatePolicyResourceType `Prelude.seq`
            Prelude.rnf policyDocument `Prelude.seq`
              Prelude.rnf policyType

instance Data.ToHeaders ValidatePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ValidatePolicy where
  toJSON ValidatePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("locale" Data..=) Prelude.<$> locale,
            ("validatePolicyResourceType" Data..=)
              Prelude.<$> validatePolicyResourceType,
            Prelude.Just
              ("policyDocument" Data..= policyDocument),
            Prelude.Just ("policyType" Data..= policyType)
          ]
      )

instance Data.ToPath ValidatePolicy where
  toPath = Prelude.const "/policy/validation"

instance Data.ToQuery ValidatePolicy where
  toQuery ValidatePolicy' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
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
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf findings
