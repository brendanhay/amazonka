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
-- Module      : Network.AWS.IAM.GenerateOrganizationsAccessReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a report for service last accessed data for AWS Organizations.
-- You can generate a report for any entities (organization root,
-- organizational unit, or account) or policies in your organization.
--
-- To call this operation, you must be signed in using your AWS
-- Organizations management account credentials. You can use your long-term
-- IAM user or root user credentials, or temporary credentials from
-- assuming an IAM role. SCPs must be enabled for your organization root.
-- You must have the required IAM and AWS Organizations permissions. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Refining permissions using service last accessed data>
-- in the /IAM User Guide/.
--
-- You can generate a service last accessed data report for entities by
-- specifying only the entity\'s path. This data includes a list of
-- services that are allowed by any service control policies (SCPs) that
-- apply to the entity.
--
-- You can generate a service last accessed data report for a policy by
-- specifying an entity\'s path and an optional AWS Organizations policy
-- ID. This data includes a list of services that are allowed by the
-- specified SCP.
--
-- For each service in both report types, the data includes the most recent
-- account activity that the policy allows to account principals in the
-- entity or the entity\'s children. For important information about the
-- data, reporting period, permissions required, troubleshooting, and
-- supported Regions see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing permissions using service last accessed data>
-- in the /IAM User Guide/.
--
-- The data includes all attempts to access AWS, not just the successful
-- ones. This includes all attempts that were made using the AWS Management
-- Console, the AWS API through any of the SDKs, or any of the command line
-- tools. An unexpected entry in the service last accessed data does not
-- mean that an account has been compromised, because the request might
-- have been denied. Refer to your CloudTrail logs as the authoritative
-- source for information about all API calls and whether they were
-- successful or denied access. For more information,
-- see <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html Logging IAM events with CloudTrail>
-- in the /IAM User Guide/.
--
-- This operation returns a @JobId@. Use this parameter in the
-- @ GetOrganizationsAccessReport @ operation to check the status of the
-- report generation. To check the status of this request, use the @JobId@
-- parameter in the @ GetOrganizationsAccessReport @ operation and test the
-- @JobStatus@ response parameter. When the job is complete, you can
-- retrieve the report.
--
-- To generate a service last accessed data report for entities, specify an
-- entity path without specifying the optional AWS Organizations policy ID.
-- The type of entity that you specify determines the data returned in the
-- report.
--
-- -   __Root__ – When you specify the organizations root as the entity,
--     the resulting report lists all of the services allowed by SCPs that
--     are attached to your root. For each service, the report includes
--     data for all accounts in your organization except the management
--     account, because the management account is not limited by SCPs.
--
-- -   __OU__ – When you specify an organizational unit (OU) as the entity,
--     the resulting report lists all of the services allowed by SCPs that
--     are attached to the OU and its parents. For each service, the report
--     includes data for all accounts in the OU or its children. This data
--     excludes the management account, because the management account is
--     not limited by SCPs.
--
-- -   __management account__ – When you specify the management account,
--     the resulting report lists all AWS services, because the management
--     account is not limited by SCPs. For each service, the report
--     includes data for only the management account.
--
-- -   __Account__ – When you specify another account as the entity, the
--     resulting report lists all of the services allowed by SCPs that are
--     attached to the account and its parents. For each service, the
--     report includes data for only the specified account.
--
-- To generate a service last accessed data report for policies, specify an
-- entity path and the optional AWS Organizations policy ID. The type of
-- entity that you specify determines the data returned for each service.
--
-- -   __Root__ – When you specify the root entity and a policy ID, the
--     resulting report lists all of the services that are allowed by the
--     specified SCP. For each service, the report includes data for all
--     accounts in your organization to which the SCP applies. This data
--     excludes the management account, because the management account is
--     not limited by SCPs. If the SCP is not attached to any entities in
--     the organization, then the report will return a list of services
--     with no data.
--
-- -   __OU__ – When you specify an OU entity and a policy ID, the
--     resulting report lists all of the services that are allowed by the
--     specified SCP. For each service, the report includes data for all
--     accounts in the OU or its children to which the SCP applies. This
--     means that other accounts outside the OU that are affected by the
--     SCP might not be included in the data. This data excludes the
--     management account, because the management account is not limited by
--     SCPs. If the SCP is not attached to the OU or one of its children,
--     the report will return a list of services with no data.
--
-- -   __management account__ – When you specify the management account,
--     the resulting report lists all AWS services, because the management
--     account is not limited by SCPs. If you specify a policy ID in the
--     CLI or API, the policy is ignored. For each service, the report
--     includes data for only the management account.
--
-- -   __Account__ – When you specify another account entity and a policy
--     ID, the resulting report lists all of the services that are allowed
--     by the specified SCP. For each service, the report includes data for
--     only the specified account. This means that other accounts in the
--     organization that are affected by the SCP might not be included in
--     the data. If the SCP is not attached to the account, the report will
--     return a list of services with no data.
--
-- Service last accessed data does not use other policy types when
-- determining whether a principal could access a service. These other
-- policy types include identity-based policies, resource-based policies,
-- access control lists, IAM permissions boundaries, and STS assume role
-- policies. It only applies SCP logic. For more about the evaluation of
-- policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating policies>
-- in the /IAM User Guide/.
--
-- For more information about service last accessed data, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing policy scope by viewing user activity>
-- in the /IAM User Guide/.
module Network.AWS.IAM.GenerateOrganizationsAccessReport
  ( -- * Creating a Request
    GenerateOrganizationsAccessReport (..),
    newGenerateOrganizationsAccessReport,

    -- * Request Lenses
    generateOrganizationsAccessReport_organizationsPolicyId,
    generateOrganizationsAccessReport_entityPath,

    -- * Destructuring the Response
    GenerateOrganizationsAccessReportResponse (..),
    newGenerateOrganizationsAccessReportResponse,

    -- * Response Lenses
    generateOrganizationsAccessReportResponse_jobId,
    generateOrganizationsAccessReportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGenerateOrganizationsAccessReport' smart constructor.
data GenerateOrganizationsAccessReport = GenerateOrganizationsAccessReport'
  { -- | The identifier of the AWS Organizations service control policy (SCP).
    -- This parameter is optional.
    --
    -- This ID is used to generate information about when an account principal
    -- that is limited by the SCP attempted to access an AWS service.
    organizationsPolicyId :: Core.Maybe Core.Text,
    -- | The path of the AWS Organizations entity (root, OU, or account). You can
    -- build an entity path using the known structure of your organization. For
    -- example, assume that your account ID is @123456789012@ and its parent OU
    -- ID is @ou-rge0-awsabcde@. The organization root ID is
    -- @r-f6g7h8i9j0example@ and your organization ID is @o-a1b2c3d4e5@. Your
    -- entity path is
    -- @o-a1b2c3d4e5\/r-f6g7h8i9j0example\/ou-rge0-awsabcde\/123456789012@.
    entityPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateOrganizationsAccessReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationsPolicyId', 'generateOrganizationsAccessReport_organizationsPolicyId' - The identifier of the AWS Organizations service control policy (SCP).
-- This parameter is optional.
--
-- This ID is used to generate information about when an account principal
-- that is limited by the SCP attempted to access an AWS service.
--
-- 'entityPath', 'generateOrganizationsAccessReport_entityPath' - The path of the AWS Organizations entity (root, OU, or account). You can
-- build an entity path using the known structure of your organization. For
-- example, assume that your account ID is @123456789012@ and its parent OU
-- ID is @ou-rge0-awsabcde@. The organization root ID is
-- @r-f6g7h8i9j0example@ and your organization ID is @o-a1b2c3d4e5@. Your
-- entity path is
-- @o-a1b2c3d4e5\/r-f6g7h8i9j0example\/ou-rge0-awsabcde\/123456789012@.
newGenerateOrganizationsAccessReport ::
  -- | 'entityPath'
  Core.Text ->
  GenerateOrganizationsAccessReport
newGenerateOrganizationsAccessReport pEntityPath_ =
  GenerateOrganizationsAccessReport'
    { organizationsPolicyId =
        Core.Nothing,
      entityPath = pEntityPath_
    }

-- | The identifier of the AWS Organizations service control policy (SCP).
-- This parameter is optional.
--
-- This ID is used to generate information about when an account principal
-- that is limited by the SCP attempted to access an AWS service.
generateOrganizationsAccessReport_organizationsPolicyId :: Lens.Lens' GenerateOrganizationsAccessReport (Core.Maybe Core.Text)
generateOrganizationsAccessReport_organizationsPolicyId = Lens.lens (\GenerateOrganizationsAccessReport' {organizationsPolicyId} -> organizationsPolicyId) (\s@GenerateOrganizationsAccessReport' {} a -> s {organizationsPolicyId = a} :: GenerateOrganizationsAccessReport)

-- | The path of the AWS Organizations entity (root, OU, or account). You can
-- build an entity path using the known structure of your organization. For
-- example, assume that your account ID is @123456789012@ and its parent OU
-- ID is @ou-rge0-awsabcde@. The organization root ID is
-- @r-f6g7h8i9j0example@ and your organization ID is @o-a1b2c3d4e5@. Your
-- entity path is
-- @o-a1b2c3d4e5\/r-f6g7h8i9j0example\/ou-rge0-awsabcde\/123456789012@.
generateOrganizationsAccessReport_entityPath :: Lens.Lens' GenerateOrganizationsAccessReport Core.Text
generateOrganizationsAccessReport_entityPath = Lens.lens (\GenerateOrganizationsAccessReport' {entityPath} -> entityPath) (\s@GenerateOrganizationsAccessReport' {} a -> s {entityPath = a} :: GenerateOrganizationsAccessReport)

instance
  Core.AWSRequest
    GenerateOrganizationsAccessReport
  where
  type
    AWSResponse GenerateOrganizationsAccessReport =
      GenerateOrganizationsAccessReportResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GenerateOrganizationsAccessReportResult"
      ( \s h x ->
          GenerateOrganizationsAccessReportResponse'
            Core.<$> (x Core..@? "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GenerateOrganizationsAccessReport

instance
  Core.NFData
    GenerateOrganizationsAccessReport

instance
  Core.ToHeaders
    GenerateOrganizationsAccessReport
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GenerateOrganizationsAccessReport
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GenerateOrganizationsAccessReport
  where
  toQuery GenerateOrganizationsAccessReport' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GenerateOrganizationsAccessReport" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "OrganizationsPolicyId"
          Core.=: organizationsPolicyId,
        "EntityPath" Core.=: entityPath
      ]

-- | /See:/ 'newGenerateOrganizationsAccessReportResponse' smart constructor.
data GenerateOrganizationsAccessReportResponse = GenerateOrganizationsAccessReportResponse'
  { -- | The job identifier that you can use in the GetOrganizationsAccessReport
    -- operation.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateOrganizationsAccessReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'generateOrganizationsAccessReportResponse_jobId' - The job identifier that you can use in the GetOrganizationsAccessReport
-- operation.
--
-- 'httpStatus', 'generateOrganizationsAccessReportResponse_httpStatus' - The response's http status code.
newGenerateOrganizationsAccessReportResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GenerateOrganizationsAccessReportResponse
newGenerateOrganizationsAccessReportResponse
  pHttpStatus_ =
    GenerateOrganizationsAccessReportResponse'
      { jobId =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The job identifier that you can use in the GetOrganizationsAccessReport
-- operation.
generateOrganizationsAccessReportResponse_jobId :: Lens.Lens' GenerateOrganizationsAccessReportResponse (Core.Maybe Core.Text)
generateOrganizationsAccessReportResponse_jobId = Lens.lens (\GenerateOrganizationsAccessReportResponse' {jobId} -> jobId) (\s@GenerateOrganizationsAccessReportResponse' {} a -> s {jobId = a} :: GenerateOrganizationsAccessReportResponse)

-- | The response's http status code.
generateOrganizationsAccessReportResponse_httpStatus :: Lens.Lens' GenerateOrganizationsAccessReportResponse Core.Int
generateOrganizationsAccessReportResponse_httpStatus = Lens.lens (\GenerateOrganizationsAccessReportResponse' {httpStatus} -> httpStatus) (\s@GenerateOrganizationsAccessReportResponse' {} a -> s {httpStatus = a} :: GenerateOrganizationsAccessReportResponse)

instance
  Core.NFData
    GenerateOrganizationsAccessReportResponse
