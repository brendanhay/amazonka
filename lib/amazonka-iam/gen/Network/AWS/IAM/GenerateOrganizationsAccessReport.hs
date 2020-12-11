{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GenerateOrganizationsAccessReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a report for service last accessed data for AWS Organizations. You can generate a report for any entities (organization root, organizational unit, or account) or policies in your organization.
--
-- To call this operation, you must be signed in using your AWS Organizations master account credentials. You can use your long-term IAM user or root user credentials, or temporary credentials from assuming an IAM role. SCPs must be enabled for your organization root. You must have the required IAM and AWS Organizations permissions. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Refining Permissions Using Service Last Accessed Data> in the /IAM User Guide/ .
-- You can generate a service last accessed data report for entities by specifying only the entity's path. This data includes a list of services that are allowed by any service control policies (SCPs) that apply to the entity.
-- You can generate a service last accessed data report for a policy by specifying an entity's path and an optional AWS Organizations policy ID. This data includes a list of services that are allowed by the specified SCP.
-- For each service in both report types, the data includes the most recent account activity that the policy allows to account principals in the entity or the entity's children. For important information about the data, reporting period, permissions required, troubleshooting, and supported Regions see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing Permissions Using Service Last Accessed Data> in the /IAM User Guide/ .
-- /Important:/ The data includes all attempts to access AWS, not just the successful ones. This includes all attempts that were made using the AWS Management Console, the AWS API through any of the SDKs, or any of the command line tools. An unexpected entry in the service last accessed data does not mean that an account has been compromised, because the request might have been denied. Refer to your CloudTrail logs as the authoritative source for information about all API calls and whether they were successful or denied access. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html Logging IAM Events with CloudTrail> in the /IAM User Guide/ .
-- This operation returns a @JobId@ . Use this parameter in the @'GetOrganizationsAccessReport' @ operation to check the status of the report generation. To check the status of this request, use the @JobId@ parameter in the @'GetOrganizationsAccessReport' @ operation and test the @JobStatus@ response parameter. When the job is complete, you can retrieve the report.
-- To generate a service last accessed data report for entities, specify an entity path without specifying the optional AWS Organizations policy ID. The type of entity that you specify determines the data returned in the report.
--
--     * __Root__ – When you specify the organizations root as the entity, the resulting report lists all of the services allowed by SCPs that are attached to your root. For each service, the report includes data for all accounts in your organization except the master account, because the master account is not limited by SCPs.
--
--
--     * __OU__ – When you specify an organizational unit (OU) as the entity, the resulting report lists all of the services allowed by SCPs that are attached to the OU and its parents. For each service, the report includes data for all accounts in the OU or its children. This data excludes the master account, because the master account is not limited by SCPs.
--
--
--     * __Master account__ – When you specify the master account, the resulting report lists all AWS services, because the master account is not limited by SCPs. For each service, the report includes data for only the master account.
--
--
--     * __Account__ – When you specify another account as the entity, the resulting report lists all of the services allowed by SCPs that are attached to the account and its parents. For each service, the report includes data for only the specified account.
--
--
-- To generate a service last accessed data report for policies, specify an entity path and the optional AWS Organizations policy ID. The type of entity that you specify determines the data returned for each service.
--
--     * __Root__ – When you specify the root entity and a policy ID, the resulting report lists all of the services that are allowed by the specified SCP. For each service, the report includes data for all accounts in your organization to which the SCP applies. This data excludes the master account, because the master account is not limited by SCPs. If the SCP is not attached to any entities in the organization, then the report will return a list of services with no data.
--
--
--     * __OU__ – When you specify an OU entity and a policy ID, the resulting report lists all of the services that are allowed by the specified SCP. For each service, the report includes data for all accounts in the OU or its children to which the SCP applies. This means that other accounts outside the OU that are affected by the SCP might not be included in the data. This data excludes the master account, because the master account is not limited by SCPs. If the SCP is not attached to the OU or one of its children, the report will return a list of services with no data.
--
--
--     * __Master account__ – When you specify the master account, the resulting report lists all AWS services, because the master account is not limited by SCPs. If you specify a policy ID in the CLI or API, the policy is ignored. For each service, the report includes data for only the master account.
--
--
--     * __Account__ – When you specify another account entity and a policy ID, the resulting report lists all of the services that are allowed by the specified SCP. For each service, the report includes data for only the specified account. This means that other accounts in the organization that are affected by the SCP might not be included in the data. If the SCP is not attached to the account, the report will return a list of services with no data.
--
--
-- For more information about service last accessed data, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing Policy Scope by Viewing User Activity> in the /IAM User Guide/ .
module Network.AWS.IAM.GenerateOrganizationsAccessReport
  ( -- * Creating a request
    GenerateOrganizationsAccessReport (..),
    mkGenerateOrganizationsAccessReport,

    -- ** Request lenses
    goarOrganizationsPolicyId,
    goarEntityPath,

    -- * Destructuring the response
    GenerateOrganizationsAccessReportResponse (..),
    mkGenerateOrganizationsAccessReportResponse,

    -- ** Response lenses
    goarrsJobId,
    goarrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGenerateOrganizationsAccessReport' smart constructor.
data GenerateOrganizationsAccessReport = GenerateOrganizationsAccessReport'
  { organizationsPolicyId ::
      Lude.Maybe Lude.Text,
    entityPath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateOrganizationsAccessReport' with the minimum fields required to make a request.
--
-- * 'entityPath' - The path of the AWS Organizations entity (root, OU, or account). You can build an entity path using the known structure of your organization. For example, assume that your account ID is @123456789012@ and its parent OU ID is @ou-rge0-awsabcde@ . The organization root ID is @r-f6g7h8i9j0example@ and your organization ID is @o-a1b2c3d4e5@ . Your entity path is @o-a1b2c3d4e5/r-f6g7h8i9j0example/ou-rge0-awsabcde/123456789012@ .
-- * 'organizationsPolicyId' - The identifier of the AWS Organizations service control policy (SCP). This parameter is optional.
--
-- This ID is used to generate information about when an account principal that is limited by the SCP attempted to access an AWS service.
mkGenerateOrganizationsAccessReport ::
  -- | 'entityPath'
  Lude.Text ->
  GenerateOrganizationsAccessReport
mkGenerateOrganizationsAccessReport pEntityPath_ =
  GenerateOrganizationsAccessReport'
    { organizationsPolicyId =
        Lude.Nothing,
      entityPath = pEntityPath_
    }

-- | The identifier of the AWS Organizations service control policy (SCP). This parameter is optional.
--
-- This ID is used to generate information about when an account principal that is limited by the SCP attempted to access an AWS service.
--
-- /Note:/ Consider using 'organizationsPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarOrganizationsPolicyId :: Lens.Lens' GenerateOrganizationsAccessReport (Lude.Maybe Lude.Text)
goarOrganizationsPolicyId = Lens.lens (organizationsPolicyId :: GenerateOrganizationsAccessReport -> Lude.Maybe Lude.Text) (\s a -> s {organizationsPolicyId = a} :: GenerateOrganizationsAccessReport)
{-# DEPRECATED goarOrganizationsPolicyId "Use generic-lens or generic-optics with 'organizationsPolicyId' instead." #-}

-- | The path of the AWS Organizations entity (root, OU, or account). You can build an entity path using the known structure of your organization. For example, assume that your account ID is @123456789012@ and its parent OU ID is @ou-rge0-awsabcde@ . The organization root ID is @r-f6g7h8i9j0example@ and your organization ID is @o-a1b2c3d4e5@ . Your entity path is @o-a1b2c3d4e5/r-f6g7h8i9j0example/ou-rge0-awsabcde/123456789012@ .
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarEntityPath :: Lens.Lens' GenerateOrganizationsAccessReport Lude.Text
goarEntityPath = Lens.lens (entityPath :: GenerateOrganizationsAccessReport -> Lude.Text) (\s a -> s {entityPath = a} :: GenerateOrganizationsAccessReport)
{-# DEPRECATED goarEntityPath "Use generic-lens or generic-optics with 'entityPath' instead." #-}

instance Lude.AWSRequest GenerateOrganizationsAccessReport where
  type
    Rs GenerateOrganizationsAccessReport =
      GenerateOrganizationsAccessReportResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GenerateOrganizationsAccessReportResult"
      ( \s h x ->
          GenerateOrganizationsAccessReportResponse'
            Lude.<$> (x Lude..@? "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateOrganizationsAccessReport where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GenerateOrganizationsAccessReport where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateOrganizationsAccessReport where
  toQuery GenerateOrganizationsAccessReport' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GenerateOrganizationsAccessReport" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "OrganizationsPolicyId" Lude.=: organizationsPolicyId,
        "EntityPath" Lude.=: entityPath
      ]

-- | /See:/ 'mkGenerateOrganizationsAccessReportResponse' smart constructor.
data GenerateOrganizationsAccessReportResponse = GenerateOrganizationsAccessReportResponse'
  { jobId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateOrganizationsAccessReportResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The job identifier that you can use in the 'GetOrganizationsAccessReport' operation.
-- * 'responseStatus' - The response status code.
mkGenerateOrganizationsAccessReportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateOrganizationsAccessReportResponse
mkGenerateOrganizationsAccessReportResponse pResponseStatus_ =
  GenerateOrganizationsAccessReportResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job identifier that you can use in the 'GetOrganizationsAccessReport' operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarrsJobId :: Lens.Lens' GenerateOrganizationsAccessReportResponse (Lude.Maybe Lude.Text)
goarrsJobId = Lens.lens (jobId :: GenerateOrganizationsAccessReportResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: GenerateOrganizationsAccessReportResponse)
{-# DEPRECATED goarrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarrsResponseStatus :: Lens.Lens' GenerateOrganizationsAccessReportResponse Lude.Int
goarrsResponseStatus = Lens.lens (responseStatus :: GenerateOrganizationsAccessReportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateOrganizationsAccessReportResponse)
{-# DEPRECATED goarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
