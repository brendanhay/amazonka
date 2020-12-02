{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
--
-- To call this operation, you must be signed in using your AWS Organizations master account credentials. You can use your long-term IAM user or root user credentials, or temporary credentials from assuming an IAM role. SCPs must be enabled for your organization root. You must have the required IAM and AWS Organizations permissions. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Refining Permissions Using Service Last Accessed Data> in the /IAM User Guide/ .
--
-- You can generate a service last accessed data report for entities by specifying only the entity's path. This data includes a list of services that are allowed by any service control policies (SCPs) that apply to the entity.
--
-- You can generate a service last accessed data report for a policy by specifying an entity's path and an optional AWS Organizations policy ID. This data includes a list of services that are allowed by the specified SCP.
--
-- For each service in both report types, the data includes the most recent account activity that the policy allows to account principals in the entity or the entity's children. For important information about the data, reporting period, permissions required, troubleshooting, and supported Regions see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing Permissions Using Service Last Accessed Data> in the /IAM User Guide/ .
--
-- /Important:/ The data includes all attempts to access AWS, not just the successful ones. This includes all attempts that were made using the AWS Management Console, the AWS API through any of the SDKs, or any of the command line tools. An unexpected entry in the service last accessed data does not mean that an account has been compromised, because the request might have been denied. Refer to your CloudTrail logs as the authoritative source for information about all API calls and whether they were successful or denied access. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html Logging IAM Events with CloudTrail> in the /IAM User Guide/ .
--
-- This operation returns a @JobId@ . Use this parameter in the @'GetOrganizationsAccessReport' @ operation to check the status of the report generation. To check the status of this request, use the @JobId@ parameter in the @'GetOrganizationsAccessReport' @ operation and test the @JobStatus@ response parameter. When the job is complete, you can retrieve the report.
--
-- To generate a service last accessed data report for entities, specify an entity path without specifying the optional AWS Organizations policy ID. The type of entity that you specify determines the data returned in the report.
--
--     * __Root__ – When you specify the organizations root as the entity, the resulting report lists all of the services allowed by SCPs that are attached to your root. For each service, the report includes data for all accounts in your organization except the master account, because the master account is not limited by SCPs.
--
--     * __OU__ – When you specify an organizational unit (OU) as the entity, the resulting report lists all of the services allowed by SCPs that are attached to the OU and its parents. For each service, the report includes data for all accounts in the OU or its children. This data excludes the master account, because the master account is not limited by SCPs.
--
--     * __Master account__ – When you specify the master account, the resulting report lists all AWS services, because the master account is not limited by SCPs. For each service, the report includes data for only the master account.
--
--     * __Account__ – When you specify another account as the entity, the resulting report lists all of the services allowed by SCPs that are attached to the account and its parents. For each service, the report includes data for only the specified account.
--
--
--
-- To generate a service last accessed data report for policies, specify an entity path and the optional AWS Organizations policy ID. The type of entity that you specify determines the data returned for each service.
--
--     * __Root__ – When you specify the root entity and a policy ID, the resulting report lists all of the services that are allowed by the specified SCP. For each service, the report includes data for all accounts in your organization to which the SCP applies. This data excludes the master account, because the master account is not limited by SCPs. If the SCP is not attached to any entities in the organization, then the report will return a list of services with no data.
--
--     * __OU__ – When you specify an OU entity and a policy ID, the resulting report lists all of the services that are allowed by the specified SCP. For each service, the report includes data for all accounts in the OU or its children to which the SCP applies. This means that other accounts outside the OU that are affected by the SCP might not be included in the data. This data excludes the master account, because the master account is not limited by SCPs. If the SCP is not attached to the OU or one of its children, the report will return a list of services with no data.
--
--     * __Master account__ – When you specify the master account, the resulting report lists all AWS services, because the master account is not limited by SCPs. If you specify a policy ID in the CLI or API, the policy is ignored. For each service, the report includes data for only the master account.
--
--     * __Account__ – When you specify another account entity and a policy ID, the resulting report lists all of the services that are allowed by the specified SCP. For each service, the report includes data for only the specified account. This means that other accounts in the organization that are affected by the SCP might not be included in the data. If the SCP is not attached to the account, the report will return a list of services with no data.
--
--
--
-- For more information about service last accessed data, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing Policy Scope by Viewing User Activity> in the /IAM User Guide/ .
module Network.AWS.IAM.GenerateOrganizationsAccessReport
  ( -- * Creating a Request
    generateOrganizationsAccessReport,
    GenerateOrganizationsAccessReport,

    -- * Request Lenses
    goarOrganizationsPolicyId,
    goarEntityPath,

    -- * Destructuring the Response
    generateOrganizationsAccessReportResponse,
    GenerateOrganizationsAccessReportResponse,

    -- * Response Lenses
    goarrsJobId,
    goarrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateOrganizationsAccessReport' smart constructor.
data GenerateOrganizationsAccessReport = GenerateOrganizationsAccessReport'
  { _goarOrganizationsPolicyId ::
      !(Maybe Text),
    _goarEntityPath ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateOrganizationsAccessReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goarOrganizationsPolicyId' - The identifier of the AWS Organizations service control policy (SCP). This parameter is optional. This ID is used to generate information about when an account principal that is limited by the SCP attempted to access an AWS service.
--
-- * 'goarEntityPath' - The path of the AWS Organizations entity (root, OU, or account). You can build an entity path using the known structure of your organization. For example, assume that your account ID is @123456789012@ and its parent OU ID is @ou-rge0-awsabcde@ . The organization root ID is @r-f6g7h8i9j0example@ and your organization ID is @o-a1b2c3d4e5@ . Your entity path is @o-a1b2c3d4e5/r-f6g7h8i9j0example/ou-rge0-awsabcde/123456789012@ .
generateOrganizationsAccessReport ::
  -- | 'goarEntityPath'
  Text ->
  GenerateOrganizationsAccessReport
generateOrganizationsAccessReport pEntityPath_ =
  GenerateOrganizationsAccessReport'
    { _goarOrganizationsPolicyId =
        Nothing,
      _goarEntityPath = pEntityPath_
    }

-- | The identifier of the AWS Organizations service control policy (SCP). This parameter is optional. This ID is used to generate information about when an account principal that is limited by the SCP attempted to access an AWS service.
goarOrganizationsPolicyId :: Lens' GenerateOrganizationsAccessReport (Maybe Text)
goarOrganizationsPolicyId = lens _goarOrganizationsPolicyId (\s a -> s {_goarOrganizationsPolicyId = a})

-- | The path of the AWS Organizations entity (root, OU, or account). You can build an entity path using the known structure of your organization. For example, assume that your account ID is @123456789012@ and its parent OU ID is @ou-rge0-awsabcde@ . The organization root ID is @r-f6g7h8i9j0example@ and your organization ID is @o-a1b2c3d4e5@ . Your entity path is @o-a1b2c3d4e5/r-f6g7h8i9j0example/ou-rge0-awsabcde/123456789012@ .
goarEntityPath :: Lens' GenerateOrganizationsAccessReport Text
goarEntityPath = lens _goarEntityPath (\s a -> s {_goarEntityPath = a})

instance AWSRequest GenerateOrganizationsAccessReport where
  type
    Rs GenerateOrganizationsAccessReport =
      GenerateOrganizationsAccessReportResponse
  request = postQuery iam
  response =
    receiveXMLWrapper
      "GenerateOrganizationsAccessReportResult"
      ( \s h x ->
          GenerateOrganizationsAccessReportResponse'
            <$> (x .@? "JobId") <*> (pure (fromEnum s))
      )

instance Hashable GenerateOrganizationsAccessReport

instance NFData GenerateOrganizationsAccessReport

instance ToHeaders GenerateOrganizationsAccessReport where
  toHeaders = const mempty

instance ToPath GenerateOrganizationsAccessReport where
  toPath = const "/"

instance ToQuery GenerateOrganizationsAccessReport where
  toQuery GenerateOrganizationsAccessReport' {..} =
    mconcat
      [ "Action" =: ("GenerateOrganizationsAccessReport" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "OrganizationsPolicyId" =: _goarOrganizationsPolicyId,
        "EntityPath" =: _goarEntityPath
      ]

-- | /See:/ 'generateOrganizationsAccessReportResponse' smart constructor.
data GenerateOrganizationsAccessReportResponse = GenerateOrganizationsAccessReportResponse'
  { _goarrsJobId ::
      !( Maybe
           Text
       ),
    _goarrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GenerateOrganizationsAccessReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goarrsJobId' - The job identifier that you can use in the 'GetOrganizationsAccessReport' operation.
--
-- * 'goarrsResponseStatus' - -- | The response status code.
generateOrganizationsAccessReportResponse ::
  -- | 'goarrsResponseStatus'
  Int ->
  GenerateOrganizationsAccessReportResponse
generateOrganizationsAccessReportResponse pResponseStatus_ =
  GenerateOrganizationsAccessReportResponse'
    { _goarrsJobId =
        Nothing,
      _goarrsResponseStatus = pResponseStatus_
    }

-- | The job identifier that you can use in the 'GetOrganizationsAccessReport' operation.
goarrsJobId :: Lens' GenerateOrganizationsAccessReportResponse (Maybe Text)
goarrsJobId = lens _goarrsJobId (\s a -> s {_goarrsJobId = a})

-- | -- | The response status code.
goarrsResponseStatus :: Lens' GenerateOrganizationsAccessReportResponse Int
goarrsResponseStatus = lens _goarrsResponseStatus (\s a -> s {_goarrsResponseStatus = a})

instance NFData GenerateOrganizationsAccessReportResponse
