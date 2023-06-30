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
-- Module      : Amazonka.IAM.GenerateServiceLastAccessedDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a report that includes details about when an IAM resource
-- (user, group, role, or policy) was last used in an attempt to access
-- Amazon Web Services services. Recent activity usually appears within
-- four hours. IAM reports activity for at least the last 400 days, or less
-- if your Region began supporting this feature within the last year. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>.
--
-- The service last accessed data includes all attempts to access an Amazon
-- Web Services API, not just the successful ones. This includes all
-- attempts that were made using the Amazon Web Services Management
-- Console, the Amazon Web Services API through any of the SDKs, or any of
-- the command line tools. An unexpected entry in the service last accessed
-- data does not mean that your account has been compromised, because the
-- request might have been denied. Refer to your CloudTrail logs as the
-- authoritative source for information about all API calls and whether
-- they were successful or denied access. For more information,
-- see <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html Logging IAM events with CloudTrail>
-- in the /IAM User Guide/.
--
-- The @GenerateServiceLastAccessedDetails@ operation returns a @JobId@.
-- Use this parameter in the following operations to retrieve the following
-- details from your report:
--
-- -   GetServiceLastAccessedDetails – Use this operation for users,
--     groups, roles, or policies to list every Amazon Web Services service
--     that the resource could access using permissions policies. For each
--     service, the response includes information about the most recent
--     access attempt.
--
--     The @JobId@ returned by @GenerateServiceLastAccessedDetail@ must be
--     used by the same role within a session, or by the same user when
--     used to call @GetServiceLastAccessedDetail@.
--
-- -   GetServiceLastAccessedDetailsWithEntities – Use this operation for
--     groups and policies to list information about the associated
--     entities (users or roles) that attempted to access a specific Amazon
--     Web Services service.
--
-- To check the status of the @GenerateServiceLastAccessedDetails@ request,
-- use the @JobId@ parameter in the same operations and test the
-- @JobStatus@ response parameter.
--
-- For additional information about the permissions policies that allow an
-- identity (user, group, or role) to access specific services, use the
-- ListPoliciesGrantingServiceAccess operation.
--
-- Service last accessed data does not use other policy types when
-- determining whether a resource could access a service. These other
-- policy types include resource-based policies, access control lists,
-- Organizations policies, IAM permissions boundaries, and STS assume role
-- policies. It only applies permissions policy logic. For more about the
-- evaluation of policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating policies>
-- in the /IAM User Guide/.
--
-- For more information about service and action last accessed data, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing permissions using service last accessed data>
-- in the /IAM User Guide/.
module Amazonka.IAM.GenerateServiceLastAccessedDetails
  ( -- * Creating a Request
    GenerateServiceLastAccessedDetails (..),
    newGenerateServiceLastAccessedDetails,

    -- * Request Lenses
    generateServiceLastAccessedDetails_granularity,
    generateServiceLastAccessedDetails_arn,

    -- * Destructuring the Response
    GenerateServiceLastAccessedDetailsResponse (..),
    newGenerateServiceLastAccessedDetailsResponse,

    -- * Response Lenses
    generateServiceLastAccessedDetailsResponse_jobId,
    generateServiceLastAccessedDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGenerateServiceLastAccessedDetails' smart constructor.
data GenerateServiceLastAccessedDetails = GenerateServiceLastAccessedDetails'
  { -- | The level of detail that you want to generate. You can specify whether
    -- you want to generate information about the last attempt to access
    -- services or actions. If you specify service-level granularity, this
    -- operation generates only service data. If you specify action-level
    -- granularity, it generates service and action data. If you don\'t include
    -- this optional parameter, the operation generates service data.
    granularity :: Prelude.Maybe AccessAdvisorUsageGranularityType,
    -- | The ARN of the IAM resource (user, group, role, or managed policy) used
    -- to generate information about when the resource was last used in an
    -- attempt to access an Amazon Web Services service.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateServiceLastAccessedDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'granularity', 'generateServiceLastAccessedDetails_granularity' - The level of detail that you want to generate. You can specify whether
-- you want to generate information about the last attempt to access
-- services or actions. If you specify service-level granularity, this
-- operation generates only service data. If you specify action-level
-- granularity, it generates service and action data. If you don\'t include
-- this optional parameter, the operation generates service data.
--
-- 'arn', 'generateServiceLastAccessedDetails_arn' - The ARN of the IAM resource (user, group, role, or managed policy) used
-- to generate information about when the resource was last used in an
-- attempt to access an Amazon Web Services service.
newGenerateServiceLastAccessedDetails ::
  -- | 'arn'
  Prelude.Text ->
  GenerateServiceLastAccessedDetails
newGenerateServiceLastAccessedDetails pArn_ =
  GenerateServiceLastAccessedDetails'
    { granularity =
        Prelude.Nothing,
      arn = pArn_
    }

-- | The level of detail that you want to generate. You can specify whether
-- you want to generate information about the last attempt to access
-- services or actions. If you specify service-level granularity, this
-- operation generates only service data. If you specify action-level
-- granularity, it generates service and action data. If you don\'t include
-- this optional parameter, the operation generates service data.
generateServiceLastAccessedDetails_granularity :: Lens.Lens' GenerateServiceLastAccessedDetails (Prelude.Maybe AccessAdvisorUsageGranularityType)
generateServiceLastAccessedDetails_granularity = Lens.lens (\GenerateServiceLastAccessedDetails' {granularity} -> granularity) (\s@GenerateServiceLastAccessedDetails' {} a -> s {granularity = a} :: GenerateServiceLastAccessedDetails)

-- | The ARN of the IAM resource (user, group, role, or managed policy) used
-- to generate information about when the resource was last used in an
-- attempt to access an Amazon Web Services service.
generateServiceLastAccessedDetails_arn :: Lens.Lens' GenerateServiceLastAccessedDetails Prelude.Text
generateServiceLastAccessedDetails_arn = Lens.lens (\GenerateServiceLastAccessedDetails' {arn} -> arn) (\s@GenerateServiceLastAccessedDetails' {} a -> s {arn = a} :: GenerateServiceLastAccessedDetails)

instance
  Core.AWSRequest
    GenerateServiceLastAccessedDetails
  where
  type
    AWSResponse GenerateServiceLastAccessedDetails =
      GenerateServiceLastAccessedDetailsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GenerateServiceLastAccessedDetailsResult"
      ( \s h x ->
          GenerateServiceLastAccessedDetailsResponse'
            Prelude.<$> (x Data..@? "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GenerateServiceLastAccessedDetails
  where
  hashWithSalt
    _salt
    GenerateServiceLastAccessedDetails' {..} =
      _salt
        `Prelude.hashWithSalt` granularity
        `Prelude.hashWithSalt` arn

instance
  Prelude.NFData
    GenerateServiceLastAccessedDetails
  where
  rnf GenerateServiceLastAccessedDetails' {..} =
    Prelude.rnf granularity
      `Prelude.seq` Prelude.rnf arn

instance
  Data.ToHeaders
    GenerateServiceLastAccessedDetails
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GenerateServiceLastAccessedDetails
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GenerateServiceLastAccessedDetails
  where
  toQuery GenerateServiceLastAccessedDetails' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GenerateServiceLastAccessedDetails" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Granularity" Data.=: granularity,
        "Arn" Data.=: arn
      ]

-- | /See:/ 'newGenerateServiceLastAccessedDetailsResponse' smart constructor.
data GenerateServiceLastAccessedDetailsResponse = GenerateServiceLastAccessedDetailsResponse'
  { -- | The @JobId@ that you can use in the GetServiceLastAccessedDetails or
    -- GetServiceLastAccessedDetailsWithEntities operations. The @JobId@
    -- returned by @GenerateServiceLastAccessedDetail@ must be used by the same
    -- role within a session, or by the same user when used to call
    -- @GetServiceLastAccessedDetail@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateServiceLastAccessedDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'generateServiceLastAccessedDetailsResponse_jobId' - The @JobId@ that you can use in the GetServiceLastAccessedDetails or
-- GetServiceLastAccessedDetailsWithEntities operations. The @JobId@
-- returned by @GenerateServiceLastAccessedDetail@ must be used by the same
-- role within a session, or by the same user when used to call
-- @GetServiceLastAccessedDetail@.
--
-- 'httpStatus', 'generateServiceLastAccessedDetailsResponse_httpStatus' - The response's http status code.
newGenerateServiceLastAccessedDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateServiceLastAccessedDetailsResponse
newGenerateServiceLastAccessedDetailsResponse
  pHttpStatus_ =
    GenerateServiceLastAccessedDetailsResponse'
      { jobId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @JobId@ that you can use in the GetServiceLastAccessedDetails or
-- GetServiceLastAccessedDetailsWithEntities operations. The @JobId@
-- returned by @GenerateServiceLastAccessedDetail@ must be used by the same
-- role within a session, or by the same user when used to call
-- @GetServiceLastAccessedDetail@.
generateServiceLastAccessedDetailsResponse_jobId :: Lens.Lens' GenerateServiceLastAccessedDetailsResponse (Prelude.Maybe Prelude.Text)
generateServiceLastAccessedDetailsResponse_jobId = Lens.lens (\GenerateServiceLastAccessedDetailsResponse' {jobId} -> jobId) (\s@GenerateServiceLastAccessedDetailsResponse' {} a -> s {jobId = a} :: GenerateServiceLastAccessedDetailsResponse)

-- | The response's http status code.
generateServiceLastAccessedDetailsResponse_httpStatus :: Lens.Lens' GenerateServiceLastAccessedDetailsResponse Prelude.Int
generateServiceLastAccessedDetailsResponse_httpStatus = Lens.lens (\GenerateServiceLastAccessedDetailsResponse' {httpStatus} -> httpStatus) (\s@GenerateServiceLastAccessedDetailsResponse' {} a -> s {httpStatus = a} :: GenerateServiceLastAccessedDetailsResponse)

instance
  Prelude.NFData
    GenerateServiceLastAccessedDetailsResponse
  where
  rnf GenerateServiceLastAccessedDetailsResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
