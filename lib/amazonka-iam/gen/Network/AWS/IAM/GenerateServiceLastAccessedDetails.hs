{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GenerateServiceLastAccessedDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a report that includes details about when an IAM resource (user, group, role, or policy) was last used in an attempt to access AWS services. Recent activity usually appears within four hours. IAM reports activity for the last 365 days, or less if your Region began supporting this feature within the last year. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> .
--
-- /Important:/ The service last accessed data includes all attempts to access an AWS API, not just the successful ones. This includes all attempts that were made using the AWS Management Console, the AWS API through any of the SDKs, or any of the command line tools. An unexpected entry in the service last accessed data does not mean that your account has been compromised, because the request might have been denied. Refer to your CloudTrail logs as the authoritative source for information about all API calls and whether they were successful or denied access. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html Logging IAM Events with CloudTrail> in the /IAM User Guide/ .
-- The @GenerateServiceLastAccessedDetails@ operation returns a @JobId@ . Use this parameter in the following operations to retrieve the following details from your report:
--
--     * 'GetServiceLastAccessedDetails' – Use this operation for users, groups, roles, or policies to list every AWS service that the resource could access using permissions policies. For each service, the response includes information about the most recent access attempt.
-- The @JobId@ returned by @GenerateServiceLastAccessedDetail@ must be used by the same role within a session, or by the same user when used to call @GetServiceLastAccessedDetail@ .
--
--
--     * 'GetServiceLastAccessedDetailsWithEntities' – Use this operation for groups and policies to list information about the associated entities (users or roles) that attempted to access a specific AWS service.
--
--
-- To check the status of the @GenerateServiceLastAccessedDetails@ request, use the @JobId@ parameter in the same operations and test the @JobStatus@ response parameter.
-- For additional information about the permissions policies that allow an identity (user, group, or role) to access specific services, use the 'ListPoliciesGrantingServiceAccess' operation.
-- For more information about service and action last accessed data, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing Permissions Using Service Last Accessed Data> in the /IAM User Guide/ .
module Network.AWS.IAM.GenerateServiceLastAccessedDetails
  ( -- * Creating a request
    GenerateServiceLastAccessedDetails (..),
    mkGenerateServiceLastAccessedDetails,

    -- ** Request lenses
    gsladArn,
    gsladGranularity,

    -- * Destructuring the response
    GenerateServiceLastAccessedDetailsResponse (..),
    mkGenerateServiceLastAccessedDetailsResponse,

    -- ** Response lenses
    gsladrfrsJobId,
    gsladrfrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGenerateServiceLastAccessedDetails' smart constructor.
data GenerateServiceLastAccessedDetails = GenerateServiceLastAccessedDetails'
  { -- | The ARN of the IAM resource (user, group, role, or managed policy) used to generate information about when the resource was last used in an attempt to access an AWS service.
    arn :: Types.Arn,
    -- | The level of detail that you want to generate. You can specify whether you want to generate information about the last attempt to access services or actions. If you specify service-level granularity, this operation generates only service data. If you specify action-level granularity, it generates service and action data. If you don't include this optional parameter, the operation generates service data.
    granularity :: Core.Maybe Types.AccessAdvisorUsageGranularityType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateServiceLastAccessedDetails' value with any optional fields omitted.
mkGenerateServiceLastAccessedDetails ::
  -- | 'arn'
  Types.Arn ->
  GenerateServiceLastAccessedDetails
mkGenerateServiceLastAccessedDetails arn =
  GenerateServiceLastAccessedDetails'
    { arn,
      granularity = Core.Nothing
    }

-- | The ARN of the IAM resource (user, group, role, or managed policy) used to generate information about when the resource was last used in an attempt to access an AWS service.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsladArn :: Lens.Lens' GenerateServiceLastAccessedDetails Types.Arn
gsladArn = Lens.field @"arn"
{-# DEPRECATED gsladArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The level of detail that you want to generate. You can specify whether you want to generate information about the last attempt to access services or actions. If you specify service-level granularity, this operation generates only service data. If you specify action-level granularity, it generates service and action data. If you don't include this optional parameter, the operation generates service data.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsladGranularity :: Lens.Lens' GenerateServiceLastAccessedDetails (Core.Maybe Types.AccessAdvisorUsageGranularityType)
gsladGranularity = Lens.field @"granularity"
{-# DEPRECATED gsladGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

instance Core.AWSRequest GenerateServiceLastAccessedDetails where
  type
    Rs GenerateServiceLastAccessedDetails =
      GenerateServiceLastAccessedDetailsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GenerateServiceLastAccessedDetails")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "Arn" arn)
                Core.<> (Core.toQueryValue "Granularity" Core.<$> granularity)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GenerateServiceLastAccessedDetailsResult"
      ( \s h x ->
          GenerateServiceLastAccessedDetailsResponse'
            Core.<$> (x Core..@? "JobId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGenerateServiceLastAccessedDetailsResponse' smart constructor.
data GenerateServiceLastAccessedDetailsResponse = GenerateServiceLastAccessedDetailsResponse'
  { -- | The @JobId@ that you can use in the 'GetServiceLastAccessedDetails' or 'GetServiceLastAccessedDetailsWithEntities' operations. The @JobId@ returned by @GenerateServiceLastAccessedDetail@ must be used by the same role within a session, or by the same user when used to call @GetServiceLastAccessedDetail@ .
    jobId :: Core.Maybe Types.JobId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateServiceLastAccessedDetailsResponse' value with any optional fields omitted.
mkGenerateServiceLastAccessedDetailsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GenerateServiceLastAccessedDetailsResponse
mkGenerateServiceLastAccessedDetailsResponse responseStatus =
  GenerateServiceLastAccessedDetailsResponse'
    { jobId = Core.Nothing,
      responseStatus
    }

-- | The @JobId@ that you can use in the 'GetServiceLastAccessedDetails' or 'GetServiceLastAccessedDetailsWithEntities' operations. The @JobId@ returned by @GenerateServiceLastAccessedDetail@ must be used by the same role within a session, or by the same user when used to call @GetServiceLastAccessedDetail@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsladrfrsJobId :: Lens.Lens' GenerateServiceLastAccessedDetailsResponse (Core.Maybe Types.JobId)
gsladrfrsJobId = Lens.field @"jobId"
{-# DEPRECATED gsladrfrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsladrfrsResponseStatus :: Lens.Lens' GenerateServiceLastAccessedDetailsResponse Core.Int
gsladrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsladrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
