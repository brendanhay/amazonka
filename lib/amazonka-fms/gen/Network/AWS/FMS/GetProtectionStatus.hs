{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetProtectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If you created a Shield Advanced policy, returns policy-level attack summary information in the event of a potential DDoS attack. Other policy types are currently unsupported.
module Network.AWS.FMS.GetProtectionStatus
  ( -- * Creating a request
    GetProtectionStatus (..),
    mkGetProtectionStatus,

    -- ** Request lenses
    gpsPolicyId,
    gpsEndTime,
    gpsMaxResults,
    gpsMemberAccountId,
    gpsNextToken,
    gpsStartTime,

    -- * Destructuring the response
    GetProtectionStatusResponse (..),
    mkGetProtectionStatusResponse,

    -- ** Response lenses
    gpsrrsAdminAccountId,
    gpsrrsData,
    gpsrrsNextToken,
    gpsrrsServiceType,
    gpsrrsResponseStatus,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetProtectionStatus' smart constructor.
data GetProtectionStatus = GetProtectionStatus'
  { -- | The ID of the policy for which you want to get the attack information.
    policyId :: Types.PolicyId,
    -- | The end of the time period to query for the attacks. This is a @timestamp@ type. The request syntax listing indicates a @number@ type because the default used by AWS Firewall Manager is Unix time in seconds. However, any valid @timestamp@ format is allowed.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | Specifies the number of objects that you want AWS Firewall Manager to return for this request. If you have more objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of objects.
    maxResults :: Core.Maybe Core.Natural,
    -- | The AWS account that is in scope of the policy that you want to get the details for.
    memberAccountId :: Core.Maybe Types.AWSAccountId,
    -- | If you specify a value for @MaxResults@ and you have more objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response, which you can use to retrieve another group of objects. For the second and subsequent @GetProtectionStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of objects.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The start of the time period to query for the attacks. This is a @timestamp@ type. The request syntax listing indicates a @number@ type because the default used by AWS Firewall Manager is Unix time in seconds. However, any valid @timestamp@ format is allowed.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetProtectionStatus' value with any optional fields omitted.
mkGetProtectionStatus ::
  -- | 'policyId'
  Types.PolicyId ->
  GetProtectionStatus
mkGetProtectionStatus policyId =
  GetProtectionStatus'
    { policyId,
      endTime = Core.Nothing,
      maxResults = Core.Nothing,
      memberAccountId = Core.Nothing,
      nextToken = Core.Nothing,
      startTime = Core.Nothing
    }

-- | The ID of the policy for which you want to get the attack information.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsPolicyId :: Lens.Lens' GetProtectionStatus Types.PolicyId
gpsPolicyId = Lens.field @"policyId"
{-# DEPRECATED gpsPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The end of the time period to query for the attacks. This is a @timestamp@ type. The request syntax listing indicates a @number@ type because the default used by AWS Firewall Manager is Unix time in seconds. However, any valid @timestamp@ format is allowed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsEndTime :: Lens.Lens' GetProtectionStatus (Core.Maybe Core.NominalDiffTime)
gpsEndTime = Lens.field @"endTime"
{-# DEPRECATED gpsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Specifies the number of objects that you want AWS Firewall Manager to return for this request. If you have more objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsMaxResults :: Lens.Lens' GetProtectionStatus (Core.Maybe Core.Natural)
gpsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gpsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The AWS account that is in scope of the policy that you want to get the details for.
--
-- /Note:/ Consider using 'memberAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsMemberAccountId :: Lens.Lens' GetProtectionStatus (Core.Maybe Types.AWSAccountId)
gpsMemberAccountId = Lens.field @"memberAccountId"
{-# DEPRECATED gpsMemberAccountId "Use generic-lens or generic-optics with 'memberAccountId' instead." #-}

-- | If you specify a value for @MaxResults@ and you have more objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response, which you can use to retrieve another group of objects. For the second and subsequent @GetProtectionStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsNextToken :: Lens.Lens' GetProtectionStatus (Core.Maybe Types.PaginationToken)
gpsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gpsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The start of the time period to query for the attacks. This is a @timestamp@ type. The request syntax listing indicates a @number@ type because the default used by AWS Firewall Manager is Unix time in seconds. However, any valid @timestamp@ format is allowed.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsStartTime :: Lens.Lens' GetProtectionStatus (Core.Maybe Core.NominalDiffTime)
gpsStartTime = Lens.field @"startTime"
{-# DEPRECATED gpsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON GetProtectionStatus where
  toJSON GetProtectionStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyId" Core..= policyId),
            ("EndTime" Core..=) Core.<$> endTime,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("MemberAccountId" Core..=) Core.<$> memberAccountId,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StartTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.AWSRequest GetProtectionStatus where
  type Rs GetProtectionStatus = GetProtectionStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSFMS_20180101.GetProtectionStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProtectionStatusResponse'
            Core.<$> (x Core..:? "AdminAccountId")
            Core.<*> (x Core..:? "Data")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ServiceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetProtectionStatusResponse' smart constructor.
data GetProtectionStatusResponse = GetProtectionStatusResponse'
  { -- | The ID of the AWS Firewall administrator account for this policy.
    adminAccountId :: Core.Maybe Types.AWSAccountId,
    -- | Details about the attack, including the following:
    --
    --
    --     * Attack type
    --
    --
    --     * Account ID
    --
    --
    --     * ARN of the resource attacked
    --
    --
    --     * Start time of the attack
    --
    --
    --     * End time of the attack (ongoing attacks will not have an end time)
    --
    --
    -- The details are in JSON format.
    data' :: Core.Maybe Types.ProtectionData,
    -- | If you have more objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more objects, submit another @GetProtectionStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
    --
    -- AWS SDKs provide auto-pagination that identify @NextToken@ in a response and make subsequent request calls automatically on your behalf. However, this feature is not supported by @GetProtectionStatus@ . You must submit subsequent requests with @NextToken@ using your own processes.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The service type that is protected by the policy. Currently, this is always @SHIELD_ADVANCED@ .
    serviceType :: Core.Maybe Types.SecurityServiceType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProtectionStatusResponse' value with any optional fields omitted.
mkGetProtectionStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetProtectionStatusResponse
mkGetProtectionStatusResponse responseStatus =
  GetProtectionStatusResponse'
    { adminAccountId = Core.Nothing,
      data' = Core.Nothing,
      nextToken = Core.Nothing,
      serviceType = Core.Nothing,
      responseStatus
    }

-- | The ID of the AWS Firewall administrator account for this policy.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsAdminAccountId :: Lens.Lens' GetProtectionStatusResponse (Core.Maybe Types.AWSAccountId)
gpsrrsAdminAccountId = Lens.field @"adminAccountId"
{-# DEPRECATED gpsrrsAdminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead." #-}

-- | Details about the attack, including the following:
--
--
--     * Attack type
--
--
--     * Account ID
--
--
--     * ARN of the resource attacked
--
--
--     * Start time of the attack
--
--
--     * End time of the attack (ongoing attacks will not have an end time)
--
--
-- The details are in JSON format.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsData :: Lens.Lens' GetProtectionStatusResponse (Core.Maybe Types.ProtectionData)
gpsrrsData = Lens.field @"data'"
{-# DEPRECATED gpsrrsData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | If you have more objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more objects, submit another @GetProtectionStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- AWS SDKs provide auto-pagination that identify @NextToken@ in a response and make subsequent request calls automatically on your behalf. However, this feature is not supported by @GetProtectionStatus@ . You must submit subsequent requests with @NextToken@ using your own processes.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsNextToken :: Lens.Lens' GetProtectionStatusResponse (Core.Maybe Types.PaginationToken)
gpsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gpsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The service type that is protected by the policy. Currently, this is always @SHIELD_ADVANCED@ .
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsServiceType :: Lens.Lens' GetProtectionStatusResponse (Core.Maybe Types.SecurityServiceType)
gpsrrsServiceType = Lens.field @"serviceType"
{-# DEPRECATED gpsrrsServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsResponseStatus :: Lens.Lens' GetProtectionStatusResponse Core.Int
gpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
