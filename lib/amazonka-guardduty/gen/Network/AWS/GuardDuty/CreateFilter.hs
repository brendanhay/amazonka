{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a filter using the specified finding criteria.
module Network.AWS.GuardDuty.CreateFilter
  ( -- * Creating a request
    CreateFilter (..),
    mkCreateFilter,

    -- ** Request lenses
    cfDetectorId,
    cfName,
    cfFindingCriteria,
    cfAction,
    cfClientToken,
    cfDescription,
    cfRank,
    cfTags,

    -- * Destructuring the response
    CreateFilterResponse (..),
    mkCreateFilterResponse,

    -- ** Response lenses
    cfrrsName,
    cfrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFilter' smart constructor.
data CreateFilter = CreateFilter'
  { -- | The unique ID of the detector of the GuardDuty account that you want to create a filter for.
    detectorId :: Types.DetectorId,
    -- | The name of the filter.
    name :: Types.FilterName,
    -- | Represents the criteria to be used in the filter for querying findings.
    --
    -- You can only use the following attributes to query findings:
    --
    --     * accountId
    --
    --
    --     * region
    --
    --
    --     * confidence
    --
    --
    --     * id
    --
    --
    --     * resource.accessKeyDetails.accessKeyId
    --
    --
    --     * resource.accessKeyDetails.principalId
    --
    --
    --     * resource.accessKeyDetails.userName
    --
    --
    --     * resource.accessKeyDetails.userType
    --
    --
    --     * resource.instanceDetails.iamInstanceProfile.id
    --
    --
    --     * resource.instanceDetails.imageId
    --
    --
    --     * resource.instanceDetails.instanceId
    --
    --
    --     * resource.instanceDetails.outpostArn
    --
    --
    --     * resource.instanceDetails.networkInterfaces.ipv6Addresses
    --
    --
    --     * resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
    --
    --
    --     * resource.instanceDetails.networkInterfaces.publicDnsName
    --
    --
    --     * resource.instanceDetails.networkInterfaces.publicIp
    --
    --
    --     * resource.instanceDetails.networkInterfaces.securityGroups.groupId
    --
    --
    --     * resource.instanceDetails.networkInterfaces.securityGroups.groupName
    --
    --
    --     * resource.instanceDetails.networkInterfaces.subnetId
    --
    --
    --     * resource.instanceDetails.networkInterfaces.vpcId
    --
    --
    --     * resource.instanceDetails.tags.key
    --
    --
    --     * resource.instanceDetails.tags.value
    --
    --
    --     * resource.resourceType
    --
    --
    --     * service.action.actionType
    --
    --
    --     * service.action.awsApiCallAction.api
    --
    --
    --     * service.action.awsApiCallAction.callerType
    --
    --
    --     * service.action.awsApiCallAction.remoteIpDetails.city.cityName
    --
    --
    --     * service.action.awsApiCallAction.remoteIpDetails.country.countryName
    --
    --
    --     * service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
    --
    --
    --     * service.action.awsApiCallAction.remoteIpDetails.organization.asn
    --
    --
    --     * service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
    --
    --
    --     * service.action.awsApiCallAction.serviceName
    --
    --
    --     * service.action.dnsRequestAction.domain
    --
    --
    --     * service.action.networkConnectionAction.blocked
    --
    --
    --     * service.action.networkConnectionAction.connectionDirection
    --
    --
    --     * service.action.networkConnectionAction.localPortDetails.port
    --
    --
    --     * service.action.networkConnectionAction.protocol
    --
    --
    --     * service.action.networkConnectionAction.localIpDetails.ipAddressV4
    --
    --
    --     * service.action.networkConnectionAction.remoteIpDetails.city.cityName
    --
    --
    --     * service.action.networkConnectionAction.remoteIpDetails.country.countryName
    --
    --
    --     * service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
    --
    --
    --     * service.action.networkConnectionAction.remoteIpDetails.organization.asn
    --
    --
    --     * service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
    --
    --
    --     * service.action.networkConnectionAction.remotePortDetails.port
    --
    --
    --     * service.additionalInfo.threatListName
    --
    --
    --     * service.archived
    -- When this attribute is set to TRUE, only archived findings are listed. When it's set to FALSE, only unarchived findings are listed. When this attribute is not set, all existing findings are listed.
    --
    --
    --     * service.resourceRole
    --
    --
    --     * severity
    --
    --
    --     * type
    --
    --
    --     * updatedAt
    -- Type: ISO 8601 string format: YYYY-MM-DDTHH:MM:SS.SSSZ or YYYY-MM-DDTHH:MM:SSZ depending on whether the value contains milliseconds.
    findingCriteria :: Types.FindingCriteria,
    -- | Specifies the action that is to be applied to the findings that match the filter.
    action :: Core.Maybe Types.FilterAction,
    -- | The idempotency token for the create request.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | The description of the filter.
    description :: Core.Maybe Types.FilterDescription,
    -- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
    rank :: Core.Maybe Core.Natural,
    -- | The tags to be added to a new filter resource.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFilter' value with any optional fields omitted.
mkCreateFilter ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'name'
  Types.FilterName ->
  -- | 'findingCriteria'
  Types.FindingCriteria ->
  CreateFilter
mkCreateFilter detectorId name findingCriteria =
  CreateFilter'
    { detectorId,
      name,
      findingCriteria,
      action = Core.Nothing,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      rank = Core.Nothing,
      tags = Core.Nothing
    }

-- | The unique ID of the detector of the GuardDuty account that you want to create a filter for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDetectorId :: Lens.Lens' CreateFilter Types.DetectorId
cfDetectorId = Lens.field @"detectorId"
{-# DEPRECATED cfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFilter Types.FilterName
cfName = Lens.field @"name"
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Represents the criteria to be used in the filter for querying findings.
--
-- You can only use the following attributes to query findings:
--
--     * accountId
--
--
--     * region
--
--
--     * confidence
--
--
--     * id
--
--
--     * resource.accessKeyDetails.accessKeyId
--
--
--     * resource.accessKeyDetails.principalId
--
--
--     * resource.accessKeyDetails.userName
--
--
--     * resource.accessKeyDetails.userType
--
--
--     * resource.instanceDetails.iamInstanceProfile.id
--
--
--     * resource.instanceDetails.imageId
--
--
--     * resource.instanceDetails.instanceId
--
--
--     * resource.instanceDetails.outpostArn
--
--
--     * resource.instanceDetails.networkInterfaces.ipv6Addresses
--
--
--     * resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
--
--
--     * resource.instanceDetails.networkInterfaces.publicDnsName
--
--
--     * resource.instanceDetails.networkInterfaces.publicIp
--
--
--     * resource.instanceDetails.networkInterfaces.securityGroups.groupId
--
--
--     * resource.instanceDetails.networkInterfaces.securityGroups.groupName
--
--
--     * resource.instanceDetails.networkInterfaces.subnetId
--
--
--     * resource.instanceDetails.networkInterfaces.vpcId
--
--
--     * resource.instanceDetails.tags.key
--
--
--     * resource.instanceDetails.tags.value
--
--
--     * resource.resourceType
--
--
--     * service.action.actionType
--
--
--     * service.action.awsApiCallAction.api
--
--
--     * service.action.awsApiCallAction.callerType
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.city.cityName
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.country.countryName
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.organization.asn
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
--
--
--     * service.action.awsApiCallAction.serviceName
--
--
--     * service.action.dnsRequestAction.domain
--
--
--     * service.action.networkConnectionAction.blocked
--
--
--     * service.action.networkConnectionAction.connectionDirection
--
--
--     * service.action.networkConnectionAction.localPortDetails.port
--
--
--     * service.action.networkConnectionAction.protocol
--
--
--     * service.action.networkConnectionAction.localIpDetails.ipAddressV4
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.city.cityName
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.country.countryName
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.organization.asn
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
--
--
--     * service.action.networkConnectionAction.remotePortDetails.port
--
--
--     * service.additionalInfo.threatListName
--
--
--     * service.archived
-- When this attribute is set to TRUE, only archived findings are listed. When it's set to FALSE, only unarchived findings are listed. When this attribute is not set, all existing findings are listed.
--
--
--     * service.resourceRole
--
--
--     * severity
--
--
--     * type
--
--
--     * updatedAt
-- Type: ISO 8601 string format: YYYY-MM-DDTHH:MM:SS.SSSZ or YYYY-MM-DDTHH:MM:SSZ depending on whether the value contains milliseconds.
--
--
--
-- /Note:/ Consider using 'findingCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFindingCriteria :: Lens.Lens' CreateFilter Types.FindingCriteria
cfFindingCriteria = Lens.field @"findingCriteria"
{-# DEPRECATED cfFindingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead." #-}

-- | Specifies the action that is to be applied to the findings that match the filter.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAction :: Lens.Lens' CreateFilter (Core.Maybe Types.FilterAction)
cfAction = Lens.field @"action"
{-# DEPRECATED cfAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The idempotency token for the create request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfClientToken :: Lens.Lens' CreateFilter (Core.Maybe Types.ClientToken)
cfClientToken = Lens.field @"clientToken"
{-# DEPRECATED cfClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The description of the filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFilter (Core.Maybe Types.FilterDescription)
cfDescription = Lens.field @"description"
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- /Note:/ Consider using 'rank' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRank :: Lens.Lens' CreateFilter (Core.Maybe Core.Natural)
cfRank = Lens.field @"rank"
{-# DEPRECATED cfRank "Use generic-lens or generic-optics with 'rank' instead." #-}

-- | The tags to be added to a new filter resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' CreateFilter (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cfTags = Lens.field @"tags"
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateFilter where
  toJSON CreateFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("findingCriteria" Core..= findingCriteria),
            ("action" Core..=) Core.<$> action,
            ("clientToken" Core..=) Core.<$> clientToken,
            ("description" Core..=) Core.<$> description,
            ("rank" Core..=) Core.<$> rank,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateFilter where
  type Rs CreateFilter = CreateFilterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/filter")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFilterResponse'
            Core.<$> (x Core..: "name") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFilterResponse' smart constructor.
data CreateFilterResponse = CreateFilterResponse'
  { -- | The name of the successfully created filter.
    name :: Types.FilterName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFilterResponse' value with any optional fields omitted.
mkCreateFilterResponse ::
  -- | 'name'
  Types.FilterName ->
  -- | 'responseStatus'
  Core.Int ->
  CreateFilterResponse
mkCreateFilterResponse name responseStatus =
  CreateFilterResponse' {name, responseStatus}

-- | The name of the successfully created filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsName :: Lens.Lens' CreateFilterResponse Types.FilterName
cfrrsName = Lens.field @"name"
{-# DEPRECATED cfrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsResponseStatus :: Lens.Lens' CreateFilterResponse Core.Int
cfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
