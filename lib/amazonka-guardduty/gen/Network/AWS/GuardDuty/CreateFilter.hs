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
    cfClientToken,
    cfFindingCriteria,
    cfAction,
    cfDetectorId,
    cfName,
    cfDescription,
    cfRank,
    cfTags,

    -- * Destructuring the response
    CreateFilterResponse (..),
    mkCreateFilterResponse,

    -- ** Response lenses
    cfrsName,
    cfrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFilter' smart constructor.
data CreateFilter = CreateFilter'
  { -- | The idempotency token for the create request.
    clientToken :: Lude.Maybe Lude.Text,
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
    findingCriteria :: FindingCriteria,
    -- | Specifies the action that is to be applied to the findings that match the filter.
    action :: Lude.Maybe FilterAction,
    -- | The unique ID of the detector of the GuardDuty account that you want to create a filter for.
    detectorId :: Lude.Text,
    -- | The name of the filter.
    name :: Lude.Text,
    -- | The description of the filter.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
    rank :: Lude.Maybe Lude.Natural,
    -- | The tags to be added to a new filter resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFilter' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token for the create request.
-- * 'findingCriteria' - Represents the criteria to be used in the filter for querying findings.
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
-- * 'action' - Specifies the action that is to be applied to the findings that match the filter.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty account that you want to create a filter for.
-- * 'name' - The name of the filter.
-- * 'description' - The description of the filter.
-- * 'rank' - Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
-- * 'tags' - The tags to be added to a new filter resource.
mkCreateFilter ::
  -- | 'findingCriteria'
  FindingCriteria ->
  -- | 'detectorId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateFilter
mkCreateFilter pFindingCriteria_ pDetectorId_ pName_ =
  CreateFilter'
    { clientToken = Lude.Nothing,
      findingCriteria = pFindingCriteria_,
      action = Lude.Nothing,
      detectorId = pDetectorId_,
      name = pName_,
      description = Lude.Nothing,
      rank = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The idempotency token for the create request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfClientToken :: Lens.Lens' CreateFilter (Lude.Maybe Lude.Text)
cfClientToken = Lens.lens (clientToken :: CreateFilter -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateFilter)
{-# DEPRECATED cfClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

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
cfFindingCriteria :: Lens.Lens' CreateFilter FindingCriteria
cfFindingCriteria = Lens.lens (findingCriteria :: CreateFilter -> FindingCriteria) (\s a -> s {findingCriteria = a} :: CreateFilter)
{-# DEPRECATED cfFindingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead." #-}

-- | Specifies the action that is to be applied to the findings that match the filter.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAction :: Lens.Lens' CreateFilter (Lude.Maybe FilterAction)
cfAction = Lens.lens (action :: CreateFilter -> Lude.Maybe FilterAction) (\s a -> s {action = a} :: CreateFilter)
{-# DEPRECATED cfAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The unique ID of the detector of the GuardDuty account that you want to create a filter for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDetectorId :: Lens.Lens' CreateFilter Lude.Text
cfDetectorId = Lens.lens (detectorId :: CreateFilter -> Lude.Text) (\s a -> s {detectorId = a} :: CreateFilter)
{-# DEPRECATED cfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFilter Lude.Text
cfName = Lens.lens (name :: CreateFilter -> Lude.Text) (\s a -> s {name = a} :: CreateFilter)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFilter (Lude.Maybe Lude.Text)
cfDescription = Lens.lens (description :: CreateFilter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateFilter)
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- /Note:/ Consider using 'rank' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRank :: Lens.Lens' CreateFilter (Lude.Maybe Lude.Natural)
cfRank = Lens.lens (rank :: CreateFilter -> Lude.Maybe Lude.Natural) (\s a -> s {rank = a} :: CreateFilter)
{-# DEPRECATED cfRank "Use generic-lens or generic-optics with 'rank' instead." #-}

-- | The tags to be added to a new filter resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' CreateFilter (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cfTags = Lens.lens (tags :: CreateFilter -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateFilter)
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateFilter where
  type Rs CreateFilter = CreateFilterResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateFilterResponse'
            Lude.<$> (x Lude..:> "name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateFilter where
  toJSON CreateFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientToken" Lude..=) Lude.<$> clientToken,
            Lude.Just ("findingCriteria" Lude..= findingCriteria),
            ("action" Lude..=) Lude.<$> action,
            Lude.Just ("name" Lude..= name),
            ("description" Lude..=) Lude.<$> description,
            ("rank" Lude..=) Lude.<$> rank,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateFilter where
  toPath CreateFilter' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/filter"]

instance Lude.ToQuery CreateFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFilterResponse' smart constructor.
data CreateFilterResponse = CreateFilterResponse'
  { -- | The name of the successfully created filter.
    name :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFilterResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the successfully created filter.
-- * 'responseStatus' - The response status code.
mkCreateFilterResponse ::
  -- | 'name'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateFilterResponse
mkCreateFilterResponse pName_ pResponseStatus_ =
  CreateFilterResponse'
    { name = pName_,
      responseStatus = pResponseStatus_
    }

-- | The name of the successfully created filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsName :: Lens.Lens' CreateFilterResponse Lude.Text
cfrsName = Lens.lens (name :: CreateFilterResponse -> Lude.Text) (\s a -> s {name = a} :: CreateFilterResponse)
{-# DEPRECATED cfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsResponseStatus :: Lens.Lens' CreateFilterResponse Lude.Int
cfrsResponseStatus = Lens.lens (responseStatus :: CreateFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFilterResponse)
{-# DEPRECATED cfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
