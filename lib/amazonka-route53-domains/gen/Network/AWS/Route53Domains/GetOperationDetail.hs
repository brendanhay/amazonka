{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetOperationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current status of an operation that is not completed.
module Network.AWS.Route53Domains.GetOperationDetail
  ( -- * Creating a request
    GetOperationDetail (..),
    mkGetOperationDetail,

    -- ** Request lenses
    godOperationId,

    -- * Destructuring the response
    GetOperationDetailResponse (..),
    mkGetOperationDetailResponse,

    -- ** Response lenses
    godrrsDomainName,
    godrrsMessage,
    godrrsOperationId,
    godrrsStatus,
    godrrsSubmittedDate,
    godrrsType,
    godrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> request includes the following element.
--
-- /See:/ 'mkGetOperationDetail' smart constructor.
newtype GetOperationDetail = GetOperationDetail'
  { -- | The identifier for the operation for which you want to get the status. Route 53 returned the identifier in the response to the original request.
    operationId :: Types.OperationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOperationDetail' value with any optional fields omitted.
mkGetOperationDetail ::
  -- | 'operationId'
  Types.OperationId ->
  GetOperationDetail
mkGetOperationDetail operationId = GetOperationDetail' {operationId}

-- | The identifier for the operation for which you want to get the status. Route 53 returned the identifier in the response to the original request.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godOperationId :: Lens.Lens' GetOperationDetail Types.OperationId
godOperationId = Lens.field @"operationId"
{-# DEPRECATED godOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Core.FromJSON GetOperationDetail where
  toJSON GetOperationDetail {..} =
    Core.object
      (Core.catMaybes [Core.Just ("OperationId" Core..= operationId)])

instance Core.AWSRequest GetOperationDetail where
  type Rs GetOperationDetail = GetOperationDetailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53Domains_v20140515.GetOperationDetail")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationDetailResponse'
            Core.<$> (x Core..:? "DomainName")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "OperationId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "SubmittedDate")
            Core.<*> (x Core..:? "Type")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The GetOperationDetail response includes the following elements.
--
-- /See:/ 'mkGetOperationDetailResponse' smart constructor.
data GetOperationDetailResponse = GetOperationDetailResponse'
  { -- | The name of a domain.
    domainName :: Core.Maybe Types.DomainName,
    -- | Detailed information on the status including possible errors.
    message :: Core.Maybe Types.Message,
    -- | The identifier for the operation.
    operationId :: Core.Maybe Types.OperationId,
    -- | The current status of the requested operation in the system.
    status :: Core.Maybe Types.OperationStatus,
    -- | The date when the request was submitted.
    submittedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The type of operation that was requested.
    type' :: Core.Maybe Types.OperationType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetOperationDetailResponse' value with any optional fields omitted.
mkGetOperationDetailResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetOperationDetailResponse
mkGetOperationDetailResponse responseStatus =
  GetOperationDetailResponse'
    { domainName = Core.Nothing,
      message = Core.Nothing,
      operationId = Core.Nothing,
      status = Core.Nothing,
      submittedDate = Core.Nothing,
      type' = Core.Nothing,
      responseStatus
    }

-- | The name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrrsDomainName :: Lens.Lens' GetOperationDetailResponse (Core.Maybe Types.DomainName)
godrrsDomainName = Lens.field @"domainName"
{-# DEPRECATED godrrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Detailed information on the status including possible errors.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrrsMessage :: Lens.Lens' GetOperationDetailResponse (Core.Maybe Types.Message)
godrrsMessage = Lens.field @"message"
{-# DEPRECATED godrrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The identifier for the operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrrsOperationId :: Lens.Lens' GetOperationDetailResponse (Core.Maybe Types.OperationId)
godrrsOperationId = Lens.field @"operationId"
{-# DEPRECATED godrrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The current status of the requested operation in the system.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrrsStatus :: Lens.Lens' GetOperationDetailResponse (Core.Maybe Types.OperationStatus)
godrrsStatus = Lens.field @"status"
{-# DEPRECATED godrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date when the request was submitted.
--
-- /Note:/ Consider using 'submittedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrrsSubmittedDate :: Lens.Lens' GetOperationDetailResponse (Core.Maybe Core.NominalDiffTime)
godrrsSubmittedDate = Lens.field @"submittedDate"
{-# DEPRECATED godrrsSubmittedDate "Use generic-lens or generic-optics with 'submittedDate' instead." #-}

-- | The type of operation that was requested.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrrsType :: Lens.Lens' GetOperationDetailResponse (Core.Maybe Types.OperationType)
godrrsType = Lens.field @"type'"
{-# DEPRECATED godrrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrrsResponseStatus :: Lens.Lens' GetOperationDetailResponse Core.Int
godrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED godrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
