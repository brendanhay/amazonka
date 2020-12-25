{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.EnableDomainTransferLock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets the transfer lock on the domain (specifically the @clientTransferProhibited@ status) to prevent domain transfers. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.EnableDomainTransferLock
  ( -- * Creating a request
    EnableDomainTransferLock (..),
    mkEnableDomainTransferLock,

    -- ** Request lenses
    edtlDomainName,

    -- * Destructuring the response
    EnableDomainTransferLockResponse (..),
    mkEnableDomainTransferLockResponse,

    -- ** Response lenses
    edtlrrsOperationId,
    edtlrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | A request to set the transfer lock for the specified domain.
--
-- /See:/ 'mkEnableDomainTransferLock' smart constructor.
newtype EnableDomainTransferLock = EnableDomainTransferLock'
  { -- | The name of the domain that you want to set the transfer lock for.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableDomainTransferLock' value with any optional fields omitted.
mkEnableDomainTransferLock ::
  -- | 'domainName'
  Types.DomainName ->
  EnableDomainTransferLock
mkEnableDomainTransferLock domainName =
  EnableDomainTransferLock' {domainName}

-- | The name of the domain that you want to set the transfer lock for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edtlDomainName :: Lens.Lens' EnableDomainTransferLock Types.DomainName
edtlDomainName = Lens.field @"domainName"
{-# DEPRECATED edtlDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.FromJSON EnableDomainTransferLock where
  toJSON EnableDomainTransferLock {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest EnableDomainTransferLock where
  type Rs EnableDomainTransferLock = EnableDomainTransferLockResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Route53Domains_v20140515.EnableDomainTransferLock"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableDomainTransferLockResponse'
            Core.<$> (x Core..: "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The EnableDomainTransferLock response includes the following elements.
--
-- /See:/ 'mkEnableDomainTransferLockResponse' smart constructor.
data EnableDomainTransferLockResponse = EnableDomainTransferLockResponse'
  { -- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
    operationId :: Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableDomainTransferLockResponse' value with any optional fields omitted.
mkEnableDomainTransferLockResponse ::
  -- | 'operationId'
  Types.OperationId ->
  -- | 'responseStatus'
  Core.Int ->
  EnableDomainTransferLockResponse
mkEnableDomainTransferLockResponse operationId responseStatus =
  EnableDomainTransferLockResponse' {operationId, responseStatus}

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edtlrrsOperationId :: Lens.Lens' EnableDomainTransferLockResponse Types.OperationId
edtlrrsOperationId = Lens.field @"operationId"
{-# DEPRECATED edtlrrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edtlrrsResponseStatus :: Lens.Lens' EnableDomainTransferLockResponse Core.Int
edtlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED edtlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
