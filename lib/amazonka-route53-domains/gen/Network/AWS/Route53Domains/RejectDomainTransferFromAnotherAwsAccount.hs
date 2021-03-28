{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAwsAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects the transfer of a domain from another AWS account to the current AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> . 
--
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ . 
module Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAwsAccount
    (
    -- * Creating a request
      RejectDomainTransferFromAnotherAwsAccount (..)
    , mkRejectDomainTransferFromAnotherAwsAccount
    -- ** Request lenses
    , rdtfaaaDomainName

    -- * Destructuring the response
    , RejectDomainTransferFromAnotherAwsAccountResponse (..)
    , mkRejectDomainTransferFromAnotherAwsAccountResponse
    -- ** Response lenses
    , rdtfaaarrsOperationId
    , rdtfaaarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The RejectDomainTransferFromAnotherAwsAccount request includes the following element.
--
-- /See:/ 'mkRejectDomainTransferFromAnotherAwsAccount' smart constructor.
newtype RejectDomainTransferFromAnotherAwsAccount = RejectDomainTransferFromAnotherAwsAccount'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectDomainTransferFromAnotherAwsAccount' value with any optional fields omitted.
mkRejectDomainTransferFromAnotherAwsAccount
    :: Types.DomainName -- ^ 'domainName'
    -> RejectDomainTransferFromAnotherAwsAccount
mkRejectDomainTransferFromAnotherAwsAccount domainName
  = RejectDomainTransferFromAnotherAwsAccount'{domainName}

-- | The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request. 
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdtfaaaDomainName :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccount Types.DomainName
rdtfaaaDomainName = Lens.field @"domainName"
{-# INLINEABLE rdtfaaaDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery RejectDomainTransferFromAnotherAwsAccount
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RejectDomainTransferFromAnotherAwsAccount
         where
        toHeaders RejectDomainTransferFromAnotherAwsAccount{..}
          = Core.pure
              ("X-Amz-Target",
               "Route53Domains_v20140515.RejectDomainTransferFromAnotherAwsAccount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RejectDomainTransferFromAnotherAwsAccount
         where
        toJSON RejectDomainTransferFromAnotherAwsAccount{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest RejectDomainTransferFromAnotherAwsAccount
         where
        type Rs RejectDomainTransferFromAnotherAwsAccount =
             RejectDomainTransferFromAnotherAwsAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RejectDomainTransferFromAnotherAwsAccountResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The RejectDomainTransferFromAnotherAwsAccount response includes the following element.
--
-- /See:/ 'mkRejectDomainTransferFromAnotherAwsAccountResponse' smart constructor.
data RejectDomainTransferFromAnotherAwsAccountResponse = RejectDomainTransferFromAnotherAwsAccountResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was rejected, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectDomainTransferFromAnotherAwsAccountResponse' value with any optional fields omitted.
mkRejectDomainTransferFromAnotherAwsAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectDomainTransferFromAnotherAwsAccountResponse
mkRejectDomainTransferFromAnotherAwsAccountResponse responseStatus
  = RejectDomainTransferFromAnotherAwsAccountResponse'{operationId =
                                                         Core.Nothing,
                                                       responseStatus}

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was rejected, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdtfaaarrsOperationId :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccountResponse (Core.Maybe Types.OperationId)
rdtfaaarrsOperationId = Lens.field @"operationId"
{-# INLINEABLE rdtfaaarrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdtfaaarrsResponseStatus :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccountResponse Core.Int
rdtfaaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdtfaaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
