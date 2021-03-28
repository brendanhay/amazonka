{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.CancelDomainTransferToAnotherAwsAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the transfer of a domain from the current AWS account to another AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> . 
--
-- /Important:/ You must cancel the transfer before the other AWS account accepts the transfer using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> .
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ . 
module Network.AWS.Route53Domains.CancelDomainTransferToAnotherAwsAccount
    (
    -- * Creating a request
      CancelDomainTransferToAnotherAwsAccount (..)
    , mkCancelDomainTransferToAnotherAwsAccount
    -- ** Request lenses
    , cdttaaaDomainName

    -- * Destructuring the response
    , CancelDomainTransferToAnotherAwsAccountResponse (..)
    , mkCancelDomainTransferToAnotherAwsAccountResponse
    -- ** Response lenses
    , cdttaaarrsOperationId
    , cdttaaarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The CancelDomainTransferToAnotherAwsAccount request includes the following element.
--
-- /See:/ 'mkCancelDomainTransferToAnotherAwsAccount' smart constructor.
newtype CancelDomainTransferToAnotherAwsAccount = CancelDomainTransferToAnotherAwsAccount'
  { domainName :: Types.DomainName
    -- ^ The name of the domain for which you want to cancel the transfer to another AWS account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelDomainTransferToAnotherAwsAccount' value with any optional fields omitted.
mkCancelDomainTransferToAnotherAwsAccount
    :: Types.DomainName -- ^ 'domainName'
    -> CancelDomainTransferToAnotherAwsAccount
mkCancelDomainTransferToAnotherAwsAccount domainName
  = CancelDomainTransferToAnotherAwsAccount'{domainName}

-- | The name of the domain for which you want to cancel the transfer to another AWS account.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdttaaaDomainName :: Lens.Lens' CancelDomainTransferToAnotherAwsAccount Types.DomainName
cdttaaaDomainName = Lens.field @"domainName"
{-# INLINEABLE cdttaaaDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery CancelDomainTransferToAnotherAwsAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelDomainTransferToAnotherAwsAccount
         where
        toHeaders CancelDomainTransferToAnotherAwsAccount{..}
          = Core.pure
              ("X-Amz-Target",
               "Route53Domains_v20140515.CancelDomainTransferToAnotherAwsAccount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelDomainTransferToAnotherAwsAccount
         where
        toJSON CancelDomainTransferToAnotherAwsAccount{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest CancelDomainTransferToAnotherAwsAccount
         where
        type Rs CancelDomainTransferToAnotherAwsAccount =
             CancelDomainTransferToAnotherAwsAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelDomainTransferToAnotherAwsAccountResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The @CancelDomainTransferToAnotherAwsAccount@ response includes the following element.
--
-- /See:/ 'mkCancelDomainTransferToAnotherAwsAccountResponse' smart constructor.
data CancelDomainTransferToAnotherAwsAccountResponse = CancelDomainTransferToAnotherAwsAccountResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was canceled, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelDomainTransferToAnotherAwsAccountResponse' value with any optional fields omitted.
mkCancelDomainTransferToAnotherAwsAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelDomainTransferToAnotherAwsAccountResponse
mkCancelDomainTransferToAnotherAwsAccountResponse responseStatus
  = CancelDomainTransferToAnotherAwsAccountResponse'{operationId =
                                                       Core.Nothing,
                                                     responseStatus}

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was canceled, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdttaaarrsOperationId :: Lens.Lens' CancelDomainTransferToAnotherAwsAccountResponse (Core.Maybe Types.OperationId)
cdttaaarrsOperationId = Lens.field @"operationId"
{-# INLINEABLE cdttaaarrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdttaaarrsResponseStatus :: Lens.Lens' CancelDomainTransferToAnotherAwsAccountResponse Core.Int
cdttaaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdttaaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
