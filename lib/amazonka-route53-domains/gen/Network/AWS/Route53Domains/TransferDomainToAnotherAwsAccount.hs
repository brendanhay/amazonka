{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.TransferDomainToAnotherAwsAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers a domain from the current AWS account to another AWS account. Note the following:
--
--
--     * The AWS account that you're transferring the domain to must accept the transfer. If the other account doesn't accept the transfer within 3 days, we cancel the transfer. See <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> . 
--
--
--     * You can cancel the transfer before the other account accepts it. See <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_CancelDomainTransferToAnotherAwsAccount.html CancelDomainTransferToAnotherAwsAccount> . 
--
--
--     * The other account can reject the transfer. See <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_RejectDomainTransferFromAnotherAwsAccount.html RejectDomainTransferFromAnotherAwsAccount> . 
--
--
-- /Important:/ When you transfer a domain from one AWS account to another, Route 53 doesn't transfer the hosted zone that is associated with the domain. DNS resolution isn't affected if the domain and the hosted zone are owned by separate accounts, so transferring the hosted zone is optional. For information about transferring the hosted zone to another AWS account, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/hosted-zones-migrating.html Migrating a Hosted Zone to a Different AWS Account> in the /Amazon Route 53 Developer Guide/ .
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ . 
module Network.AWS.Route53Domains.TransferDomainToAnotherAwsAccount
    (
    -- * Creating a request
      TransferDomainToAnotherAwsAccount (..)
    , mkTransferDomainToAnotherAwsAccount
    -- ** Request lenses
    , tdtaaaDomainName
    , tdtaaaAccountId

    -- * Destructuring the response
    , TransferDomainToAnotherAwsAccountResponse (..)
    , mkTransferDomainToAnotherAwsAccountResponse
    -- ** Response lenses
    , tdtaaarrsOperationId
    , tdtaaarrsPassword
    , tdtaaarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The TransferDomainToAnotherAwsAccount request includes the following elements.
--
-- /See:/ 'mkTransferDomainToAnotherAwsAccount' smart constructor.
data TransferDomainToAnotherAwsAccount = TransferDomainToAnotherAwsAccount'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to transfer from the current AWS account to another account.
  , accountId :: Types.AccountId
    -- ^ The account ID of the AWS account that you want to transfer the domain to, for example, @111122223333@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransferDomainToAnotherAwsAccount' value with any optional fields omitted.
mkTransferDomainToAnotherAwsAccount
    :: Types.DomainName -- ^ 'domainName'
    -> Types.AccountId -- ^ 'accountId'
    -> TransferDomainToAnotherAwsAccount
mkTransferDomainToAnotherAwsAccount domainName accountId
  = TransferDomainToAnotherAwsAccount'{domainName, accountId}

-- | The name of the domain that you want to transfer from the current AWS account to another account.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaaDomainName :: Lens.Lens' TransferDomainToAnotherAwsAccount Types.DomainName
tdtaaaDomainName = Lens.field @"domainName"
{-# INLINEABLE tdtaaaDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The account ID of the AWS account that you want to transfer the domain to, for example, @111122223333@ .
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaaAccountId :: Lens.Lens' TransferDomainToAnotherAwsAccount Types.AccountId
tdtaaaAccountId = Lens.field @"accountId"
{-# INLINEABLE tdtaaaAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

instance Core.ToQuery TransferDomainToAnotherAwsAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TransferDomainToAnotherAwsAccount where
        toHeaders TransferDomainToAnotherAwsAccount{..}
          = Core.pure
              ("X-Amz-Target",
               "Route53Domains_v20140515.TransferDomainToAnotherAwsAccount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TransferDomainToAnotherAwsAccount where
        toJSON TransferDomainToAnotherAwsAccount{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  Core.Just ("AccountId" Core..= accountId)])

instance Core.AWSRequest TransferDomainToAnotherAwsAccount where
        type Rs TransferDomainToAnotherAwsAccount =
             TransferDomainToAnotherAwsAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TransferDomainToAnotherAwsAccountResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> x Core..:? "Password" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The @TransferDomainToAnotherAwsAccount@ response includes the following elements.
--
-- /See:/ 'mkTransferDomainToAnotherAwsAccountResponse' smart constructor.
data TransferDomainToAnotherAwsAccountResponse = TransferDomainToAnotherAwsAccountResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
  , password :: Core.Maybe Core.Text
    -- ^ To finish transferring a domain to another AWS account, the account that the domain is being transferred to must submit an <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> request. The request must include the value of the @Password@ element that was returned in the @TransferDomainToAnotherAwsAccount@ response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransferDomainToAnotherAwsAccountResponse' value with any optional fields omitted.
mkTransferDomainToAnotherAwsAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TransferDomainToAnotherAwsAccountResponse
mkTransferDomainToAnotherAwsAccountResponse responseStatus
  = TransferDomainToAnotherAwsAccountResponse'{operationId =
                                                 Core.Nothing,
                                               password = Core.Nothing, responseStatus}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaarrsOperationId :: Lens.Lens' TransferDomainToAnotherAwsAccountResponse (Core.Maybe Types.OperationId)
tdtaaarrsOperationId = Lens.field @"operationId"
{-# INLINEABLE tdtaaarrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | To finish transferring a domain to another AWS account, the account that the domain is being transferred to must submit an <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> request. The request must include the value of the @Password@ element that was returned in the @TransferDomainToAnotherAwsAccount@ response.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaarrsPassword :: Lens.Lens' TransferDomainToAnotherAwsAccountResponse (Core.Maybe Core.Text)
tdtaaarrsPassword = Lens.field @"password"
{-# INLINEABLE tdtaaarrsPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaarrsResponseStatus :: Lens.Lens' TransferDomainToAnotherAwsAccountResponse Core.Int
tdtaaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tdtaaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
