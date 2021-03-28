{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the transfer of a domain from another AWS account to the current AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> . 
--
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ . 
module Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount
    (
    -- * Creating a request
      AcceptDomainTransferFromAnotherAwsAccount (..)
    , mkAcceptDomainTransferFromAnotherAwsAccount
    -- ** Request lenses
    , adtfaaaDomainName
    , adtfaaaPassword

    -- * Destructuring the response
    , AcceptDomainTransferFromAnotherAwsAccountResponse (..)
    , mkAcceptDomainTransferFromAnotherAwsAccountResponse
    -- ** Response lenses
    , adtfaaarrsOperationId
    , adtfaaarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The AcceptDomainTransferFromAnotherAwsAccount request includes the following elements.
--
-- /See:/ 'mkAcceptDomainTransferFromAnotherAwsAccount' smart constructor.
data AcceptDomainTransferFromAnotherAwsAccount = AcceptDomainTransferFromAnotherAwsAccount'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request. 
  , password :: Core.Text
    -- ^ The password that was returned by the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptDomainTransferFromAnotherAwsAccount' value with any optional fields omitted.
mkAcceptDomainTransferFromAnotherAwsAccount
    :: Types.DomainName -- ^ 'domainName'
    -> Core.Text -- ^ 'password'
    -> AcceptDomainTransferFromAnotherAwsAccount
mkAcceptDomainTransferFromAnotherAwsAccount domainName password
  = AcceptDomainTransferFromAnotherAwsAccount'{domainName, password}

-- | The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request. 
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtfaaaDomainName :: Lens.Lens' AcceptDomainTransferFromAnotherAwsAccount Types.DomainName
adtfaaaDomainName = Lens.field @"domainName"
{-# INLINEABLE adtfaaaDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The password that was returned by the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request. 
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtfaaaPassword :: Lens.Lens' AcceptDomainTransferFromAnotherAwsAccount Core.Text
adtfaaaPassword = Lens.field @"password"
{-# INLINEABLE adtfaaaPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

instance Core.ToQuery AcceptDomainTransferFromAnotherAwsAccount
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AcceptDomainTransferFromAnotherAwsAccount
         where
        toHeaders AcceptDomainTransferFromAnotherAwsAccount{..}
          = Core.pure
              ("X-Amz-Target",
               "Route53Domains_v20140515.AcceptDomainTransferFromAnotherAwsAccount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AcceptDomainTransferFromAnotherAwsAccount
         where
        toJSON AcceptDomainTransferFromAnotherAwsAccount{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  Core.Just ("Password" Core..= password)])

instance Core.AWSRequest AcceptDomainTransferFromAnotherAwsAccount
         where
        type Rs AcceptDomainTransferFromAnotherAwsAccount =
             AcceptDomainTransferFromAnotherAwsAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AcceptDomainTransferFromAnotherAwsAccountResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The AcceptDomainTransferFromAnotherAwsAccount response includes the following element.
--
-- /See:/ 'mkAcceptDomainTransferFromAnotherAwsAccountResponse' smart constructor.
data AcceptDomainTransferFromAnotherAwsAccountResponse = AcceptDomainTransferFromAnotherAwsAccountResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptDomainTransferFromAnotherAwsAccountResponse' value with any optional fields omitted.
mkAcceptDomainTransferFromAnotherAwsAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptDomainTransferFromAnotherAwsAccountResponse
mkAcceptDomainTransferFromAnotherAwsAccountResponse responseStatus
  = AcceptDomainTransferFromAnotherAwsAccountResponse'{operationId =
                                                         Core.Nothing,
                                                       responseStatus}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtfaaarrsOperationId :: Lens.Lens' AcceptDomainTransferFromAnotherAwsAccountResponse (Core.Maybe Types.OperationId)
adtfaaarrsOperationId = Lens.field @"operationId"
{-# INLINEABLE adtfaaarrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtfaaarrsResponseStatus :: Lens.Lens' AcceptDomainTransferFromAnotherAwsAccountResponse Core.Int
adtfaaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adtfaaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
