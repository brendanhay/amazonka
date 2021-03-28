{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.TransferCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers the specified certificate to the specified AWS account.
--
-- You can cancel the transfer until it is acknowledged by the recipient.
-- No notification is sent to the transfer destination's account. It is up to the caller to notify the transfer target.
-- The certificate being transferred must not be in the ACTIVE state. You can use the UpdateCertificate API to deactivate it.
-- The certificate must not have any policies attached to it. You can use the DetachPrincipalPolicy API to detach them.
module Network.AWS.IoT.TransferCertificate
    (
    -- * Creating a request
      TransferCertificate (..)
    , mkTransferCertificate
    -- ** Request lenses
    , tcCertificateId
    , tcTargetAwsAccount
    , tcTransferMessage

    -- * Destructuring the response
    , TransferCertificateResponse (..)
    , mkTransferCertificateResponse
    -- ** Response lenses
    , tcrrsTransferredCertificateArn
    , tcrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the TransferCertificate operation.
--
-- /See:/ 'mkTransferCertificate' smart constructor.
data TransferCertificate = TransferCertificate'
  { certificateId :: Types.CertificateId
    -- ^ The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
  , targetAwsAccount :: Types.TargetAwsAccount
    -- ^ The AWS account.
  , transferMessage :: Core.Maybe Types.TransferMessage
    -- ^ The transfer message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransferCertificate' value with any optional fields omitted.
mkTransferCertificate
    :: Types.CertificateId -- ^ 'certificateId'
    -> Types.TargetAwsAccount -- ^ 'targetAwsAccount'
    -> TransferCertificate
mkTransferCertificate certificateId targetAwsAccount
  = TransferCertificate'{certificateId, targetAwsAccount,
                         transferMessage = Core.Nothing}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcCertificateId :: Lens.Lens' TransferCertificate Types.CertificateId
tcCertificateId = Lens.field @"certificateId"
{-# INLINEABLE tcCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The AWS account.
--
-- /Note:/ Consider using 'targetAwsAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTargetAwsAccount :: Lens.Lens' TransferCertificate Types.TargetAwsAccount
tcTargetAwsAccount = Lens.field @"targetAwsAccount"
{-# INLINEABLE tcTargetAwsAccount #-}
{-# DEPRECATED targetAwsAccount "Use generic-lens or generic-optics with 'targetAwsAccount' instead"  #-}

-- | The transfer message.
--
-- /Note:/ Consider using 'transferMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTransferMessage :: Lens.Lens' TransferCertificate (Core.Maybe Types.TransferMessage)
tcTransferMessage = Lens.field @"transferMessage"
{-# INLINEABLE tcTransferMessage #-}
{-# DEPRECATED transferMessage "Use generic-lens or generic-optics with 'transferMessage' instead"  #-}

instance Core.ToQuery TransferCertificate where
        toQuery TransferCertificate{..}
          = Core.toQueryPair "targetAwsAccount" targetAwsAccount

instance Core.ToHeaders TransferCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON TransferCertificate where
        toJSON TransferCertificate{..}
          = Core.object
              (Core.catMaybes
                 [("transferMessage" Core..=) Core.<$> transferMessage])

instance Core.AWSRequest TransferCertificate where
        type Rs TransferCertificate = TransferCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/transfer-certificate/" Core.<> Core.toText certificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TransferCertificateResponse' Core.<$>
                   (x Core..:? "transferredCertificateArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the TransferCertificate operation.
--
-- /See:/ 'mkTransferCertificateResponse' smart constructor.
data TransferCertificateResponse = TransferCertificateResponse'
  { transferredCertificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The ARN of the certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransferCertificateResponse' value with any optional fields omitted.
mkTransferCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TransferCertificateResponse
mkTransferCertificateResponse responseStatus
  = TransferCertificateResponse'{transferredCertificateArn =
                                   Core.Nothing,
                                 responseStatus}

-- | The ARN of the certificate.
--
-- /Note:/ Consider using 'transferredCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrrsTransferredCertificateArn :: Lens.Lens' TransferCertificateResponse (Core.Maybe Types.CertificateArn)
tcrrsTransferredCertificateArn = Lens.field @"transferredCertificateArn"
{-# INLINEABLE tcrrsTransferredCertificateArn #-}
{-# DEPRECATED transferredCertificateArn "Use generic-lens or generic-optics with 'transferredCertificateArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrrsResponseStatus :: Lens.Lens' TransferCertificateResponse Core.Int
tcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
