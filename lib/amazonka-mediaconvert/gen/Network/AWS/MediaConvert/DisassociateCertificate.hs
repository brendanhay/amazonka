{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DisassociateCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an association between the Amazon Resource Name (ARN) of an AWS Certificate Manager (ACM) certificate and an AWS Elemental MediaConvert resource.
module Network.AWS.MediaConvert.DisassociateCertificate
    (
    -- * Creating a request
      DisassociateCertificate (..)
    , mkDisassociateCertificate
    -- ** Request lenses
    , dcArn

    -- * Destructuring the response
    , DisassociateCertificateResponse (..)
    , mkDisassociateCertificateResponse
    -- ** Response lenses
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateCertificate' smart constructor.
newtype DisassociateCertificate = DisassociateCertificate'
  { arn :: Core.Text
    -- ^ The ARN of the ACM certificate that you want to disassociate from your MediaConvert resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateCertificate' value with any optional fields omitted.
mkDisassociateCertificate
    :: Core.Text -- ^ 'arn'
    -> DisassociateCertificate
mkDisassociateCertificate arn = DisassociateCertificate'{arn}

-- | The ARN of the ACM certificate that you want to disassociate from your MediaConvert resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcArn :: Lens.Lens' DisassociateCertificate Core.Text
dcArn = Lens.field @"arn"
{-# INLINEABLE dcArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DisassociateCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateCertificate where
        toHeaders DisassociateCertificate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DisassociateCertificate where
        type Rs DisassociateCertificate = DisassociateCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/2017-08-29/certificates/" Core.<> Core.toText arn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateCertificateResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateCertificateResponse' smart constructor.
newtype DisassociateCertificateResponse = DisassociateCertificateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateCertificateResponse' value with any optional fields omitted.
mkDisassociateCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateCertificateResponse
mkDisassociateCertificateResponse responseStatus
  = DisassociateCertificateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DisassociateCertificateResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
