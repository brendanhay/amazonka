{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.UpdateCertificateOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a certificate. Currently, you can use this function to specify whether to opt in to or out of recording your certificate in a certificate transparency log. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> . 
module Network.AWS.CertificateManager.UpdateCertificateOptions
    (
    -- * Creating a request
      UpdateCertificateOptions (..)
    , mkUpdateCertificateOptions
    -- ** Request lenses
    , ucoCertificateArn
    , ucoOptions

    -- * Destructuring the response
    , UpdateCertificateOptionsResponse (..)
    , mkUpdateCertificateOptionsResponse
    ) where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCertificateOptions' smart constructor.
data UpdateCertificateOptions = UpdateCertificateOptions'
  { certificateArn :: Types.Arn
    -- ^ ARN of the requested certificate to update. This must be of the form:
--
-- @arn:aws:acm:us-east-1:/account/ :certificate//12345678-1234-1234-1234-123456789012/ @ 
  , options :: Types.CertificateOptions
    -- ^ Use to update the options for your certificate. Currently, you can specify whether to add your certificate to a transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCertificateOptions' value with any optional fields omitted.
mkUpdateCertificateOptions
    :: Types.Arn -- ^ 'certificateArn'
    -> Types.CertificateOptions -- ^ 'options'
    -> UpdateCertificateOptions
mkUpdateCertificateOptions certificateArn options
  = UpdateCertificateOptions'{certificateArn, options}

-- | ARN of the requested certificate to update. This must be of the form:
--
-- @arn:aws:acm:us-east-1:/account/ :certificate//12345678-1234-1234-1234-123456789012/ @ 
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucoCertificateArn :: Lens.Lens' UpdateCertificateOptions Types.Arn
ucoCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE ucoCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | Use to update the options for your certificate. Currently, you can specify whether to add your certificate to a transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser. 
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucoOptions :: Lens.Lens' UpdateCertificateOptions Types.CertificateOptions
ucoOptions = Lens.field @"options"
{-# INLINEABLE ucoOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

instance Core.ToQuery UpdateCertificateOptions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCertificateOptions where
        toHeaders UpdateCertificateOptions{..}
          = Core.pure
              ("X-Amz-Target", "CertificateManager.UpdateCertificateOptions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateCertificateOptions where
        toJSON UpdateCertificateOptions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateArn" Core..= certificateArn),
                  Core.Just ("Options" Core..= options)])

instance Core.AWSRequest UpdateCertificateOptions where
        type Rs UpdateCertificateOptions = UpdateCertificateOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull UpdateCertificateOptionsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateCertificateOptionsResponse' smart constructor.
data UpdateCertificateOptionsResponse = UpdateCertificateOptionsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCertificateOptionsResponse' value with any optional fields omitted.
mkUpdateCertificateOptionsResponse
    :: UpdateCertificateOptionsResponse
mkUpdateCertificateOptionsResponse
  = UpdateCertificateOptionsResponse'
