{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about your private certificate authority (CA) or one that has been shared with you. You specify the private CA on input by its ARN (Amazon Resource Name). The output contains the status of your CA. This can be any of the following: 
--
--
--     * @CREATING@ - ACM Private CA is creating your private certificate authority.
--
--
--     * @PENDING_CERTIFICATE@ - The certificate is pending. You must use your ACM Private CA-hosted or on-premises root or subordinate CA to sign your private CA CSR and then import it into PCA. 
--
--
--     * @ACTIVE@ - Your private CA is active.
--
--
--     * @DISABLED@ - Your private CA has been disabled.
--
--
--     * @EXPIRED@ - Your private CA certificate has expired.
--
--
--     * @FAILED@ - Your private CA has failed. Your CA can fail because of problems such a network outage or backend AWS failure or other errors. A failed CA can never return to the pending state. You must create a new CA. 
--
--
--     * @DELETED@ - Your private CA is within the restoration period, after which it is permanently deleted. The length of time remaining in the CA's restoration period is also included in this action's output.
--
--
module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
    (
    -- * Creating a request
      DescribeCertificateAuthority (..)
    , mkDescribeCertificateAuthority
    -- ** Request lenses
    , dCertificateAuthorityArn

    -- * Destructuring the response
    , DescribeCertificateAuthorityResponse (..)
    , mkDescribeCertificateAuthorityResponse
    -- ** Response lenses
    , dcarrsCertificateAuthority
    , dcarrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCertificateAuthority' smart constructor.
newtype DescribeCertificateAuthority = DescribeCertificateAuthority'
  { certificateAuthorityArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCertificateAuthority' value with any optional fields omitted.
mkDescribeCertificateAuthority
    :: Types.Arn -- ^ 'certificateAuthorityArn'
    -> DescribeCertificateAuthority
mkDescribeCertificateAuthority certificateAuthorityArn
  = DescribeCertificateAuthority'{certificateAuthorityArn}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ . 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateAuthorityArn :: Lens.Lens' DescribeCertificateAuthority Types.Arn
dCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE dCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

instance Core.ToQuery DescribeCertificateAuthority where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCertificateAuthority where
        toHeaders DescribeCertificateAuthority{..}
          = Core.pure
              ("X-Amz-Target", "ACMPrivateCA.DescribeCertificateAuthority")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCertificateAuthority where
        toJSON DescribeCertificateAuthority{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn)])

instance Core.AWSRequest DescribeCertificateAuthority where
        type Rs DescribeCertificateAuthority =
             DescribeCertificateAuthorityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCertificateAuthorityResponse' Core.<$>
                   (x Core..:? "CertificateAuthority") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCertificateAuthorityResponse' smart constructor.
data DescribeCertificateAuthorityResponse = DescribeCertificateAuthorityResponse'
  { certificateAuthority :: Core.Maybe Types.CertificateAuthority
    -- ^ A <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CertificateAuthority.html CertificateAuthority> structure that contains information about your private CA.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCertificateAuthorityResponse' value with any optional fields omitted.
mkDescribeCertificateAuthorityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCertificateAuthorityResponse
mkDescribeCertificateAuthorityResponse responseStatus
  = DescribeCertificateAuthorityResponse'{certificateAuthority =
                                            Core.Nothing,
                                          responseStatus}

-- | A <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CertificateAuthority.html CertificateAuthority> structure that contains information about your private CA.
--
-- /Note:/ Consider using 'certificateAuthority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarrsCertificateAuthority :: Lens.Lens' DescribeCertificateAuthorityResponse (Core.Maybe Types.CertificateAuthority)
dcarrsCertificateAuthority = Lens.field @"certificateAuthority"
{-# INLINEABLE dcarrsCertificateAuthority #-}
{-# DEPRECATED certificateAuthority "Use generic-lens or generic-optics with 'certificateAuthority' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarrsResponseStatus :: Lens.Lens' DescribeCertificateAuthorityResponse Core.Int
dcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
