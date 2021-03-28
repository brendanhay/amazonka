{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a private certificate authority (CA). You must provide the Amazon Resource Name (ARN) of the private CA that you want to delete. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. 
--
-- Before you can delete a CA that you have created and activated, you must disable it. To do this, call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> action and set the __CertificateAuthorityStatus__ parameter to @DISABLED@ . 
-- Additionally, you can delete a CA if you are waiting for it to be created (that is, the status of the CA is @CREATING@ ). You can also delete it if the CA has been created but you haven't yet imported the signed certificate into ACM Private CA (that is, the status of the CA is @PENDING_CERTIFICATE@ ). 
-- When you successfully call <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthority.html DeleteCertificateAuthority> , the CA's status changes to @DELETED@ . However, the CA won't be permanently deleted until the restoration period has passed. By default, if you do not set the @PermanentDeletionTimeInDays@ parameter, the CA remains restorable for 30 days. You can set the parameter from 7 to 30 days. The <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DescribeCertificateAuthority.html DescribeCertificateAuthority> action returns the time remaining in the restoration window of a private CA in the @DELETED@ state. To restore an eligible CA, call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RestoreCertificateAuthority.html RestoreCertificateAuthority> action.
module Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
    (
    -- * Creating a request
      DeleteCertificateAuthority (..)
    , mkDeleteCertificateAuthority
    -- ** Request lenses
    , dcaCertificateAuthorityArn
    , dcaPermanentDeletionTimeInDays

    -- * Destructuring the response
    , DeleteCertificateAuthorityResponse (..)
    , mkDeleteCertificateAuthorityResponse
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCertificateAuthority' smart constructor.
data DeleteCertificateAuthority = DeleteCertificateAuthority'
  { certificateAuthorityArn :: Types.CertificateAuthorityArn
    -- ^ The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must have the following form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ . 
  , permanentDeletionTimeInDays :: Core.Maybe Core.Natural
    -- ^ The number of days to make a CA restorable after it has been deleted. This can be anywhere from 7 to 30 days, with 30 being the default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCertificateAuthority' value with any optional fields omitted.
mkDeleteCertificateAuthority
    :: Types.CertificateAuthorityArn -- ^ 'certificateAuthorityArn'
    -> DeleteCertificateAuthority
mkDeleteCertificateAuthority certificateAuthorityArn
  = DeleteCertificateAuthority'{certificateAuthorityArn,
                                permanentDeletionTimeInDays = Core.Nothing}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must have the following form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ . 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaCertificateAuthorityArn :: Lens.Lens' DeleteCertificateAuthority Types.CertificateAuthorityArn
dcaCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE dcaCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | The number of days to make a CA restorable after it has been deleted. This can be anywhere from 7 to 30 days, with 30 being the default.
--
-- /Note:/ Consider using 'permanentDeletionTimeInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaPermanentDeletionTimeInDays :: Lens.Lens' DeleteCertificateAuthority (Core.Maybe Core.Natural)
dcaPermanentDeletionTimeInDays = Lens.field @"permanentDeletionTimeInDays"
{-# INLINEABLE dcaPermanentDeletionTimeInDays #-}
{-# DEPRECATED permanentDeletionTimeInDays "Use generic-lens or generic-optics with 'permanentDeletionTimeInDays' instead"  #-}

instance Core.ToQuery DeleteCertificateAuthority where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCertificateAuthority where
        toHeaders DeleteCertificateAuthority{..}
          = Core.pure
              ("X-Amz-Target", "ACMPrivateCA.DeleteCertificateAuthority")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCertificateAuthority where
        toJSON DeleteCertificateAuthority{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
                  ("PermanentDeletionTimeInDays" Core..=) Core.<$>
                    permanentDeletionTimeInDays])

instance Core.AWSRequest DeleteCertificateAuthority where
        type Rs DeleteCertificateAuthority =
             DeleteCertificateAuthorityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteCertificateAuthorityResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCertificateAuthorityResponse' smart constructor.
data DeleteCertificateAuthorityResponse = DeleteCertificateAuthorityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCertificateAuthorityResponse' value with any optional fields omitted.
mkDeleteCertificateAuthorityResponse
    :: DeleteCertificateAuthorityResponse
mkDeleteCertificateAuthorityResponse
  = DeleteCertificateAuthorityResponse'
