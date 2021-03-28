{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ListTagsForCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been applied to the ACM certificate. Use the certificate's Amazon Resource Name (ARN) to specify the certificate. To add a tag to an ACM certificate, use the 'AddTagsToCertificate' action. To delete a tag, use the 'RemoveTagsFromCertificate' action. 
module Network.AWS.CertificateManager.ListTagsForCertificate
    (
    -- * Creating a request
      ListTagsForCertificate (..)
    , mkListTagsForCertificate
    -- ** Request lenses
    , ltfcCertificateArn

    -- * Destructuring the response
    , ListTagsForCertificateResponse (..)
    , mkListTagsForCertificateResponse
    -- ** Response lenses
    , ltfcrrsTags
    , ltfcrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForCertificate' smart constructor.
newtype ListTagsForCertificate = ListTagsForCertificate'
  { certificateArn :: Types.CertificateArn
    -- ^ String that contains the ARN of the ACM certificate for which you want to list the tags. This must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForCertificate' value with any optional fields omitted.
mkListTagsForCertificate
    :: Types.CertificateArn -- ^ 'certificateArn'
    -> ListTagsForCertificate
mkListTagsForCertificate certificateArn
  = ListTagsForCertificate'{certificateArn}

-- | String that contains the ARN of the ACM certificate for which you want to list the tags. This must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . 
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfcCertificateArn :: Lens.Lens' ListTagsForCertificate Types.CertificateArn
ltfcCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE ltfcCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

instance Core.ToQuery ListTagsForCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForCertificate where
        toHeaders ListTagsForCertificate{..}
          = Core.pure
              ("X-Amz-Target", "CertificateManager.ListTagsForCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsForCertificate where
        toJSON ListTagsForCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateArn" Core..= certificateArn)])

instance Core.AWSRequest ListTagsForCertificate where
        type Rs ListTagsForCertificate = ListTagsForCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForCertificateResponse' Core.<$>
                   (x Core..:? "Tags") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsForCertificateResponse' smart constructor.
data ListTagsForCertificateResponse = ListTagsForCertificateResponse'
  { tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The key-value pairs that define the applied tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForCertificateResponse' value with any optional fields omitted.
mkListTagsForCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForCertificateResponse
mkListTagsForCertificateResponse responseStatus
  = ListTagsForCertificateResponse'{tags = Core.Nothing,
                                    responseStatus}

-- | The key-value pairs that define the applied tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfcrrsTags :: Lens.Lens' ListTagsForCertificateResponse (Core.Maybe (Core.NonEmpty Types.Tag))
ltfcrrsTags = Lens.field @"tags"
{-# INLINEABLE ltfcrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfcrrsResponseStatus :: Lens.Lens' ListTagsForCertificateResponse Core.Int
ltfcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
