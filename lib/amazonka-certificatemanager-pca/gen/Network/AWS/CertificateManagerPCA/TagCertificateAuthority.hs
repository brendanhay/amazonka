{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.TagCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to your private CA. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a key and an optional value. You specify the private CA on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair. You can apply a tag to just one private CA if you want to identify a specific characteristic of that CA, or you can apply the same tag to multiple private CAs if you want to filter for a common relationship among those CAs. To remove one or more tags, use the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UntagCertificateAuthority.html UntagCertificateAuthority> action. Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListTags.html ListTags> action to see what tags are associated with your CA. 
module Network.AWS.CertificateManagerPCA.TagCertificateAuthority
    (
    -- * Creating a request
      TagCertificateAuthority (..)
    , mkTagCertificateAuthority
    -- ** Request lenses
    , tcaCertificateAuthorityArn
    , tcaTags

    -- * Destructuring the response
    , TagCertificateAuthorityResponse (..)
    , mkTagCertificateAuthorityResponse
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagCertificateAuthority' smart constructor.
data TagCertificateAuthority = TagCertificateAuthority'
  { certificateAuthorityArn :: Types.CertificateAuthorityArn
    -- ^ The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ 
  , tags :: Core.NonEmpty Types.Tag
    -- ^ List of tags to be associated with the CA.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagCertificateAuthority' value with any optional fields omitted.
mkTagCertificateAuthority
    :: Types.CertificateAuthorityArn -- ^ 'certificateAuthorityArn'
    -> Core.NonEmpty Types.Tag -- ^ 'tags'
    -> TagCertificateAuthority
mkTagCertificateAuthority certificateAuthorityArn tags
  = TagCertificateAuthority'{certificateAuthorityArn, tags}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcaCertificateAuthorityArn :: Lens.Lens' TagCertificateAuthority Types.CertificateAuthorityArn
tcaCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE tcaCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | List of tags to be associated with the CA.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcaTags :: Lens.Lens' TagCertificateAuthority (Core.NonEmpty Types.Tag)
tcaTags = Lens.field @"tags"
{-# INLINEABLE tcaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery TagCertificateAuthority where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TagCertificateAuthority where
        toHeaders TagCertificateAuthority{..}
          = Core.pure
              ("X-Amz-Target", "ACMPrivateCA.TagCertificateAuthority")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TagCertificateAuthority where
        toJSON TagCertificateAuthority{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest TagCertificateAuthority where
        type Rs TagCertificateAuthority = TagCertificateAuthorityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull TagCertificateAuthorityResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTagCertificateAuthorityResponse' smart constructor.
data TagCertificateAuthorityResponse = TagCertificateAuthorityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagCertificateAuthorityResponse' value with any optional fields omitted.
mkTagCertificateAuthorityResponse
    :: TagCertificateAuthorityResponse
mkTagCertificateAuthorityResponse
  = TagCertificateAuthorityResponse'
