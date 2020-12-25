{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.AddTagsToCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an ACM certificate. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a @key@ and an optional @value@ . You specify the certificate on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair.
--
-- You can apply a tag to just one certificate if you want to identify a specific characteristic of that certificate, or you can apply the same tag to multiple certificates if you want to filter for a common relationship among those certificates. Similarly, you can apply the same tag to multiple resources if you want to specify a relationship among those resources. For example, you can add the same tag to an ACM certificate and an Elastic Load Balancing load balancer to indicate that they are both used by the same website. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/tags.html Tagging ACM certificates> .
-- To remove one or more tags, use the 'RemoveTagsFromCertificate' action. To view all of the tags that have been applied to the certificate, use the 'ListTagsForCertificate' action.
module Network.AWS.CertificateManager.AddTagsToCertificate
  ( -- * Creating a request
    AddTagsToCertificate (..),
    mkAddTagsToCertificate,

    -- ** Request lenses
    attcCertificateArn,
    attcTags,

    -- * Destructuring the response
    AddTagsToCertificateResponse (..),
    mkAddTagsToCertificateResponse,
  )
where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddTagsToCertificate' smart constructor.
data AddTagsToCertificate = AddTagsToCertificate'
  { -- | String that contains the ARN of the ACM certificate to which the tag is to be applied. This must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    certificateArn :: Types.CertificateArn,
    -- | The key-value pair that defines the tag. The tag value is optional.
    tags :: Core.NonEmpty Types.Tag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToCertificate' value with any optional fields omitted.
mkAddTagsToCertificate ::
  -- | 'certificateArn'
  Types.CertificateArn ->
  -- | 'tags'
  Core.NonEmpty Types.Tag ->
  AddTagsToCertificate
mkAddTagsToCertificate certificateArn tags =
  AddTagsToCertificate' {certificateArn, tags}

-- | String that contains the ARN of the ACM certificate to which the tag is to be applied. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attcCertificateArn :: Lens.Lens' AddTagsToCertificate Types.CertificateArn
attcCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED attcCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The key-value pair that defines the tag. The tag value is optional.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attcTags :: Lens.Lens' AddTagsToCertificate (Core.NonEmpty Types.Tag)
attcTags = Lens.field @"tags"
{-# DEPRECATED attcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON AddTagsToCertificate where
  toJSON AddTagsToCertificate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CertificateArn" Core..= certificateArn),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest AddTagsToCertificate where
  type Rs AddTagsToCertificate = AddTagsToCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CertificateManager.AddTagsToCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull AddTagsToCertificateResponse'

-- | /See:/ 'mkAddTagsToCertificateResponse' smart constructor.
data AddTagsToCertificateResponse = AddTagsToCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToCertificateResponse' value with any optional fields omitted.
mkAddTagsToCertificateResponse ::
  AddTagsToCertificateResponse
mkAddTagsToCertificateResponse = AddTagsToCertificateResponse'
