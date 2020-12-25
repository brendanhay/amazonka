{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.RemoveTagsFromCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more tags from an ACM certificate. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this function, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value.
--
-- To add tags to a certificate, use the 'AddTagsToCertificate' action. To view all of the tags that have been applied to a specific ACM certificate, use the 'ListTagsForCertificate' action.
module Network.AWS.CertificateManager.RemoveTagsFromCertificate
  ( -- * Creating a request
    RemoveTagsFromCertificate (..),
    mkRemoveTagsFromCertificate,

    -- ** Request lenses
    rtfcCertificateArn,
    rtfcTags,

    -- * Destructuring the response
    RemoveTagsFromCertificateResponse (..),
    mkRemoveTagsFromCertificateResponse,
  )
where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveTagsFromCertificate' smart constructor.
data RemoveTagsFromCertificate = RemoveTagsFromCertificate'
  { -- | String that contains the ARN of the ACM Certificate with one or more tags that you want to remove. This must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    certificateArn :: Types.CertificateArn,
    -- | The key-value pair that defines the tag to remove.
    tags :: Core.NonEmpty Types.Tag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromCertificate' value with any optional fields omitted.
mkRemoveTagsFromCertificate ::
  -- | 'certificateArn'
  Types.CertificateArn ->
  -- | 'tags'
  Core.NonEmpty Types.Tag ->
  RemoveTagsFromCertificate
mkRemoveTagsFromCertificate certificateArn tags =
  RemoveTagsFromCertificate' {certificateArn, tags}

-- | String that contains the ARN of the ACM Certificate with one or more tags that you want to remove. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcCertificateArn :: Lens.Lens' RemoveTagsFromCertificate Types.CertificateArn
rtfcCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED rtfcCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The key-value pair that defines the tag to remove.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcTags :: Lens.Lens' RemoveTagsFromCertificate (Core.NonEmpty Types.Tag)
rtfcTags = Lens.field @"tags"
{-# DEPRECATED rtfcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON RemoveTagsFromCertificate where
  toJSON RemoveTagsFromCertificate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CertificateArn" Core..= certificateArn),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest RemoveTagsFromCertificate where
  type
    Rs RemoveTagsFromCertificate =
      RemoveTagsFromCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CertificateManager.RemoveTagsFromCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RemoveTagsFromCertificateResponse'

-- | /See:/ 'mkRemoveTagsFromCertificateResponse' smart constructor.
data RemoveTagsFromCertificateResponse = RemoveTagsFromCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromCertificateResponse' value with any optional fields omitted.
mkRemoveTagsFromCertificateResponse ::
  RemoveTagsFromCertificateResponse
mkRemoveTagsFromCertificateResponse =
  RemoveTagsFromCertificateResponse'
