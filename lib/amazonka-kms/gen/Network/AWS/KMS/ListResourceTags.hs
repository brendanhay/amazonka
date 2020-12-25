{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListResourceTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all tags for the specified customer master key (CMK).
--
-- You cannot perform this operation on a CMK in a different AWS account.
module Network.AWS.KMS.ListResourceTags
  ( -- * Creating a request
    ListResourceTags (..),
    mkListResourceTags,

    -- ** Request lenses
    lrtKeyId,
    lrtLimit,
    lrtMarker,

    -- * Destructuring the response
    ListResourceTagsResponse (..),
    mkListResourceTagsResponse,

    -- ** Response lenses
    lrtrrsNextMarker,
    lrtrrsTags,
    lrtrrsTruncated,
    lrtrrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResourceTags' smart constructor.
data ListResourceTags = ListResourceTags'
  { -- | A unique identifier for the customer master key (CMK).
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
    keyId :: Types.KeyId,
    -- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
    limit :: Core.Maybe Core.Natural,
    -- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
    --
    -- Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
    marker :: Core.Maybe Types.MarkerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceTags' value with any optional fields omitted.
mkListResourceTags ::
  -- | 'keyId'
  Types.KeyId ->
  ListResourceTags
mkListResourceTags keyId =
  ListResourceTags'
    { keyId,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtKeyId :: Lens.Lens' ListResourceTags Types.KeyId
lrtKeyId = Lens.field @"keyId"
{-# DEPRECATED lrtKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtLimit :: Lens.Lens' ListResourceTags (Core.Maybe Core.Natural)
lrtLimit = Lens.field @"limit"
{-# DEPRECATED lrtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMarker :: Lens.Lens' ListResourceTags (Core.Maybe Types.MarkerType)
lrtMarker = Lens.field @"marker"
{-# DEPRECATED lrtMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListResourceTags where
  toJSON ListResourceTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.AWSRequest ListResourceTags where
  type Rs ListResourceTags = ListResourceTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.ListResourceTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceTagsResponse'
            Core.<$> (x Core..:? "NextMarker")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (x Core..:? "Truncated")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListResourceTagsResponse' smart constructor.
data ListResourceTagsResponse = ListResourceTagsResponse'
  { -- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
    --
    -- Do not assume or infer any information from this value.
    nextMarker :: Core.Maybe Types.MarkerType,
    -- | A list of tags. Each tag consists of a tag key and a tag value.
    tags :: Core.Maybe [Types.Tag],
    -- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
    truncated :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceTagsResponse' value with any optional fields omitted.
mkListResourceTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListResourceTagsResponse
mkListResourceTagsResponse responseStatus =
  ListResourceTagsResponse'
    { nextMarker = Core.Nothing,
      tags = Core.Nothing,
      truncated = Core.Nothing,
      responseStatus
    }

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- Do not assume or infer any information from this value.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsNextMarker :: Lens.Lens' ListResourceTagsResponse (Core.Maybe Types.MarkerType)
lrtrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lrtrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A list of tags. Each tag consists of a tag key and a tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsTags :: Lens.Lens' ListResourceTagsResponse (Core.Maybe [Types.Tag])
lrtrrsTags = Lens.field @"tags"
{-# DEPRECATED lrtrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsTruncated :: Lens.Lens' ListResourceTagsResponse (Core.Maybe Core.Bool)
lrtrrsTruncated = Lens.field @"truncated"
{-# DEPRECATED lrtrrsTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsResponseStatus :: Lens.Lens' ListResourceTagsResponse Core.Int
lrtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
