{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of aliases in the caller's AWS account and region. You cannot list aliases in other accounts. For more information about aliases, see 'CreateAlias' .
--
-- By default, the ListAliases command returns all aliases in the account and region. To get only the aliases that point to a particular customer master key (CMK), use the @KeyId@ parameter.
-- The @ListAliases@ response can include aliases that you created and associated with your customer managed CMKs, and aliases that AWS created and associated with AWS managed CMKs in your account. You can recognize AWS aliases because their names have the format @aws/<service-name>@ , such as @aws/dynamodb@ .
-- The response might also include aliases that have no @TargetKeyId@ field. These are predefined aliases that AWS has created but has not yet associated with a CMK. Aliases that AWS creates in your account, including predefined aliases, do not count against your <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#aliases-limit AWS KMS aliases quota> .
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListAliases
  ( -- * Creating a request
    ListAliases (..),
    mkListAliases,

    -- ** Request lenses
    laKeyId,
    laLimit,
    laMarker,

    -- * Destructuring the response
    ListAliasesResponse (..),
    mkListAliasesResponse,

    -- ** Response lenses
    larrsAliases,
    larrsNextMarker,
    larrsTruncated,
    larrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | Lists only aliases that refer to the specified CMK. The value of this parameter can be the ID or Amazon Resource Name (ARN) of a CMK in the caller's account and region. You cannot use an alias name or alias ARN in this value.
    --
    -- This parameter is optional. If you omit it, @ListAliases@ returns all aliases in the account and region.
    keyId :: Core.Maybe Types.KeyIdType,
    -- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
    limit :: Core.Maybe Core.Natural,
    -- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
    marker :: Core.Maybe Types.MarkerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAliases' value with any optional fields omitted.
mkListAliases ::
  ListAliases
mkListAliases =
  ListAliases'
    { keyId = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | Lists only aliases that refer to the specified CMK. The value of this parameter can be the ID or Amazon Resource Name (ARN) of a CMK in the caller's account and region. You cannot use an alias name or alias ARN in this value.
--
-- This parameter is optional. If you omit it, @ListAliases@ returns all aliases in the account and region.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laKeyId :: Lens.Lens' ListAliases (Core.Maybe Types.KeyIdType)
laKeyId = Lens.field @"keyId"
{-# DEPRECATED laKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLimit :: Lens.Lens' ListAliases (Core.Maybe Core.Natural)
laLimit = Lens.field @"limit"
{-# DEPRECATED laLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMarker :: Lens.Lens' ListAliases (Core.Maybe Types.MarkerType)
laMarker = Lens.field @"marker"
{-# DEPRECATED laMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListAliases where
  toJSON ListAliases {..} =
    Core.object
      ( Core.catMaybes
          [ ("KeyId" Core..=) Core.<$> keyId,
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.AWSRequest ListAliases where
  type Rs ListAliases = ListAliasesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.ListAliases")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Core.<$> (x Core..:? "Aliases")
            Core.<*> (x Core..:? "NextMarker")
            Core.<*> (x Core..:? "Truncated")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAliases where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"truncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"nextMarker") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | A list of aliases.
    aliases :: Core.Maybe [Types.AliasListEntry],
    -- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Core.Maybe Types.MarkerType,
    -- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
    truncated :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAliasesResponse' value with any optional fields omitted.
mkListAliasesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAliasesResponse
mkListAliasesResponse responseStatus =
  ListAliasesResponse'
    { aliases = Core.Nothing,
      nextMarker = Core.Nothing,
      truncated = Core.Nothing,
      responseStatus
    }

-- | A list of aliases.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsAliases :: Lens.Lens' ListAliasesResponse (Core.Maybe [Types.AliasListEntry])
larrsAliases = Lens.field @"aliases"
{-# DEPRECATED larrsAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextMarker :: Lens.Lens' ListAliasesResponse (Core.Maybe Types.MarkerType)
larrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED larrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsTruncated :: Lens.Lens' ListAliasesResponse (Core.Maybe Core.Bool)
larrsTruncated = Lens.field @"truncated"
{-# DEPRECATED larrsTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAliasesResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED larrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
