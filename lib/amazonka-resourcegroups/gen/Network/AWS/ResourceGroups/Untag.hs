{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Untag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a specified resource group.
module Network.AWS.ResourceGroups.Untag
  ( -- * Creating a request
    Untag (..),
    mkUntag,

    -- ** Request lenses
    uArn,
    uKeys,

    -- * Destructuring the response
    UntagResponse (..),
    mkUntagResponse,

    -- ** Response lenses
    urrsArn,
    urrsKeys,
    urrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntag' smart constructor.
data Untag = Untag'
  { -- | The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
    arn :: Types.Arn,
    -- | The keys of the tags to be removed.
    keys :: [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Untag' value with any optional fields omitted.
mkUntag ::
  -- | 'arn'
  Types.Arn ->
  Untag
mkUntag arn = Untag' {arn, keys = Core.mempty}

-- | The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uArn :: Lens.Lens' Untag Types.Arn
uArn = Lens.field @"arn"
{-# DEPRECATED uArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The keys of the tags to be removed.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uKeys :: Lens.Lens' Untag [Types.TagKey]
uKeys = Lens.field @"keys"
{-# DEPRECATED uKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

instance Core.FromJSON Untag where
  toJSON Untag {..} =
    Core.object (Core.catMaybes [Core.Just ("Keys" Core..= keys)])

instance Core.AWSRequest Untag where
  type Rs Untag = UntagResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ("/resources/" Core.<> (Core.toText arn) Core.<> ("/tags")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UntagResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Keys")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUntagResponse' smart constructor.
data UntagResponse = UntagResponse'
  { -- | The ARN of the resource group from which tags have been removed.
    arn :: Core.Maybe Types.Arn,
    -- | The keys of the tags that were removed.
    keys :: Core.Maybe [Types.TagKey],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResponse' value with any optional fields omitted.
mkUntagResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UntagResponse
mkUntagResponse responseStatus =
  UntagResponse'
    { arn = Core.Nothing,
      keys = Core.Nothing,
      responseStatus
    }

-- | The ARN of the resource group from which tags have been removed.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsArn :: Lens.Lens' UntagResponse (Core.Maybe Types.Arn)
urrsArn = Lens.field @"arn"
{-# DEPRECATED urrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The keys of the tags that were removed.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsKeys :: Lens.Lens' UntagResponse (Core.Maybe [Types.TagKey])
urrsKeys = Lens.field @"keys"
{-# DEPRECATED urrsKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UntagResponse Core.Int
urrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
