{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateOrUpdateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates tags for the specified Auto Scaling group.
--
-- When you specify a tag with a key that already exists, the operation overwrites the previous tag definition, and you do not get an error message.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.CreateOrUpdateTags
    (
    -- * Creating a request
      CreateOrUpdateTags (..)
    , mkCreateOrUpdateTags
    -- ** Request lenses
    , coutTags

    -- * Destructuring the response
    , CreateOrUpdateTagsResponse (..)
    , mkCreateOrUpdateTagsResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateOrUpdateTags' smart constructor.
newtype CreateOrUpdateTags = CreateOrUpdateTags'
  { tags :: [Types.Tag]
    -- ^ One or more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOrUpdateTags' value with any optional fields omitted.
mkCreateOrUpdateTags
    :: CreateOrUpdateTags
mkCreateOrUpdateTags = CreateOrUpdateTags'{tags = Core.mempty}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coutTags :: Lens.Lens' CreateOrUpdateTags [Types.Tag]
coutTags = Lens.field @"tags"
{-# INLINEABLE coutTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateOrUpdateTags where
        toQuery CreateOrUpdateTags{..}
          = Core.toQueryPair "Action" ("CreateOrUpdateTags" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "Tags" (Core.toQueryList "member" tags)

instance Core.ToHeaders CreateOrUpdateTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateOrUpdateTags where
        type Rs CreateOrUpdateTags = CreateOrUpdateTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull CreateOrUpdateTagsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateOrUpdateTagsResponse' smart constructor.
data CreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOrUpdateTagsResponse' value with any optional fields omitted.
mkCreateOrUpdateTagsResponse
    :: CreateOrUpdateTagsResponse
mkCreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse'
