{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites only the specified tags for the specified Amazon EC2 resource or resources. When you specify an existing tag key, the value is overwritten with the new value. Each resource can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique per resource.
--
-- For more information about tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /Amazon Elastic Compute Cloud User Guide/ . For more information about creating IAM policies that control users' access to resources based on tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-iam-actions-resources.html Supported Resource-Level Permissions for Amazon EC2 API Actions> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateTags
    (
    -- * Creating a request
      CreateTags (..)
    , mkCreateTags
    -- ** Request lenses
    , ctsResources
    , ctsTags
    , ctsDryRun

    -- * Destructuring the response
    , CreateTagsResponse (..)
    , mkCreateTagsResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { resources :: [Types.TaggableResourceId]
    -- ^ The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
  , tags :: [Types.Tag]
    -- ^ The tags. The @value@ parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTags' value with any optional fields omitted.
mkCreateTags
    :: CreateTags
mkCreateTags
  = CreateTags'{resources = Core.mempty, tags = Core.mempty,
                dryRun = Core.Nothing}

-- | The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsResources :: Lens.Lens' CreateTags [Types.TaggableResourceId]
ctsResources = Lens.field @"resources"
{-# INLINEABLE ctsResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | The tags. The @value@ parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsTags :: Lens.Lens' CreateTags [Types.Tag]
ctsTags = Lens.field @"tags"
{-# INLINEABLE ctsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsDryRun :: Lens.Lens' CreateTags (Core.Maybe Core.Bool)
ctsDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery CreateTags where
        toQuery CreateTags{..}
          = Core.toQueryPair "Action" ("CreateTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "ResourceId" resources
              Core.<> Core.toQueryList "Tag" tags
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders CreateTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
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
        parseResponse = Response.receiveNull CreateTagsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagsResponse' value with any optional fields omitted.
mkCreateTagsResponse
    :: CreateTagsResponse
mkCreateTagsResponse = CreateTagsResponse'
