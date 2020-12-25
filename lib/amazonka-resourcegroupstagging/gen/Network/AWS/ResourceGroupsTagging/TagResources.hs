{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.TagResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies one or more tags to the specified resources. Note the following:
--
--
--     * Not all resources can have tags. For a list of services that support tagging, see <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/Welcome.html this list> .
--
--
--     * Each resource can have up to 50 tags. For other limits, see <http://docs.aws.amazon.com/general/latest/gr/aws_tagging.html#tag-conventions Tag Naming and Usage Conventions> in the /AWS General Reference./
--
--
--     * You can only tag resources that are located in the specified Region for the AWS account.
--
--
--     * To add tags to a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for adding tags. For more information, see <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/Welcome.html this list> .
--
--
-- /Important:/ Do not store personally identifiable information (PII) or other confidential or sensitive information in tags. We use tags to provide you with billing and administration services. Tags are not intended to be used for private or sensitive data.
module Network.AWS.ResourceGroupsTagging.TagResources
  ( -- * Creating a request
    TagResources (..),
    mkTagResources,

    -- ** Request lenses
    trResourceARNList,
    trTags,

    -- * Destructuring the response
    TagResourcesResponse (..),
    mkTagResourcesResponse,

    -- ** Response lenses
    trrrsFailedResourcesMap,
    trrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroupsTagging.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagResources' smart constructor.
data TagResources = TagResources'
  { -- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    resourceARNList :: Core.NonEmpty Types.ResourceARN,
    -- | The tags that you want to add to the specified resources. A tag consists of a key and a value that you define.
    tags :: Core.HashMap Types.TagKey Types.TagValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResources' value with any optional fields omitted.
mkTagResources ::
  -- | 'resourceARNList'
  Core.NonEmpty Types.ResourceARN ->
  TagResources
mkTagResources resourceARNList =
  TagResources' {resourceARNList, tags = Core.mempty}

-- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'resourceARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceARNList :: Lens.Lens' TagResources (Core.NonEmpty Types.ResourceARN)
trResourceARNList = Lens.field @"resourceARNList"
{-# DEPRECATED trResourceARNList "Use generic-lens or generic-optics with 'resourceARNList' instead." #-}

-- | The tags that you want to add to the specified resources. A tag consists of a key and a value that you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResources (Core.HashMap Types.TagKey Types.TagValue)
trTags = Lens.field @"tags"
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON TagResources where
  toJSON TagResources {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARNList" Core..= resourceARNList),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest TagResources where
  type Rs TagResources = TagResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ResourceGroupsTaggingAPI_20170126.TagResources")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TagResourcesResponse'
            Core.<$> (x Core..:? "FailedResourcesMap")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTagResourcesResponse' smart constructor.
data TagResourcesResponse = TagResourcesResponse'
  { -- | A map containing a key-value pair for each failed item that couldn't be tagged. The key is the ARN of the failed resource. The value is a @FailureInfo@ object that contains an error code, a status code, and an error message. If there are no errors, the @FailedResourcesMap@ is empty.
    failedResourcesMap :: Core.Maybe (Core.HashMap Types.ResourceARN Types.FailureInfo),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResourcesResponse' value with any optional fields omitted.
mkTagResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TagResourcesResponse
mkTagResourcesResponse responseStatus =
  TagResourcesResponse'
    { failedResourcesMap = Core.Nothing,
      responseStatus
    }

-- | A map containing a key-value pair for each failed item that couldn't be tagged. The key is the ARN of the failed resource. The value is a @FailureInfo@ object that contains an error code, a status code, and an error message. If there are no errors, the @FailedResourcesMap@ is empty.
--
-- /Note:/ Consider using 'failedResourcesMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrrsFailedResourcesMap :: Lens.Lens' TagResourcesResponse (Core.Maybe (Core.HashMap Types.ResourceARN Types.FailureInfo))
trrrsFailedResourcesMap = Lens.field @"failedResourcesMap"
{-# DEPRECATED trrrsFailedResourcesMap "Use generic-lens or generic-optics with 'failedResourcesMap' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrrsResponseStatus :: Lens.Lens' TagResourcesResponse Core.Int
trrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED trrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
