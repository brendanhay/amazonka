{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.UntagResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified resources. When you specify a tag key, the action removes both that key and its associated value. The operation succeeds even if you attempt to remove tags from a resource that were already removed. Note the following:
--
--
--     * To remove tags from a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for removing tags. For more information, see <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/Welcome.html this list> .
--
--
--     * You can only tag resources that are located in the specified Region for the AWS account.
module Network.AWS.ResourceGroupsTagging.UntagResources
  ( -- * Creating a request
    UntagResources (..),
    mkUntagResources,

    -- ** Request lenses
    urResourceARNList,
    urTagKeys,

    -- * Destructuring the response
    UntagResourcesResponse (..),
    mkUntagResourcesResponse,

    -- ** Response lenses
    urrrsFailedResourcesMap,
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroupsTagging.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagResources' smart constructor.
data UntagResources = UntagResources'
  { -- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    resourceARNList :: Core.NonEmpty Types.ResourceARN,
    -- | A list of the tag keys that you want to remove from the specified resources.
    tagKeys :: Core.NonEmpty Types.TagKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResources' value with any optional fields omitted.
mkUntagResources ::
  -- | 'resourceARNList'
  Core.NonEmpty Types.ResourceARN ->
  -- | 'tagKeys'
  Core.NonEmpty Types.TagKey ->
  UntagResources
mkUntagResources resourceARNList tagKeys =
  UntagResources' {resourceARNList, tagKeys}

-- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'resourceARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARNList :: Lens.Lens' UntagResources (Core.NonEmpty Types.ResourceARN)
urResourceARNList = Lens.field @"resourceARNList"
{-# DEPRECATED urResourceARNList "Use generic-lens or generic-optics with 'resourceARNList' instead." #-}

-- | A list of the tag keys that you want to remove from the specified resources.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResources (Core.NonEmpty Types.TagKey)
urTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON UntagResources where
  toJSON UntagResources {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARNList" Core..= resourceARNList),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.AWSRequest UntagResources where
  type Rs UntagResources = UntagResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "ResourceGroupsTaggingAPI_20170126.UntagResources"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UntagResourcesResponse'
            Core.<$> (x Core..:? "FailedResourcesMap")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUntagResourcesResponse' smart constructor.
data UntagResourcesResponse = UntagResourcesResponse'
  { -- | Details of resources that could not be untagged. An error code, status code, and error message are returned for each failed item.
    failedResourcesMap :: Core.Maybe (Core.HashMap Types.ResourceARN Types.FailureInfo),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResourcesResponse' value with any optional fields omitted.
mkUntagResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UntagResourcesResponse
mkUntagResourcesResponse responseStatus =
  UntagResourcesResponse'
    { failedResourcesMap = Core.Nothing,
      responseStatus
    }

-- | Details of resources that could not be untagged. An error code, status code, and error message are returned for each failed item.
--
-- /Note:/ Consider using 'failedResourcesMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsFailedResourcesMap :: Lens.Lens' UntagResourcesResponse (Core.Maybe (Core.HashMap Types.ResourceARN Types.FailureInfo))
urrrsFailedResourcesMap = Lens.field @"failedResourcesMap"
{-# DEPRECATED urrrsFailedResourcesMap "Use generic-lens or generic-optics with 'failedResourcesMap' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UntagResourcesResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
