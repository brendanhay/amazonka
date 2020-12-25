{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeEventTopics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about which SNS topics receive status messages from the specified directory.
--
-- If no input parameters are provided, such as DirectoryId or TopicName, this request describes all of the associations in the account.
module Network.AWS.DirectoryService.DescribeEventTopics
  ( -- * Creating a request
    DescribeEventTopics (..),
    mkDescribeEventTopics,

    -- ** Request lenses
    dDirectoryId,
    dTopicNames,

    -- * Destructuring the response
    DescribeEventTopicsResponse (..),
    mkDescribeEventTopicsResponse,

    -- ** Response lenses
    detrfrsEventTopics,
    detrfrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes event topics.
--
-- /See:/ 'mkDescribeEventTopics' smart constructor.
data DescribeEventTopics = DescribeEventTopics'
  { -- | The Directory ID for which to get the list of associated SNS topics. If this member is null, associations for all Directory IDs are returned.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | A list of SNS topic names for which to obtain the information. If this member is null, all associations for the specified Directory ID are returned.
    --
    -- An empty list results in an @InvalidParameterException@ being thrown.
    topicNames :: Core.Maybe [Types.TopicName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventTopics' value with any optional fields omitted.
mkDescribeEventTopics ::
  DescribeEventTopics
mkDescribeEventTopics =
  DescribeEventTopics'
    { directoryId = Core.Nothing,
      topicNames = Core.Nothing
    }

-- | The Directory ID for which to get the list of associated SNS topics. If this member is null, associations for all Directory IDs are returned.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDirectoryId :: Lens.Lens' DescribeEventTopics (Core.Maybe Types.DirectoryId)
dDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | A list of SNS topic names for which to obtain the information. If this member is null, all associations for the specified Directory ID are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
--
-- /Note:/ Consider using 'topicNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTopicNames :: Lens.Lens' DescribeEventTopics (Core.Maybe [Types.TopicName])
dTopicNames = Lens.field @"topicNames"
{-# DEPRECATED dTopicNames "Use generic-lens or generic-optics with 'topicNames' instead." #-}

instance Core.FromJSON DescribeEventTopics where
  toJSON DescribeEventTopics {..} =
    Core.object
      ( Core.catMaybes
          [ ("DirectoryId" Core..=) Core.<$> directoryId,
            ("TopicNames" Core..=) Core.<$> topicNames
          ]
      )

instance Core.AWSRequest DescribeEventTopics where
  type Rs DescribeEventTopics = DescribeEventTopicsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.DescribeEventTopics")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventTopicsResponse'
            Core.<$> (x Core..:? "EventTopics") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a DescribeEventTopic request.
--
-- /See:/ 'mkDescribeEventTopicsResponse' smart constructor.
data DescribeEventTopicsResponse = DescribeEventTopicsResponse'
  { -- | A list of SNS topic names that receive status messages from the specified Directory ID.
    eventTopics :: Core.Maybe [Types.EventTopic],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEventTopicsResponse' value with any optional fields omitted.
mkDescribeEventTopicsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEventTopicsResponse
mkDescribeEventTopicsResponse responseStatus =
  DescribeEventTopicsResponse'
    { eventTopics = Core.Nothing,
      responseStatus
    }

-- | A list of SNS topic names that receive status messages from the specified Directory ID.
--
-- /Note:/ Consider using 'eventTopics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrfrsEventTopics :: Lens.Lens' DescribeEventTopicsResponse (Core.Maybe [Types.EventTopic])
detrfrsEventTopics = Lens.field @"eventTopics"
{-# DEPRECATED detrfrsEventTopics "Use generic-lens or generic-optics with 'eventTopics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrfrsResponseStatus :: Lens.Lens' DescribeEventTopicsResponse Core.Int
detrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED detrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
