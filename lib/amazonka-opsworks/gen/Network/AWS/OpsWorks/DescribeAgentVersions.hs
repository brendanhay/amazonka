{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeAgentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available AWS OpsWorks Stacks agent versions. You must specify a stack ID or a configuration manager. @DescribeAgentVersions@ returns a list of available agent versions for the specified stack or configuration manager.
module Network.AWS.OpsWorks.DescribeAgentVersions
  ( -- * Creating a request
    DescribeAgentVersions (..),
    mkDescribeAgentVersions,

    -- ** Request lenses
    davConfigurationManager,
    davStackId,

    -- * Destructuring the response
    DescribeAgentVersionsResponse (..),
    mkDescribeAgentVersionsResponse,

    -- ** Response lenses
    davrrsAgentVersions,
    davrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAgentVersions' smart constructor.
data DescribeAgentVersions = DescribeAgentVersions'
  { -- | The configuration manager.
    configurationManager :: Core.Maybe Types.StackConfigurationManager,
    -- | The stack ID.
    stackId :: Core.Maybe Types.StackId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAgentVersions' value with any optional fields omitted.
mkDescribeAgentVersions ::
  DescribeAgentVersions
mkDescribeAgentVersions =
  DescribeAgentVersions'
    { configurationManager = Core.Nothing,
      stackId = Core.Nothing
    }

-- | The configuration manager.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davConfigurationManager :: Lens.Lens' DescribeAgentVersions (Core.Maybe Types.StackConfigurationManager)
davConfigurationManager = Lens.field @"configurationManager"
{-# DEPRECATED davConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davStackId :: Lens.Lens' DescribeAgentVersions (Core.Maybe Types.StackId)
davStackId = Lens.field @"stackId"
{-# DEPRECATED davStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribeAgentVersions where
  toJSON DescribeAgentVersions {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConfigurationManager" Core..=) Core.<$> configurationManager,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.AWSRequest DescribeAgentVersions where
  type Rs DescribeAgentVersions = DescribeAgentVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.DescribeAgentVersions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAgentVersionsResponse'
            Core.<$> (x Core..:? "AgentVersions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeAgentVersions@ request.
--
-- /See:/ 'mkDescribeAgentVersionsResponse' smart constructor.
data DescribeAgentVersionsResponse = DescribeAgentVersionsResponse'
  { -- | The agent versions for the specified stack or configuration manager. Note that this value is the complete version number, not the abbreviated number used by the console.
    agentVersions :: Core.Maybe [Types.AgentVersion],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAgentVersionsResponse' value with any optional fields omitted.
mkDescribeAgentVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAgentVersionsResponse
mkDescribeAgentVersionsResponse responseStatus =
  DescribeAgentVersionsResponse'
    { agentVersions = Core.Nothing,
      responseStatus
    }

-- | The agent versions for the specified stack or configuration manager. Note that this value is the complete version number, not the abbreviated number used by the console.
--
-- /Note:/ Consider using 'agentVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrrsAgentVersions :: Lens.Lens' DescribeAgentVersionsResponse (Core.Maybe [Types.AgentVersion])
davrrsAgentVersions = Lens.field @"agentVersions"
{-# DEPRECATED davrrsAgentVersions "Use generic-lens or generic-optics with 'agentVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrrsResponseStatus :: Lens.Lens' DescribeAgentVersionsResponse Core.Int
davrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED davrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
