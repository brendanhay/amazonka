{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific work team. You can see information such as the create date, the last updated date, membership information, and the work team's Amazon Resource Name (ARN).
module Network.AWS.SageMaker.DescribeWorkteam
  ( -- * Creating a request
    DescribeWorkteam (..),
    mkDescribeWorkteam,

    -- ** Request lenses
    dWorkteamName,

    -- * Destructuring the response
    DescribeWorkteamResponse (..),
    mkDescribeWorkteamResponse,

    -- ** Response lenses
    dwrgrsWorkteam,
    dwrgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeWorkteam' smart constructor.
newtype DescribeWorkteam = DescribeWorkteam'
  { -- | The name of the work team to return a description of.
    workteamName :: Types.WorkteamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkteam' value with any optional fields omitted.
mkDescribeWorkteam ::
  -- | 'workteamName'
  Types.WorkteamName ->
  DescribeWorkteam
mkDescribeWorkteam workteamName = DescribeWorkteam' {workteamName}

-- | The name of the work team to return a description of.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWorkteamName :: Lens.Lens' DescribeWorkteam Types.WorkteamName
dWorkteamName = Lens.field @"workteamName"
{-# DEPRECATED dWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

instance Core.FromJSON DescribeWorkteam where
  toJSON DescribeWorkteam {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WorkteamName" Core..= workteamName)])

instance Core.AWSRequest DescribeWorkteam where
  type Rs DescribeWorkteam = DescribeWorkteamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeWorkteam")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkteamResponse'
            Core.<$> (x Core..: "Workteam") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeWorkteamResponse' smart constructor.
data DescribeWorkteamResponse = DescribeWorkteamResponse'
  { -- | A @Workteam@ instance that contains information about the work team.
    workteam :: Types.Workteam,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeWorkteamResponse' value with any optional fields omitted.
mkDescribeWorkteamResponse ::
  -- | 'workteam'
  Types.Workteam ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeWorkteamResponse
mkDescribeWorkteamResponse workteam responseStatus =
  DescribeWorkteamResponse' {workteam, responseStatus}

-- | A @Workteam@ instance that contains information about the work team.
--
-- /Note:/ Consider using 'workteam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrgrsWorkteam :: Lens.Lens' DescribeWorkteamResponse Types.Workteam
dwrgrsWorkteam = Lens.field @"workteam"
{-# DEPRECATED dwrgrsWorkteam "Use generic-lens or generic-optics with 'workteam' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrgrsResponseStatus :: Lens.Lens' DescribeWorkteamResponse Core.Int
dwrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
