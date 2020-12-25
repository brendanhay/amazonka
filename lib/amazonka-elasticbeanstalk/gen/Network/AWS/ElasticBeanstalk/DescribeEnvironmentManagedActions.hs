{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment's upcoming and in-progress managed actions.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
  ( -- * Creating a request
    DescribeEnvironmentManagedActions (..),
    mkDescribeEnvironmentManagedActions,

    -- ** Request lenses
    demaEnvironmentId,
    demaEnvironmentName,
    demaStatus,

    -- * Destructuring the response
    DescribeEnvironmentManagedActionsResponse (..),
    mkDescribeEnvironmentManagedActionsResponse,

    -- ** Response lenses
    demarrsManagedActions,
    demarrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list an environment's upcoming and in-progress managed actions.
--
-- /See:/ 'mkDescribeEnvironmentManagedActions' smart constructor.
data DescribeEnvironmentManagedActions = DescribeEnvironmentManagedActions'
  { -- | The environment ID of the target environment.
    environmentId :: Core.Maybe Types.String,
    -- | The name of the target environment.
    environmentName :: Core.Maybe Types.String,
    -- | To show only actions with a particular status, specify a status.
    status :: Core.Maybe Types.ActionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentManagedActions' value with any optional fields omitted.
mkDescribeEnvironmentManagedActions ::
  DescribeEnvironmentManagedActions
mkDescribeEnvironmentManagedActions =
  DescribeEnvironmentManagedActions'
    { environmentId = Core.Nothing,
      environmentName = Core.Nothing,
      status = Core.Nothing
    }

-- | The environment ID of the target environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demaEnvironmentId :: Lens.Lens' DescribeEnvironmentManagedActions (Core.Maybe Types.String)
demaEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED demaEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The name of the target environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demaEnvironmentName :: Lens.Lens' DescribeEnvironmentManagedActions (Core.Maybe Types.String)
demaEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED demaEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | To show only actions with a particular status, specify a status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demaStatus :: Lens.Lens' DescribeEnvironmentManagedActions (Core.Maybe Types.ActionStatus)
demaStatus = Lens.field @"status"
{-# DEPRECATED demaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.AWSRequest DescribeEnvironmentManagedActions where
  type
    Rs DescribeEnvironmentManagedActions =
      DescribeEnvironmentManagedActionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeEnvironmentManagedActions")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "EnvironmentId" Core.<$> environmentId)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
                Core.<> (Core.toQueryValue "Status" Core.<$> status)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentManagedActionsResult"
      ( \s h x ->
          DescribeEnvironmentManagedActionsResponse'
            Core.<$> ( x Core..@? "ManagedActions"
                         Core..<@> Core.parseXMLNonEmpty "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result message containing a list of managed actions.
--
-- /See:/ 'mkDescribeEnvironmentManagedActionsResponse' smart constructor.
data DescribeEnvironmentManagedActionsResponse = DescribeEnvironmentManagedActionsResponse'
  { -- | A list of upcoming and in-progress managed actions.
    managedActions :: Core.Maybe (Core.NonEmpty Types.ManagedAction),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEnvironmentManagedActionsResponse' value with any optional fields omitted.
mkDescribeEnvironmentManagedActionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEnvironmentManagedActionsResponse
mkDescribeEnvironmentManagedActionsResponse responseStatus =
  DescribeEnvironmentManagedActionsResponse'
    { managedActions =
        Core.Nothing,
      responseStatus
    }

-- | A list of upcoming and in-progress managed actions.
--
-- /Note:/ Consider using 'managedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demarrsManagedActions :: Lens.Lens' DescribeEnvironmentManagedActionsResponse (Core.Maybe (Core.NonEmpty Types.ManagedAction))
demarrsManagedActions = Lens.field @"managedActions"
{-# DEPRECATED demarrsManagedActions "Use generic-lens or generic-optics with 'managedActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demarrsResponseStatus :: Lens.Lens' DescribeEnvironmentManagedActionsResponse Core.Int
demarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED demarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
