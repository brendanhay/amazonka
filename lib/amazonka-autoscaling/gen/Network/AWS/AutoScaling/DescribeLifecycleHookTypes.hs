{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
-- The following hook types are supported:
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
  ( -- * Creating a request
    DescribeLifecycleHookTypes (..),
    mkDescribeLifecycleHookTypes,

    -- * Destructuring the response
    DescribeLifecycleHookTypesResponse (..),
    mkDescribeLifecycleHookTypesResponse,

    -- ** Response lenses
    dlhtrrsLifecycleHookTypes,
    dlhtrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLifecycleHookTypes' value with any optional fields omitted.
mkDescribeLifecycleHookTypes ::
  DescribeLifecycleHookTypes
mkDescribeLifecycleHookTypes = DescribeLifecycleHookTypes'

instance Core.AWSRequest DescribeLifecycleHookTypes where
  type
    Rs DescribeLifecycleHookTypes =
      DescribeLifecycleHookTypesResponse
  request x@_ =
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
            ( Core.pure ("Action", "DescribeLifecycleHookTypes")
                Core.<> (Core.pure ("Version", "2011-01-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeLifecycleHookTypesResult"
      ( \s h x ->
          DescribeLifecycleHookTypesResponse'
            Core.<$> ( x Core..@? "LifecycleHookTypes"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeLifecycleHookTypesResponse' smart constructor.
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
  { -- | The lifecycle hook types.
    lifecycleHookTypes :: Core.Maybe [Types.XmlStringMaxLen255],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLifecycleHookTypesResponse' value with any optional fields omitted.
mkDescribeLifecycleHookTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLifecycleHookTypesResponse
mkDescribeLifecycleHookTypesResponse responseStatus =
  DescribeLifecycleHookTypesResponse'
    { lifecycleHookTypes =
        Core.Nothing,
      responseStatus
    }

-- | The lifecycle hook types.
--
-- /Note:/ Consider using 'lifecycleHookTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhtrrsLifecycleHookTypes :: Lens.Lens' DescribeLifecycleHookTypesResponse (Core.Maybe [Types.XmlStringMaxLen255])
dlhtrrsLifecycleHookTypes = Lens.field @"lifecycleHookTypes"
{-# DEPRECATED dlhtrrsLifecycleHookTypes "Use generic-lens or generic-optics with 'lifecycleHookTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhtrrsResponseStatus :: Lens.Lens' DescribeLifecycleHookTypesResponse Core.Int
dlhtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlhtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
