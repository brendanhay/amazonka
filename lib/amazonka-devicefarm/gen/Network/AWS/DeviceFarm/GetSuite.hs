{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetSuite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a suite.
module Network.AWS.DeviceFarm.GetSuite
  ( -- * Creating a request
    GetSuite (..),
    mkGetSuite,

    -- ** Request lenses
    gsArn,

    -- * Destructuring the response
    GetSuiteResponse (..),
    mkGetSuiteResponse,

    -- ** Response lenses
    gsrrsSuite,
    gsrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get suite operation.
--
-- /See:/ 'mkGetSuite' smart constructor.
newtype GetSuite = GetSuite'
  { -- | The suite's ARN.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSuite' value with any optional fields omitted.
mkGetSuite ::
  -- | 'arn'
  Types.Arn ->
  GetSuite
mkGetSuite arn = GetSuite' {arn}

-- | The suite's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsArn :: Lens.Lens' GetSuite Types.Arn
gsArn = Lens.field @"arn"
{-# DEPRECATED gsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetSuite where
  toJSON GetSuite {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetSuite where
  type Rs GetSuite = GetSuiteResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetSuite")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSuiteResponse'
            Core.<$> (x Core..:? "suite") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a get suite request.
--
-- /See:/ 'mkGetSuiteResponse' smart constructor.
data GetSuiteResponse = GetSuiteResponse'
  { -- | A collection of one or more tests.
    suite :: Core.Maybe Types.Suite,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSuiteResponse' value with any optional fields omitted.
mkGetSuiteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSuiteResponse
mkGetSuiteResponse responseStatus =
  GetSuiteResponse' {suite = Core.Nothing, responseStatus}

-- | A collection of one or more tests.
--
-- /Note:/ Consider using 'suite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSuite :: Lens.Lens' GetSuiteResponse (Core.Maybe Types.Suite)
gsrrsSuite = Lens.field @"suite"
{-# DEPRECATED gsrrsSuite "Use generic-lens or generic-optics with 'suite' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetSuiteResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
