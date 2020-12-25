{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.EnableProactiveEngagement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT) to use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.EnableProactiveEngagement
  ( -- * Creating a request
    EnableProactiveEngagement (..),
    mkEnableProactiveEngagement,

    -- * Destructuring the response
    EnableProactiveEngagementResponse (..),
    mkEnableProactiveEngagementResponse,

    -- ** Response lenses
    eperrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkEnableProactiveEngagement' smart constructor.
data EnableProactiveEngagement = EnableProactiveEngagement'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableProactiveEngagement' value with any optional fields omitted.
mkEnableProactiveEngagement ::
  EnableProactiveEngagement
mkEnableProactiveEngagement = EnableProactiveEngagement'

instance Core.FromJSON EnableProactiveEngagement where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest EnableProactiveEngagement where
  type
    Rs EnableProactiveEngagement =
      EnableProactiveEngagementResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.EnableProactiveEngagement")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableProactiveEngagementResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableProactiveEngagementResponse' smart constructor.
newtype EnableProactiveEngagementResponse = EnableProactiveEngagementResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableProactiveEngagementResponse' value with any optional fields omitted.
mkEnableProactiveEngagementResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableProactiveEngagementResponse
mkEnableProactiveEngagementResponse responseStatus =
  EnableProactiveEngagementResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eperrsResponseStatus :: Lens.Lens' EnableProactiveEngagementResponse Core.Int
eperrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eperrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
