{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a specific conference provider.
module Network.AWS.AlexaBusiness.GetConferenceProvider
  ( -- * Creating a request
    GetConferenceProvider (..),
    mkGetConferenceProvider,

    -- ** Request lenses
    gcpConferenceProviderArn,

    -- * Destructuring the response
    GetConferenceProviderResponse (..),
    mkGetConferenceProviderResponse,

    -- ** Response lenses
    grsConferenceProvider,
    grsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConferenceProvider' smart constructor.
newtype GetConferenceProvider = GetConferenceProvider'
  { -- | The ARN of the newly created conference provider.
    conferenceProviderArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetConferenceProvider' value with any optional fields omitted.
mkGetConferenceProvider ::
  -- | 'conferenceProviderArn'
  Types.Arn ->
  GetConferenceProvider
mkGetConferenceProvider conferenceProviderArn =
  GetConferenceProvider' {conferenceProviderArn}

-- | The ARN of the newly created conference provider.
--
-- /Note:/ Consider using 'conferenceProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpConferenceProviderArn :: Lens.Lens' GetConferenceProvider Types.Arn
gcpConferenceProviderArn = Lens.field @"conferenceProviderArn"
{-# DEPRECATED gcpConferenceProviderArn "Use generic-lens or generic-optics with 'conferenceProviderArn' instead." #-}

instance Core.FromJSON GetConferenceProvider where
  toJSON GetConferenceProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ConferenceProviderArn" Core..= conferenceProviderArn)
          ]
      )

instance Core.AWSRequest GetConferenceProvider where
  type Rs GetConferenceProvider = GetConferenceProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.GetConferenceProvider")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConferenceProviderResponse'
            Core.<$> (x Core..:? "ConferenceProvider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetConferenceProviderResponse' smart constructor.
data GetConferenceProviderResponse = GetConferenceProviderResponse'
  { -- | The conference provider.
    conferenceProvider :: Core.Maybe Types.ConferenceProvider,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConferenceProviderResponse' value with any optional fields omitted.
mkGetConferenceProviderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConferenceProviderResponse
mkGetConferenceProviderResponse responseStatus =
  GetConferenceProviderResponse'
    { conferenceProvider = Core.Nothing,
      responseStatus
    }

-- | The conference provider.
--
-- /Note:/ Consider using 'conferenceProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsConferenceProvider :: Lens.Lens' GetConferenceProviderResponse (Core.Maybe Types.ConferenceProvider)
grsConferenceProvider = Lens.field @"conferenceProvider"
{-# DEPRECATED grsConferenceProvider "Use generic-lens or generic-optics with 'conferenceProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetConferenceProviderResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
