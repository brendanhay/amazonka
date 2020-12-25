{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conference provider.
module Network.AWS.AlexaBusiness.DeleteConferenceProvider
  ( -- * Creating a request
    DeleteConferenceProvider (..),
    mkDeleteConferenceProvider,

    -- ** Request lenses
    dcpConferenceProviderArn,

    -- * Destructuring the response
    DeleteConferenceProviderResponse (..),
    mkDeleteConferenceProviderResponse,

    -- ** Response lenses
    dcprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteConferenceProvider' smart constructor.
newtype DeleteConferenceProvider = DeleteConferenceProvider'
  { -- | The ARN of the conference provider.
    conferenceProviderArn :: Types.ConferenceProviderArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConferenceProvider' value with any optional fields omitted.
mkDeleteConferenceProvider ::
  -- | 'conferenceProviderArn'
  Types.ConferenceProviderArn ->
  DeleteConferenceProvider
mkDeleteConferenceProvider conferenceProviderArn =
  DeleteConferenceProvider' {conferenceProviderArn}

-- | The ARN of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpConferenceProviderArn :: Lens.Lens' DeleteConferenceProvider Types.ConferenceProviderArn
dcpConferenceProviderArn = Lens.field @"conferenceProviderArn"
{-# DEPRECATED dcpConferenceProviderArn "Use generic-lens or generic-optics with 'conferenceProviderArn' instead." #-}

instance Core.FromJSON DeleteConferenceProvider where
  toJSON DeleteConferenceProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ConferenceProviderArn" Core..= conferenceProviderArn)
          ]
      )

instance Core.AWSRequest DeleteConferenceProvider where
  type Rs DeleteConferenceProvider = DeleteConferenceProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.DeleteConferenceProvider")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConferenceProviderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteConferenceProviderResponse' smart constructor.
newtype DeleteConferenceProviderResponse = DeleteConferenceProviderResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConferenceProviderResponse' value with any optional fields omitted.
mkDeleteConferenceProviderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteConferenceProviderResponse
mkDeleteConferenceProviderResponse responseStatus =
  DeleteConferenceProviderResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DeleteConferenceProviderResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
