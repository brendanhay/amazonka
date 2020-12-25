{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutConferencePreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the conference preferences on a specific conference provider at the account level.
module Network.AWS.AlexaBusiness.PutConferencePreference
  ( -- * Creating a request
    PutConferencePreference (..),
    mkPutConferencePreference,

    -- ** Request lenses
    pcpConferencePreference,

    -- * Destructuring the response
    PutConferencePreferenceResponse (..),
    mkPutConferencePreferenceResponse,

    -- ** Response lenses
    pcprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutConferencePreference' smart constructor.
newtype PutConferencePreference = PutConferencePreference'
  { -- | The conference preference of a specific conference provider.
    conferencePreference :: Types.ConferencePreference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutConferencePreference' value with any optional fields omitted.
mkPutConferencePreference ::
  -- | 'conferencePreference'
  Types.ConferencePreference ->
  PutConferencePreference
mkPutConferencePreference conferencePreference =
  PutConferencePreference' {conferencePreference}

-- | The conference preference of a specific conference provider.
--
-- /Note:/ Consider using 'conferencePreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConferencePreference :: Lens.Lens' PutConferencePreference Types.ConferencePreference
pcpConferencePreference = Lens.field @"conferencePreference"
{-# DEPRECATED pcpConferencePreference "Use generic-lens or generic-optics with 'conferencePreference' instead." #-}

instance Core.FromJSON PutConferencePreference where
  toJSON PutConferencePreference {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ConferencePreference" Core..= conferencePreference)]
      )

instance Core.AWSRequest PutConferencePreference where
  type Rs PutConferencePreference = PutConferencePreferenceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.PutConferencePreference")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConferencePreferenceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutConferencePreferenceResponse' smart constructor.
newtype PutConferencePreferenceResponse = PutConferencePreferenceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutConferencePreferenceResponse' value with any optional fields omitted.
mkPutConferencePreferenceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutConferencePreferenceResponse
mkPutConferencePreferenceResponse responseStatus =
  PutConferencePreferenceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprrsResponseStatus :: Lens.Lens' PutConferencePreferenceResponse Core.Int
pcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
