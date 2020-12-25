{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing conference provider's settings.
module Network.AWS.AlexaBusiness.UpdateConferenceProvider
  ( -- * Creating a request
    UpdateConferenceProvider (..),
    mkUpdateConferenceProvider,

    -- ** Request lenses
    ucpConferenceProviderArn,
    ucpConferenceProviderType,
    ucpMeetingSetting,
    ucpIPDialIn,
    ucpPSTNDialIn,

    -- * Destructuring the response
    UpdateConferenceProviderResponse (..),
    mkUpdateConferenceProviderResponse,

    -- ** Response lenses
    ucprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateConferenceProvider' smart constructor.
data UpdateConferenceProvider = UpdateConferenceProvider'
  { -- | The ARN of the conference provider.
    conferenceProviderArn :: Types.ConferenceProviderArn,
    -- | The type of the conference provider.
    conferenceProviderType :: Types.ConferenceProviderType,
    -- | The meeting settings for the conference provider.
    meetingSetting :: Types.MeetingSetting,
    -- | The IP endpoint and protocol for calling.
    iPDialIn :: Core.Maybe Types.IPDialIn,
    -- | The information for PSTN conferencing.
    pSTNDialIn :: Core.Maybe Types.PSTNDialIn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConferenceProvider' value with any optional fields omitted.
mkUpdateConferenceProvider ::
  -- | 'conferenceProviderArn'
  Types.ConferenceProviderArn ->
  -- | 'conferenceProviderType'
  Types.ConferenceProviderType ->
  -- | 'meetingSetting'
  Types.MeetingSetting ->
  UpdateConferenceProvider
mkUpdateConferenceProvider
  conferenceProviderArn
  conferenceProviderType
  meetingSetting =
    UpdateConferenceProvider'
      { conferenceProviderArn,
        conferenceProviderType,
        meetingSetting,
        iPDialIn = Core.Nothing,
        pSTNDialIn = Core.Nothing
      }

-- | The ARN of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpConferenceProviderArn :: Lens.Lens' UpdateConferenceProvider Types.ConferenceProviderArn
ucpConferenceProviderArn = Lens.field @"conferenceProviderArn"
{-# DEPRECATED ucpConferenceProviderArn "Use generic-lens or generic-optics with 'conferenceProviderArn' instead." #-}

-- | The type of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpConferenceProviderType :: Lens.Lens' UpdateConferenceProvider Types.ConferenceProviderType
ucpConferenceProviderType = Lens.field @"conferenceProviderType"
{-# DEPRECATED ucpConferenceProviderType "Use generic-lens or generic-optics with 'conferenceProviderType' instead." #-}

-- | The meeting settings for the conference provider.
--
-- /Note:/ Consider using 'meetingSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpMeetingSetting :: Lens.Lens' UpdateConferenceProvider Types.MeetingSetting
ucpMeetingSetting = Lens.field @"meetingSetting"
{-# DEPRECATED ucpMeetingSetting "Use generic-lens or generic-optics with 'meetingSetting' instead." #-}

-- | The IP endpoint and protocol for calling.
--
-- /Note:/ Consider using 'iPDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpIPDialIn :: Lens.Lens' UpdateConferenceProvider (Core.Maybe Types.IPDialIn)
ucpIPDialIn = Lens.field @"iPDialIn"
{-# DEPRECATED ucpIPDialIn "Use generic-lens or generic-optics with 'iPDialIn' instead." #-}

-- | The information for PSTN conferencing.
--
-- /Note:/ Consider using 'pSTNDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpPSTNDialIn :: Lens.Lens' UpdateConferenceProvider (Core.Maybe Types.PSTNDialIn)
ucpPSTNDialIn = Lens.field @"pSTNDialIn"
{-# DEPRECATED ucpPSTNDialIn "Use generic-lens or generic-optics with 'pSTNDialIn' instead." #-}

instance Core.FromJSON UpdateConferenceProvider where
  toJSON UpdateConferenceProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConferenceProviderArn" Core..= conferenceProviderArn),
            Core.Just
              ("ConferenceProviderType" Core..= conferenceProviderType),
            Core.Just ("MeetingSetting" Core..= meetingSetting),
            ("IPDialIn" Core..=) Core.<$> iPDialIn,
            ("PSTNDialIn" Core..=) Core.<$> pSTNDialIn
          ]
      )

instance Core.AWSRequest UpdateConferenceProvider where
  type Rs UpdateConferenceProvider = UpdateConferenceProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.UpdateConferenceProvider")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConferenceProviderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateConferenceProviderResponse' smart constructor.
newtype UpdateConferenceProviderResponse = UpdateConferenceProviderResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConferenceProviderResponse' value with any optional fields omitted.
mkUpdateConferenceProviderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateConferenceProviderResponse
mkUpdateConferenceProviderResponse responseStatus =
  UpdateConferenceProviderResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsResponseStatus :: Lens.Lens' UpdateConferenceProviderResponse Core.Int
ucprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
