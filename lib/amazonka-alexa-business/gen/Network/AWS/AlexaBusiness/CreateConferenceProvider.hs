{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new conference provider under the user's AWS account.
module Network.AWS.AlexaBusiness.CreateConferenceProvider
  ( -- * Creating a request
    CreateConferenceProvider (..),
    mkCreateConferenceProvider,

    -- ** Request lenses
    ccpConferenceProviderName,
    ccpConferenceProviderType,
    ccpMeetingSetting,
    ccpClientRequestToken,
    ccpIPDialIn,
    ccpPSTNDialIn,

    -- * Destructuring the response
    CreateConferenceProviderResponse (..),
    mkCreateConferenceProviderResponse,

    -- ** Response lenses
    ccprrsConferenceProviderArn,
    ccprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateConferenceProvider' smart constructor.
data CreateConferenceProvider = CreateConferenceProvider'
  { -- | The name of the conference provider.
    conferenceProviderName :: Types.ConferenceProviderName,
    -- | Represents a type within a list of predefined types.
    conferenceProviderType :: Types.ConferenceProviderType,
    -- | The meeting settings for the conference provider.
    meetingSetting :: Types.MeetingSetting,
    -- | The request token of the client.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The IP endpoint and protocol for calling.
    iPDialIn :: Core.Maybe Types.IPDialIn,
    -- | The information for PSTN conferencing.
    pSTNDialIn :: Core.Maybe Types.PSTNDialIn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConferenceProvider' value with any optional fields omitted.
mkCreateConferenceProvider ::
  -- | 'conferenceProviderName'
  Types.ConferenceProviderName ->
  -- | 'conferenceProviderType'
  Types.ConferenceProviderType ->
  -- | 'meetingSetting'
  Types.MeetingSetting ->
  CreateConferenceProvider
mkCreateConferenceProvider
  conferenceProviderName
  conferenceProviderType
  meetingSetting =
    CreateConferenceProvider'
      { conferenceProviderName,
        conferenceProviderType,
        meetingSetting,
        clientRequestToken = Core.Nothing,
        iPDialIn = Core.Nothing,
        pSTNDialIn = Core.Nothing
      }

-- | The name of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpConferenceProviderName :: Lens.Lens' CreateConferenceProvider Types.ConferenceProviderName
ccpConferenceProviderName = Lens.field @"conferenceProviderName"
{-# DEPRECATED ccpConferenceProviderName "Use generic-lens or generic-optics with 'conferenceProviderName' instead." #-}

-- | Represents a type within a list of predefined types.
--
-- /Note:/ Consider using 'conferenceProviderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpConferenceProviderType :: Lens.Lens' CreateConferenceProvider Types.ConferenceProviderType
ccpConferenceProviderType = Lens.field @"conferenceProviderType"
{-# DEPRECATED ccpConferenceProviderType "Use generic-lens or generic-optics with 'conferenceProviderType' instead." #-}

-- | The meeting settings for the conference provider.
--
-- /Note:/ Consider using 'meetingSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpMeetingSetting :: Lens.Lens' CreateConferenceProvider Types.MeetingSetting
ccpMeetingSetting = Lens.field @"meetingSetting"
{-# DEPRECATED ccpMeetingSetting "Use generic-lens or generic-optics with 'meetingSetting' instead." #-}

-- | The request token of the client.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpClientRequestToken :: Lens.Lens' CreateConferenceProvider (Core.Maybe Types.ClientRequestToken)
ccpClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED ccpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The IP endpoint and protocol for calling.
--
-- /Note:/ Consider using 'iPDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpIPDialIn :: Lens.Lens' CreateConferenceProvider (Core.Maybe Types.IPDialIn)
ccpIPDialIn = Lens.field @"iPDialIn"
{-# DEPRECATED ccpIPDialIn "Use generic-lens or generic-optics with 'iPDialIn' instead." #-}

-- | The information for PSTN conferencing.
--
-- /Note:/ Consider using 'pSTNDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpPSTNDialIn :: Lens.Lens' CreateConferenceProvider (Core.Maybe Types.PSTNDialIn)
ccpPSTNDialIn = Lens.field @"pSTNDialIn"
{-# DEPRECATED ccpPSTNDialIn "Use generic-lens or generic-optics with 'pSTNDialIn' instead." #-}

instance Core.FromJSON CreateConferenceProvider where
  toJSON CreateConferenceProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ConferenceProviderName" Core..= conferenceProviderName),
            Core.Just
              ("ConferenceProviderType" Core..= conferenceProviderType),
            Core.Just ("MeetingSetting" Core..= meetingSetting),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("IPDialIn" Core..=) Core.<$> iPDialIn,
            ("PSTNDialIn" Core..=) Core.<$> pSTNDialIn
          ]
      )

instance Core.AWSRequest CreateConferenceProvider where
  type Rs CreateConferenceProvider = CreateConferenceProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.CreateConferenceProvider")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConferenceProviderResponse'
            Core.<$> (x Core..:? "ConferenceProviderArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateConferenceProviderResponse' smart constructor.
data CreateConferenceProviderResponse = CreateConferenceProviderResponse'
  { -- | The ARN of the newly-created conference provider.
    conferenceProviderArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConferenceProviderResponse' value with any optional fields omitted.
mkCreateConferenceProviderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateConferenceProviderResponse
mkCreateConferenceProviderResponse responseStatus =
  CreateConferenceProviderResponse'
    { conferenceProviderArn =
        Core.Nothing,
      responseStatus
    }

-- | The ARN of the newly-created conference provider.
--
-- /Note:/ Consider using 'conferenceProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsConferenceProviderArn :: Lens.Lens' CreateConferenceProviderResponse (Core.Maybe Types.Arn)
ccprrsConferenceProviderArn = Lens.field @"conferenceProviderArn"
{-# DEPRECATED ccprrsConferenceProviderArn "Use generic-lens or generic-optics with 'conferenceProviderArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsResponseStatus :: Lens.Lens' CreateConferenceProviderResponse Core.Int
ccprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
