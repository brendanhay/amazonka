{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateConferenceProvider (..)
    , mkCreateConferenceProvider
    -- ** Request lenses
    , ccpConferenceProviderName
    , ccpConferenceProviderType
    , ccpMeetingSetting
    , ccpClientRequestToken
    , ccpIPDialIn
    , ccpPSTNDialIn

    -- * Destructuring the response
    , CreateConferenceProviderResponse (..)
    , mkCreateConferenceProviderResponse
    -- ** Response lenses
    , ccprrsConferenceProviderArn
    , ccprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateConferenceProvider' smart constructor.
data CreateConferenceProvider = CreateConferenceProvider'
  { conferenceProviderName :: Types.ConferenceProviderName
    -- ^ The name of the conference provider.
  , conferenceProviderType :: Types.ConferenceProviderType
    -- ^ Represents a type within a list of predefined types.
  , meetingSetting :: Types.MeetingSetting
    -- ^ The meeting settings for the conference provider.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ The request token of the client.
  , iPDialIn :: Core.Maybe Types.IPDialIn
    -- ^ The IP endpoint and protocol for calling.
  , pSTNDialIn :: Core.Maybe Types.PSTNDialIn
    -- ^ The information for PSTN conferencing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConferenceProvider' value with any optional fields omitted.
mkCreateConferenceProvider
    :: Types.ConferenceProviderName -- ^ 'conferenceProviderName'
    -> Types.ConferenceProviderType -- ^ 'conferenceProviderType'
    -> Types.MeetingSetting -- ^ 'meetingSetting'
    -> CreateConferenceProvider
mkCreateConferenceProvider conferenceProviderName
  conferenceProviderType meetingSetting
  = CreateConferenceProvider'{conferenceProviderName,
                              conferenceProviderType, meetingSetting,
                              clientRequestToken = Core.Nothing, iPDialIn = Core.Nothing,
                              pSTNDialIn = Core.Nothing}

-- | The name of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpConferenceProviderName :: Lens.Lens' CreateConferenceProvider Types.ConferenceProviderName
ccpConferenceProviderName = Lens.field @"conferenceProviderName"
{-# INLINEABLE ccpConferenceProviderName #-}
{-# DEPRECATED conferenceProviderName "Use generic-lens or generic-optics with 'conferenceProviderName' instead"  #-}

-- | Represents a type within a list of predefined types.
--
-- /Note:/ Consider using 'conferenceProviderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpConferenceProviderType :: Lens.Lens' CreateConferenceProvider Types.ConferenceProviderType
ccpConferenceProviderType = Lens.field @"conferenceProviderType"
{-# INLINEABLE ccpConferenceProviderType #-}
{-# DEPRECATED conferenceProviderType "Use generic-lens or generic-optics with 'conferenceProviderType' instead"  #-}

-- | The meeting settings for the conference provider.
--
-- /Note:/ Consider using 'meetingSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpMeetingSetting :: Lens.Lens' CreateConferenceProvider Types.MeetingSetting
ccpMeetingSetting = Lens.field @"meetingSetting"
{-# INLINEABLE ccpMeetingSetting #-}
{-# DEPRECATED meetingSetting "Use generic-lens or generic-optics with 'meetingSetting' instead"  #-}

-- | The request token of the client.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpClientRequestToken :: Lens.Lens' CreateConferenceProvider (Core.Maybe Types.ClientRequestToken)
ccpClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE ccpClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The IP endpoint and protocol for calling.
--
-- /Note:/ Consider using 'iPDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpIPDialIn :: Lens.Lens' CreateConferenceProvider (Core.Maybe Types.IPDialIn)
ccpIPDialIn = Lens.field @"iPDialIn"
{-# INLINEABLE ccpIPDialIn #-}
{-# DEPRECATED iPDialIn "Use generic-lens or generic-optics with 'iPDialIn' instead"  #-}

-- | The information for PSTN conferencing.
--
-- /Note:/ Consider using 'pSTNDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpPSTNDialIn :: Lens.Lens' CreateConferenceProvider (Core.Maybe Types.PSTNDialIn)
ccpPSTNDialIn = Lens.field @"pSTNDialIn"
{-# INLINEABLE ccpPSTNDialIn #-}
{-# DEPRECATED pSTNDialIn "Use generic-lens or generic-optics with 'pSTNDialIn' instead"  #-}

instance Core.ToQuery CreateConferenceProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateConferenceProvider where
        toHeaders CreateConferenceProvider{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.CreateConferenceProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateConferenceProvider where
        toJSON CreateConferenceProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConferenceProviderName" Core..= conferenceProviderName),
                  Core.Just
                    ("ConferenceProviderType" Core..= conferenceProviderType),
                  Core.Just ("MeetingSetting" Core..= meetingSetting),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("IPDialIn" Core..=) Core.<$> iPDialIn,
                  ("PSTNDialIn" Core..=) Core.<$> pSTNDialIn])

instance Core.AWSRequest CreateConferenceProvider where
        type Rs CreateConferenceProvider = CreateConferenceProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateConferenceProviderResponse' Core.<$>
                   (x Core..:? "ConferenceProviderArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateConferenceProviderResponse' smart constructor.
data CreateConferenceProviderResponse = CreateConferenceProviderResponse'
  { conferenceProviderArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the newly-created conference provider.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConferenceProviderResponse' value with any optional fields omitted.
mkCreateConferenceProviderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateConferenceProviderResponse
mkCreateConferenceProviderResponse responseStatus
  = CreateConferenceProviderResponse'{conferenceProviderArn =
                                        Core.Nothing,
                                      responseStatus}

-- | The ARN of the newly-created conference provider.
--
-- /Note:/ Consider using 'conferenceProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsConferenceProviderArn :: Lens.Lens' CreateConferenceProviderResponse (Core.Maybe Types.Arn)
ccprrsConferenceProviderArn = Lens.field @"conferenceProviderArn"
{-# INLINEABLE ccprrsConferenceProviderArn #-}
{-# DEPRECATED conferenceProviderArn "Use generic-lens or generic-optics with 'conferenceProviderArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsResponseStatus :: Lens.Lens' CreateConferenceProviderResponse Core.Int
ccprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
