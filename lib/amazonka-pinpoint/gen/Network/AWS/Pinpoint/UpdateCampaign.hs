{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateCampaign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration and other settings for a campaign.
module Network.AWS.Pinpoint.UpdateCampaign
    (
    -- * Creating a request
      UpdateCampaign (..)
    , mkUpdateCampaign
    -- ** Request lenses
    , ucCampaignId
    , ucApplicationId
    , ucWriteCampaignRequest

    -- * Destructuring the response
    , UpdateCampaignResponse (..)
    , mkUpdateCampaignResponse
    -- ** Response lenses
    , ucrrsCampaignResponse
    , ucrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCampaign' smart constructor.
data UpdateCampaign = UpdateCampaign'
  { campaignId :: Core.Text
    -- ^ The unique identifier for the campaign.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , writeCampaignRequest :: Types.WriteCampaignRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCampaign' value with any optional fields omitted.
mkUpdateCampaign
    :: Core.Text -- ^ 'campaignId'
    -> Core.Text -- ^ 'applicationId'
    -> Types.WriteCampaignRequest -- ^ 'writeCampaignRequest'
    -> UpdateCampaign
mkUpdateCampaign campaignId applicationId writeCampaignRequest
  = UpdateCampaign'{campaignId, applicationId, writeCampaignRequest}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCampaignId :: Lens.Lens' UpdateCampaign Core.Text
ucCampaignId = Lens.field @"campaignId"
{-# INLINEABLE ucCampaignId #-}
{-# DEPRECATED campaignId "Use generic-lens or generic-optics with 'campaignId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucApplicationId :: Lens.Lens' UpdateCampaign Core.Text
ucApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ucApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeCampaignRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucWriteCampaignRequest :: Lens.Lens' UpdateCampaign Types.WriteCampaignRequest
ucWriteCampaignRequest = Lens.field @"writeCampaignRequest"
{-# INLINEABLE ucWriteCampaignRequest #-}
{-# DEPRECATED writeCampaignRequest "Use generic-lens or generic-optics with 'writeCampaignRequest' instead"  #-}

instance Core.ToQuery UpdateCampaign where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCampaign where
        toHeaders UpdateCampaign{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateCampaign where
        toJSON UpdateCampaign{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WriteCampaignRequest" Core..= writeCampaignRequest)])

instance Core.AWSRequest UpdateCampaign where
        type Rs UpdateCampaign = UpdateCampaignResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/campaigns/"
                             Core.<> Core.toText campaignId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateCampaignResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateCampaignResponse' smart constructor.
data UpdateCampaignResponse = UpdateCampaignResponse'
  { campaignResponse :: Types.CampaignResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCampaignResponse' value with any optional fields omitted.
mkUpdateCampaignResponse
    :: Types.CampaignResponse -- ^ 'campaignResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateCampaignResponse
mkUpdateCampaignResponse campaignResponse responseStatus
  = UpdateCampaignResponse'{campaignResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsCampaignResponse :: Lens.Lens' UpdateCampaignResponse Types.CampaignResponse
ucrrsCampaignResponse = Lens.field @"campaignResponse"
{-# INLINEABLE ucrrsCampaignResponse #-}
{-# DEPRECATED campaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateCampaignResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
