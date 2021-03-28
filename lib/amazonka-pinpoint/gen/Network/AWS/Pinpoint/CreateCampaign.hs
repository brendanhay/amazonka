{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateCampaign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new campaign for an application or updates the settings of an existing campaign for an application.
module Network.AWS.Pinpoint.CreateCampaign
    (
    -- * Creating a request
      CreateCampaign (..)
    , mkCreateCampaign
    -- ** Request lenses
    , ccApplicationId
    , ccWriteCampaignRequest

    -- * Destructuring the response
    , CreateCampaignResponse (..)
    , mkCreateCampaignResponse
    -- ** Response lenses
    , ccrrsCampaignResponse
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCampaign' smart constructor.
data CreateCampaign = CreateCampaign'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , writeCampaignRequest :: Types.WriteCampaignRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCampaign' value with any optional fields omitted.
mkCreateCampaign
    :: Core.Text -- ^ 'applicationId'
    -> Types.WriteCampaignRequest -- ^ 'writeCampaignRequest'
    -> CreateCampaign
mkCreateCampaign applicationId writeCampaignRequest
  = CreateCampaign'{applicationId, writeCampaignRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccApplicationId :: Lens.Lens' CreateCampaign Core.Text
ccApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ccApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeCampaignRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccWriteCampaignRequest :: Lens.Lens' CreateCampaign Types.WriteCampaignRequest
ccWriteCampaignRequest = Lens.field @"writeCampaignRequest"
{-# INLINEABLE ccWriteCampaignRequest #-}
{-# DEPRECATED writeCampaignRequest "Use generic-lens or generic-optics with 'writeCampaignRequest' instead"  #-}

instance Core.ToQuery CreateCampaign where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCampaign where
        toHeaders CreateCampaign{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCampaign where
        toJSON CreateCampaign{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WriteCampaignRequest" Core..= writeCampaignRequest)])

instance Core.AWSRequest CreateCampaign where
        type Rs CreateCampaign = CreateCampaignResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/campaigns",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCampaignResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'
  { campaignResponse :: Types.CampaignResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCampaignResponse' value with any optional fields omitted.
mkCreateCampaignResponse
    :: Types.CampaignResponse -- ^ 'campaignResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateCampaignResponse
mkCreateCampaignResponse campaignResponse responseStatus
  = CreateCampaignResponse'{campaignResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCampaignResponse :: Lens.Lens' CreateCampaignResponse Types.CampaignResponse
ccrrsCampaignResponse = Lens.field @"campaignResponse"
{-# INLINEABLE ccrrsCampaignResponse #-}
{-# DEPRECATED campaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCampaignResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
