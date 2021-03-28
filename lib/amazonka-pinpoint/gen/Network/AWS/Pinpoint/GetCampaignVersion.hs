{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetCampaignVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for a specific version of a campaign.
module Network.AWS.Pinpoint.GetCampaignVersion
    (
    -- * Creating a request
      GetCampaignVersion (..)
    , mkGetCampaignVersion
    -- ** Request lenses
    , gcvfVersion
    , gcvfApplicationId
    , gcvfCampaignId

    -- * Destructuring the response
    , GetCampaignVersionResponse (..)
    , mkGetCampaignVersionResponse
    -- ** Response lenses
    , gcvrfrsCampaignResponse
    , gcvrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCampaignVersion' smart constructor.
data GetCampaignVersion = GetCampaignVersion'
  { version :: Core.Text
    -- ^ The unique version number (Version property) for the campaign version.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , campaignId :: Core.Text
    -- ^ The unique identifier for the campaign.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignVersion' value with any optional fields omitted.
mkGetCampaignVersion
    :: Core.Text -- ^ 'version'
    -> Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'campaignId'
    -> GetCampaignVersion
mkGetCampaignVersion version applicationId campaignId
  = GetCampaignVersion'{version, applicationId, campaignId}

-- | The unique version number (Version property) for the campaign version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvfVersion :: Lens.Lens' GetCampaignVersion Core.Text
gcvfVersion = Lens.field @"version"
{-# INLINEABLE gcvfVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvfApplicationId :: Lens.Lens' GetCampaignVersion Core.Text
gcvfApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gcvfApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvfCampaignId :: Lens.Lens' GetCampaignVersion Core.Text
gcvfCampaignId = Lens.field @"campaignId"
{-# INLINEABLE gcvfCampaignId #-}
{-# DEPRECATED campaignId "Use generic-lens or generic-optics with 'campaignId' instead"  #-}

instance Core.ToQuery GetCampaignVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCampaignVersion where
        toHeaders GetCampaignVersion{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetCampaignVersion where
        type Rs GetCampaignVersion = GetCampaignVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/campaigns/"
                             Core.<> Core.toText campaignId
                             Core.<> "/versions/"
                             Core.<> Core.toText version,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCampaignVersionResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCampaignVersionResponse' smart constructor.
data GetCampaignVersionResponse = GetCampaignVersionResponse'
  { campaignResponse :: Types.CampaignResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignVersionResponse' value with any optional fields omitted.
mkGetCampaignVersionResponse
    :: Types.CampaignResponse -- ^ 'campaignResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetCampaignVersionResponse
mkGetCampaignVersionResponse campaignResponse responseStatus
  = GetCampaignVersionResponse'{campaignResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvrfrsCampaignResponse :: Lens.Lens' GetCampaignVersionResponse Types.CampaignResponse
gcvrfrsCampaignResponse = Lens.field @"campaignResponse"
{-# INLINEABLE gcvrfrsCampaignResponse #-}
{-# DEPRECATED campaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvrfrsResponseStatus :: Lens.Lens' GetCampaignVersionResponse Core.Int
gcvrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcvrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
