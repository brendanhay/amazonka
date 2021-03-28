{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an association between a configuration set and a custom domain for open and click event tracking. 
--
-- By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using custom domains, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide> .
module Network.AWS.SES.UpdateConfigurationSetTrackingOptions
    (
    -- * Creating a request
      UpdateConfigurationSetTrackingOptions (..)
    , mkUpdateConfigurationSetTrackingOptions
    -- ** Request lenses
    , ucstoConfigurationSetName
    , ucstoTrackingOptions

    -- * Destructuring the response
    , UpdateConfigurationSetTrackingOptionsResponse (..)
    , mkUpdateConfigurationSetTrackingOptionsResponse
    -- ** Response lenses
    , ucstorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to update the tracking options for a configuration set. 
--
-- /See:/ 'mkUpdateConfigurationSetTrackingOptions' smart constructor.
data UpdateConfigurationSetTrackingOptions = UpdateConfigurationSetTrackingOptions'
  { configurationSetName :: Types.ConfigurationSetName
    -- ^ The name of the configuration set for which you want to update the custom tracking domain.
  , trackingOptions :: Types.TrackingOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConfigurationSetTrackingOptions' value with any optional fields omitted.
mkUpdateConfigurationSetTrackingOptions
    :: Types.ConfigurationSetName -- ^ 'configurationSetName'
    -> Types.TrackingOptions -- ^ 'trackingOptions'
    -> UpdateConfigurationSetTrackingOptions
mkUpdateConfigurationSetTrackingOptions configurationSetName
  trackingOptions
  = UpdateConfigurationSetTrackingOptions'{configurationSetName,
                                           trackingOptions}

-- | The name of the configuration set for which you want to update the custom tracking domain.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucstoConfigurationSetName :: Lens.Lens' UpdateConfigurationSetTrackingOptions Types.ConfigurationSetName
ucstoConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE ucstoConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trackingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucstoTrackingOptions :: Lens.Lens' UpdateConfigurationSetTrackingOptions Types.TrackingOptions
ucstoTrackingOptions = Lens.field @"trackingOptions"
{-# INLINEABLE ucstoTrackingOptions #-}
{-# DEPRECATED trackingOptions "Use generic-lens or generic-optics with 'trackingOptions' instead"  #-}

instance Core.ToQuery UpdateConfigurationSetTrackingOptions where
        toQuery UpdateConfigurationSetTrackingOptions{..}
          = Core.toQueryPair "Action"
              ("UpdateConfigurationSetTrackingOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConfigurationSetName" configurationSetName
              Core.<> Core.toQueryPair "TrackingOptions" trackingOptions

instance Core.ToHeaders UpdateConfigurationSetTrackingOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateConfigurationSetTrackingOptions
         where
        type Rs UpdateConfigurationSetTrackingOptions =
             UpdateConfigurationSetTrackingOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "UpdateConfigurationSetTrackingOptionsResult"
              (\ s h x ->
                 UpdateConfigurationSetTrackingOptionsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkUpdateConfigurationSetTrackingOptionsResponse' smart constructor.
newtype UpdateConfigurationSetTrackingOptionsResponse = UpdateConfigurationSetTrackingOptionsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConfigurationSetTrackingOptionsResponse' value with any optional fields omitted.
mkUpdateConfigurationSetTrackingOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateConfigurationSetTrackingOptionsResponse
mkUpdateConfigurationSetTrackingOptionsResponse responseStatus
  = UpdateConfigurationSetTrackingOptionsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucstorrsResponseStatus :: Lens.Lens' UpdateConfigurationSetTrackingOptionsResponse Core.Int
ucstorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucstorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
