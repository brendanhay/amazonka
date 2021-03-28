{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a configuration set and a custom domain for open and click event tracking. 
--
-- By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using custom domains, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide> .
module Network.AWS.SES.CreateConfigurationSetTrackingOptions
    (
    -- * Creating a request
      CreateConfigurationSetTrackingOptions (..)
    , mkCreateConfigurationSetTrackingOptions
    -- ** Request lenses
    , ccstoConfigurationSetName
    , ccstoTrackingOptions

    -- * Destructuring the response
    , CreateConfigurationSetTrackingOptionsResponse (..)
    , mkCreateConfigurationSetTrackingOptionsResponse
    -- ** Response lenses
    , ccstorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create an open and click tracking option object in a configuration set. 
--
-- /See:/ 'mkCreateConfigurationSetTrackingOptions' smart constructor.
data CreateConfigurationSetTrackingOptions = CreateConfigurationSetTrackingOptions'
  { configurationSetName :: Types.ConfigurationSetName
    -- ^ The name of the configuration set that the tracking options should be associated with.
  , trackingOptions :: Types.TrackingOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConfigurationSetTrackingOptions' value with any optional fields omitted.
mkCreateConfigurationSetTrackingOptions
    :: Types.ConfigurationSetName -- ^ 'configurationSetName'
    -> Types.TrackingOptions -- ^ 'trackingOptions'
    -> CreateConfigurationSetTrackingOptions
mkCreateConfigurationSetTrackingOptions configurationSetName
  trackingOptions
  = CreateConfigurationSetTrackingOptions'{configurationSetName,
                                           trackingOptions}

-- | The name of the configuration set that the tracking options should be associated with.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccstoConfigurationSetName :: Lens.Lens' CreateConfigurationSetTrackingOptions Types.ConfigurationSetName
ccstoConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE ccstoConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trackingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccstoTrackingOptions :: Lens.Lens' CreateConfigurationSetTrackingOptions Types.TrackingOptions
ccstoTrackingOptions = Lens.field @"trackingOptions"
{-# INLINEABLE ccstoTrackingOptions #-}
{-# DEPRECATED trackingOptions "Use generic-lens or generic-optics with 'trackingOptions' instead"  #-}

instance Core.ToQuery CreateConfigurationSetTrackingOptions where
        toQuery CreateConfigurationSetTrackingOptions{..}
          = Core.toQueryPair "Action"
              ("CreateConfigurationSetTrackingOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConfigurationSetName" configurationSetName
              Core.<> Core.toQueryPair "TrackingOptions" trackingOptions

instance Core.ToHeaders CreateConfigurationSetTrackingOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateConfigurationSetTrackingOptions
         where
        type Rs CreateConfigurationSetTrackingOptions =
             CreateConfigurationSetTrackingOptionsResponse
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
              "CreateConfigurationSetTrackingOptionsResult"
              (\ s h x ->
                 CreateConfigurationSetTrackingOptionsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateConfigurationSetTrackingOptionsResponse' smart constructor.
newtype CreateConfigurationSetTrackingOptionsResponse = CreateConfigurationSetTrackingOptionsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConfigurationSetTrackingOptionsResponse' value with any optional fields omitted.
mkCreateConfigurationSetTrackingOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateConfigurationSetTrackingOptionsResponse
mkCreateConfigurationSetTrackingOptionsResponse responseStatus
  = CreateConfigurationSetTrackingOptionsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccstorrsResponseStatus :: Lens.Lens' CreateConfigurationSetTrackingOptionsResponse Core.Int
ccstorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccstorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
