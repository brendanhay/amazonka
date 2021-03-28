{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an association between a configuration set and a custom domain for open and click event tracking.
--
-- By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using custom domains, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide> .
module Network.AWS.SES.DeleteConfigurationSetTrackingOptions
    (
    -- * Creating a request
      DeleteConfigurationSetTrackingOptions (..)
    , mkDeleteConfigurationSetTrackingOptions
    -- ** Request lenses
    , dcstoConfigurationSetName

    -- * Destructuring the response
    , DeleteConfigurationSetTrackingOptionsResponse (..)
    , mkDeleteConfigurationSetTrackingOptionsResponse
    -- ** Response lenses
    , dcstorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete open and click tracking options in a configuration set. 
--
-- /See:/ 'mkDeleteConfigurationSetTrackingOptions' smart constructor.
newtype DeleteConfigurationSetTrackingOptions = DeleteConfigurationSetTrackingOptions'
  { configurationSetName :: Types.ConfigurationSetName
    -- ^ The name of the configuration set from which you want to delete the tracking options.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationSetTrackingOptions' value with any optional fields omitted.
mkDeleteConfigurationSetTrackingOptions
    :: Types.ConfigurationSetName -- ^ 'configurationSetName'
    -> DeleteConfigurationSetTrackingOptions
mkDeleteConfigurationSetTrackingOptions configurationSetName
  = DeleteConfigurationSetTrackingOptions'{configurationSetName}

-- | The name of the configuration set from which you want to delete the tracking options.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcstoConfigurationSetName :: Lens.Lens' DeleteConfigurationSetTrackingOptions Types.ConfigurationSetName
dcstoConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE dcstoConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

instance Core.ToQuery DeleteConfigurationSetTrackingOptions where
        toQuery DeleteConfigurationSetTrackingOptions{..}
          = Core.toQueryPair "Action"
              ("DeleteConfigurationSetTrackingOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConfigurationSetName" configurationSetName

instance Core.ToHeaders DeleteConfigurationSetTrackingOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteConfigurationSetTrackingOptions
         where
        type Rs DeleteConfigurationSetTrackingOptions =
             DeleteConfigurationSetTrackingOptionsResponse
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
              "DeleteConfigurationSetTrackingOptionsResult"
              (\ s h x ->
                 DeleteConfigurationSetTrackingOptionsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteConfigurationSetTrackingOptionsResponse' smart constructor.
newtype DeleteConfigurationSetTrackingOptionsResponse = DeleteConfigurationSetTrackingOptionsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationSetTrackingOptionsResponse' value with any optional fields omitted.
mkDeleteConfigurationSetTrackingOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteConfigurationSetTrackingOptionsResponse
mkDeleteConfigurationSetTrackingOptionsResponse responseStatus
  = DeleteConfigurationSetTrackingOptionsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcstorrsResponseStatus :: Lens.Lens' DeleteConfigurationSetTrackingOptionsResponse Core.Int
dcstorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcstorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
