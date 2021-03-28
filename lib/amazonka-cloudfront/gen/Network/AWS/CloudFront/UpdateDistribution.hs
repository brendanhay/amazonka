{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for a web distribution. 
--
-- /Important:/ When you update a distribution, there are more required fields than when you create a distribution. When you update your distribution by using this API action, follow the steps here to get the current configuration and then make your updates, to make sure that you include all of the required fields. To view a summary, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-overview-required-fields.html Required Fields for Create Distribution and Update Distribution> in the /Amazon CloudFront Developer Guide/ .
-- The update process includes getting the current distribution configuration, updating the XML document that is returned to make your changes, and then submitting an @UpdateDistribution@ request to make the updates.
-- For information about updating a distribution using the CloudFront console instead, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-creating-console.html Creating a Distribution> in the /Amazon CloudFront Developer Guide/ .
-- __To update a web distribution using the CloudFront API__ 
--
--     * Submit a <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_GetDistributionConfig.html GetDistributionConfig> request to get the current configuration and an @Etag@ header for the distribution.
--
--
--     * Update the XML document that was returned in the response to your @GetDistributionConfig@ request to include your changes. 
-- /Important:/ When you edit the XML file, be aware of the following:
--
--     * You must strip out the ETag parameter that is returned.
--
--
--     * Additional fields are required when you update a distribution. There may be fields included in the XML file for features that you haven't configured for your distribution. This is expected and required to successfully update the distribution.
--
--
--     * You can't change the value of @CallerReference@ . If you try to change this value, CloudFront returns an @IllegalUpdate@ error. 
--
--
--     * The new configuration replaces the existing configuration; the values that you specify in an @UpdateDistribution@ request are not merged into your existing configuration. When you add, delete, or replace values in an element that allows multiple values (for example, @CNAME@ ), you must specify all of the values that you want to appear in the updated distribution. In addition, you must update the corresponding @Quantity@ element.
--
--
--
--
--     * Submit an @UpdateDistribution@ request to update the configuration for your distribution:
--
--     * In the request body, include the XML document that you updated in Step 2. The request body must include an XML document with a @DistributionConfig@ element.
--
--
--     * Set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GetDistributionConfig@ request in Step 1.
--
--
--
--
--     * Review the response to the @UpdateDistribution@ request to confirm that the configuration was successfully updated.
--
--
--     * Optional: Submit a <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_GetDistribution.html GetDistribution> request to confirm that your changes have propagated. When propagation is complete, the value of @Status@ is @Deployed@ .
--
--
module Network.AWS.CloudFront.UpdateDistribution
    (
    -- * Creating a request
      UpdateDistribution (..)
    , mkUpdateDistribution
    -- ** Request lenses
    , udDistributionConfig
    , udId
    , udIfMatch

    -- * Destructuring the response
    , UpdateDistributionResponse (..)
    , mkUpdateDistributionResponse
    -- ** Response lenses
    , udrrsDistribution
    , udrrsETag
    , udrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update a distribution.
--
-- /See:/ 'mkUpdateDistribution' smart constructor.
data UpdateDistribution = UpdateDistribution'
  { distributionConfig :: Types.DistributionConfig
    -- ^ The distribution's configuration information.
  , id :: Core.Text
    -- ^ The distribution's id.
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header that you received when retrieving the distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDistribution' value with any optional fields omitted.
mkUpdateDistribution
    :: Types.DistributionConfig -- ^ 'distributionConfig'
    -> Core.Text -- ^ 'id'
    -> UpdateDistribution
mkUpdateDistribution distributionConfig id
  = UpdateDistribution'{distributionConfig, id,
                        ifMatch = Core.Nothing}

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDistributionConfig :: Lens.Lens' UpdateDistribution Types.DistributionConfig
udDistributionConfig = Lens.field @"distributionConfig"
{-# INLINEABLE udDistributionConfig #-}
{-# DEPRECATED distributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead"  #-}

-- | The distribution's id.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udId :: Lens.Lens' UpdateDistribution Core.Text
udId = Lens.field @"id"
{-# INLINEABLE udId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of the @ETag@ header that you received when retrieving the distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udIfMatch :: Lens.Lens' UpdateDistribution (Core.Maybe Core.Text)
udIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE udIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery UpdateDistribution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDistribution where
        toHeaders UpdateDistribution{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest UpdateDistribution where
        type Rs UpdateDistribution = UpdateDistributionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2020-05-31/distribution/" Core.<> Core.toText id Core.<>
                             "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateDistributionResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkUpdateDistributionResponse' smart constructor.
data UpdateDistributionResponse = UpdateDistributionResponse'
  { distribution :: Core.Maybe Types.Distribution
    -- ^ The distribution's information.
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateDistributionResponse' value with any optional fields omitted.
mkUpdateDistributionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDistributionResponse
mkUpdateDistributionResponse responseStatus
  = UpdateDistributionResponse'{distribution = Core.Nothing,
                                eTag = Core.Nothing, responseStatus}

-- | The distribution's information.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsDistribution :: Lens.Lens' UpdateDistributionResponse (Core.Maybe Types.Distribution)
udrrsDistribution = Lens.field @"distribution"
{-# INLINEABLE udrrsDistribution #-}
{-# DEPRECATED distribution "Use generic-lens or generic-optics with 'distribution' instead"  #-}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsETag :: Lens.Lens' UpdateDistributionResponse (Core.Maybe Core.Text)
udrrsETag = Lens.field @"eTag"
{-# INLINEABLE udrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDistributionResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
