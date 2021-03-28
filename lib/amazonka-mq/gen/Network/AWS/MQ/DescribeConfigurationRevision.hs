{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeConfigurationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified configuration revision for the specified configuration.
module Network.AWS.MQ.DescribeConfigurationRevision
    (
    -- * Creating a request
      DescribeConfigurationRevision (..)
    , mkDescribeConfigurationRevision
    -- ** Request lenses
    , dcrConfigurationRevision
    , dcrConfigurationId

    -- * Destructuring the response
    , DescribeConfigurationRevisionResponse (..)
    , mkDescribeConfigurationRevisionResponse
    -- ** Response lenses
    , dcrrrsConfigurationId
    , dcrrrsCreated
    , dcrrrsData
    , dcrrrsDescription
    , dcrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConfigurationRevision' smart constructor.
data DescribeConfigurationRevision = DescribeConfigurationRevision'
  { configurationRevision :: Core.Text
    -- ^ The revision of the configuration.
  , configurationId :: Core.Text
    -- ^ The unique ID that Amazon MQ generates for the configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationRevision' value with any optional fields omitted.
mkDescribeConfigurationRevision
    :: Core.Text -- ^ 'configurationRevision'
    -> Core.Text -- ^ 'configurationId'
    -> DescribeConfigurationRevision
mkDescribeConfigurationRevision configurationRevision
  configurationId
  = DescribeConfigurationRevision'{configurationRevision,
                                   configurationId}

-- | The revision of the configuration.
--
-- /Note:/ Consider using 'configurationRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigurationRevision :: Lens.Lens' DescribeConfigurationRevision Core.Text
dcrConfigurationRevision = Lens.field @"configurationRevision"
{-# INLINEABLE dcrConfigurationRevision #-}
{-# DEPRECATED configurationRevision "Use generic-lens or generic-optics with 'configurationRevision' instead"  #-}

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigurationId :: Lens.Lens' DescribeConfigurationRevision Core.Text
dcrConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE dcrConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

instance Core.ToQuery DescribeConfigurationRevision where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeConfigurationRevision where
        toHeaders DescribeConfigurationRevision{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeConfigurationRevision where
        type Rs DescribeConfigurationRevision =
             DescribeConfigurationRevisionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/configurations/" Core.<> Core.toText configurationId Core.<>
                             "/revisions/"
                             Core.<> Core.toText configurationRevision,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeConfigurationRevisionResponse' Core.<$>
                   (x Core..:? "configurationId") Core.<*> x Core..:? "created"
                     Core.<*> x Core..:? "data"
                     Core.<*> x Core..:? "description"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeConfigurationRevisionResponse' smart constructor.
data DescribeConfigurationRevisionResponse = DescribeConfigurationRevisionResponse'
  { configurationId :: Core.Maybe Core.Text
    -- ^ Required. The unique ID that Amazon MQ generates for the configuration.
  , created :: Core.Maybe Core.UTCTime
    -- ^ Required. The date and time of the configuration.
  , data' :: Core.Maybe Core.Text
    -- ^ Required. The base64-encoded XML configuration.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeConfigurationRevisionResponse' value with any optional fields omitted.
mkDescribeConfigurationRevisionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConfigurationRevisionResponse
mkDescribeConfigurationRevisionResponse responseStatus
  = DescribeConfigurationRevisionResponse'{configurationId =
                                             Core.Nothing,
                                           created = Core.Nothing, data' = Core.Nothing,
                                           description = Core.Nothing, responseStatus}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsConfigurationId :: Lens.Lens' DescribeConfigurationRevisionResponse (Core.Maybe Core.Text)
dcrrrsConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE dcrrrsConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

-- | Required. The date and time of the configuration.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsCreated :: Lens.Lens' DescribeConfigurationRevisionResponse (Core.Maybe Core.UTCTime)
dcrrrsCreated = Lens.field @"created"
{-# INLINEABLE dcrrrsCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | Required. The base64-encoded XML configuration.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsData :: Lens.Lens' DescribeConfigurationRevisionResponse (Core.Maybe Core.Text)
dcrrrsData = Lens.field @"data'"
{-# INLINEABLE dcrrrsData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | The description of the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsDescription :: Lens.Lens' DescribeConfigurationRevisionResponse (Core.Maybe Core.Text)
dcrrrsDescription = Lens.field @"description"
{-# INLINEABLE dcrrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsResponseStatus :: Lens.Lens' DescribeConfigurationRevisionResponse Core.Int
dcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
