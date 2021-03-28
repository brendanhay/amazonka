{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a service software update for an Amazon ES domain.
module Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
    (
    -- * Creating a request
      StartElasticsearchServiceSoftwareUpdate (..)
    , mkStartElasticsearchServiceSoftwareUpdate
    -- ** Request lenses
    , sessuDomainName

    -- * Destructuring the response
    , StartElasticsearchServiceSoftwareUpdateResponse (..)
    , mkStartElasticsearchServiceSoftwareUpdateResponse
    -- ** Response lenses
    , sessurrsServiceSoftwareOptions
    , sessurrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'StartElasticsearchServiceSoftwareUpdate' @ operation. Specifies the name of the Elasticsearch domain that you wish to schedule a service software update on.
--
-- /See:/ 'mkStartElasticsearchServiceSoftwareUpdate' smart constructor.
newtype StartElasticsearchServiceSoftwareUpdate = StartElasticsearchServiceSoftwareUpdate'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to update to the latest service software.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartElasticsearchServiceSoftwareUpdate' value with any optional fields omitted.
mkStartElasticsearchServiceSoftwareUpdate
    :: Types.DomainName -- ^ 'domainName'
    -> StartElasticsearchServiceSoftwareUpdate
mkStartElasticsearchServiceSoftwareUpdate domainName
  = StartElasticsearchServiceSoftwareUpdate'{domainName}

-- | The name of the domain that you want to update to the latest service software.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sessuDomainName :: Lens.Lens' StartElasticsearchServiceSoftwareUpdate Types.DomainName
sessuDomainName = Lens.field @"domainName"
{-# INLINEABLE sessuDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery StartElasticsearchServiceSoftwareUpdate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartElasticsearchServiceSoftwareUpdate
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON StartElasticsearchServiceSoftwareUpdate
         where
        toJSON StartElasticsearchServiceSoftwareUpdate{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest StartElasticsearchServiceSoftwareUpdate
         where
        type Rs StartElasticsearchServiceSoftwareUpdate =
             StartElasticsearchServiceSoftwareUpdateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2015-01-01/es/serviceSoftwareUpdate/start",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartElasticsearchServiceSoftwareUpdateResponse' Core.<$>
                   (x Core..:? "ServiceSoftwareOptions") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @StartElasticsearchServiceSoftwareUpdate@ operation. Contains the status of the update.
--
-- /See:/ 'mkStartElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data StartElasticsearchServiceSoftwareUpdateResponse = StartElasticsearchServiceSoftwareUpdateResponse'
  { serviceSoftwareOptions :: Core.Maybe Types.ServiceSoftwareOptions
    -- ^ The current status of the Elasticsearch service software update.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartElasticsearchServiceSoftwareUpdateResponse' value with any optional fields omitted.
mkStartElasticsearchServiceSoftwareUpdateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartElasticsearchServiceSoftwareUpdateResponse
mkStartElasticsearchServiceSoftwareUpdateResponse responseStatus
  = StartElasticsearchServiceSoftwareUpdateResponse'{serviceSoftwareOptions
                                                       = Core.Nothing,
                                                     responseStatus}

-- | The current status of the Elasticsearch service software update.
--
-- /Note:/ Consider using 'serviceSoftwareOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sessurrsServiceSoftwareOptions :: Lens.Lens' StartElasticsearchServiceSoftwareUpdateResponse (Core.Maybe Types.ServiceSoftwareOptions)
sessurrsServiceSoftwareOptions = Lens.field @"serviceSoftwareOptions"
{-# INLINEABLE sessurrsServiceSoftwareOptions #-}
{-# DEPRECATED serviceSoftwareOptions "Use generic-lens or generic-optics with 'serviceSoftwareOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sessurrsResponseStatus :: Lens.Lens' StartElasticsearchServiceSoftwareUpdateResponse Core.Int
sessurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sessurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
