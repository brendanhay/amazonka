{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.BatchGetAggregateResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current configuration items for resources that are present in your AWS Config aggregator. The operation also returns a list of resources that are not processed in the current request. If there are no unprocessed resources, the operation returns an empty @unprocessedResourceIdentifiers@ list. 
module Network.AWS.Config.BatchGetAggregateResourceConfig
    (
    -- * Creating a request
      BatchGetAggregateResourceConfig (..)
    , mkBatchGetAggregateResourceConfig
    -- ** Request lenses
    , bgarcConfigurationAggregatorName
    , bgarcResourceIdentifiers

    -- * Destructuring the response
    , BatchGetAggregateResourceConfigResponse (..)
    , mkBatchGetAggregateResourceConfigResponse
    -- ** Response lenses
    , bgarcrrsBaseConfigurationItems
    , bgarcrrsUnprocessedResourceIdentifiers
    , bgarcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetAggregateResourceConfig' smart constructor.
data BatchGetAggregateResourceConfig = BatchGetAggregateResourceConfig'
  { configurationAggregatorName :: Types.ConfigurationAggregatorName
    -- ^ The name of the configuration aggregator.
  , resourceIdentifiers :: Core.NonEmpty Types.AggregateResourceIdentifier
    -- ^ A list of aggregate ResourceIdentifiers objects. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetAggregateResourceConfig' value with any optional fields omitted.
mkBatchGetAggregateResourceConfig
    :: Types.ConfigurationAggregatorName -- ^ 'configurationAggregatorName'
    -> Core.NonEmpty Types.AggregateResourceIdentifier -- ^ 'resourceIdentifiers'
    -> BatchGetAggregateResourceConfig
mkBatchGetAggregateResourceConfig configurationAggregatorName
  resourceIdentifiers
  = BatchGetAggregateResourceConfig'{configurationAggregatorName,
                                     resourceIdentifiers}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcConfigurationAggregatorName :: Lens.Lens' BatchGetAggregateResourceConfig Types.ConfigurationAggregatorName
bgarcConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# INLINEABLE bgarcConfigurationAggregatorName #-}
{-# DEPRECATED configurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead"  #-}

-- | A list of aggregate ResourceIdentifiers objects. 
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcResourceIdentifiers :: Lens.Lens' BatchGetAggregateResourceConfig (Core.NonEmpty Types.AggregateResourceIdentifier)
bgarcResourceIdentifiers = Lens.field @"resourceIdentifiers"
{-# INLINEABLE bgarcResourceIdentifiers #-}
{-# DEPRECATED resourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead"  #-}

instance Core.ToQuery BatchGetAggregateResourceConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetAggregateResourceConfig where
        toHeaders BatchGetAggregateResourceConfig{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.BatchGetAggregateResourceConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetAggregateResourceConfig where
        toJSON BatchGetAggregateResourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConfigurationAggregatorName" Core..=
                       configurationAggregatorName),
                  Core.Just ("ResourceIdentifiers" Core..= resourceIdentifiers)])

instance Core.AWSRequest BatchGetAggregateResourceConfig where
        type Rs BatchGetAggregateResourceConfig =
             BatchGetAggregateResourceConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetAggregateResourceConfigResponse' Core.<$>
                   (x Core..:? "BaseConfigurationItems") Core.<*>
                     x Core..:? "UnprocessedResourceIdentifiers"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetAggregateResourceConfigResponse' smart constructor.
data BatchGetAggregateResourceConfigResponse = BatchGetAggregateResourceConfigResponse'
  { baseConfigurationItems :: Core.Maybe [Types.BaseConfigurationItem]
    -- ^ A list that contains the current configuration of one or more resources.
  , unprocessedResourceIdentifiers :: Core.Maybe [Types.AggregateResourceIdentifier]
    -- ^ A list of resource identifiers that were not processed with current scope. The list is empty if all the resources are processed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetAggregateResourceConfigResponse' value with any optional fields omitted.
mkBatchGetAggregateResourceConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetAggregateResourceConfigResponse
mkBatchGetAggregateResourceConfigResponse responseStatus
  = BatchGetAggregateResourceConfigResponse'{baseConfigurationItems =
                                               Core.Nothing,
                                             unprocessedResourceIdentifiers = Core.Nothing,
                                             responseStatus}

-- | A list that contains the current configuration of one or more resources.
--
-- /Note:/ Consider using 'baseConfigurationItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcrrsBaseConfigurationItems :: Lens.Lens' BatchGetAggregateResourceConfigResponse (Core.Maybe [Types.BaseConfigurationItem])
bgarcrrsBaseConfigurationItems = Lens.field @"baseConfigurationItems"
{-# INLINEABLE bgarcrrsBaseConfigurationItems #-}
{-# DEPRECATED baseConfigurationItems "Use generic-lens or generic-optics with 'baseConfigurationItems' instead"  #-}

-- | A list of resource identifiers that were not processed with current scope. The list is empty if all the resources are processed.
--
-- /Note:/ Consider using 'unprocessedResourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcrrsUnprocessedResourceIdentifiers :: Lens.Lens' BatchGetAggregateResourceConfigResponse (Core.Maybe [Types.AggregateResourceIdentifier])
bgarcrrsUnprocessedResourceIdentifiers = Lens.field @"unprocessedResourceIdentifiers"
{-# INLINEABLE bgarcrrsUnprocessedResourceIdentifiers #-}
{-# DEPRECATED unprocessedResourceIdentifiers "Use generic-lens or generic-optics with 'unprocessedResourceIdentifiers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcrrsResponseStatus :: Lens.Lens' BatchGetAggregateResourceConfigResponse Core.Int
bgarcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgarcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
