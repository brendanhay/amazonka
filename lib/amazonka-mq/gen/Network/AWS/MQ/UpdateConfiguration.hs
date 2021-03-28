{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.UpdateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified configuration.
module Network.AWS.MQ.UpdateConfiguration
    (
    -- * Creating a request
      UpdateConfiguration (..)
    , mkUpdateConfiguration
    -- ** Request lenses
    , ucConfigurationId
    , ucData
    , ucDescription

    -- * Destructuring the response
    , UpdateConfigurationResponse (..)
    , mkUpdateConfigurationResponse
    -- ** Response lenses
    , ucrrsArn
    , ucrrsCreated
    , ucrrsId
    , ucrrsLatestRevision
    , ucrrsName
    , ucrrsWarnings
    , ucrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates the specified configuration.
--
-- /See:/ 'mkUpdateConfiguration' smart constructor.
data UpdateConfiguration = UpdateConfiguration'
  { configurationId :: Core.Text
    -- ^ The unique ID that Amazon MQ generates for the configuration.
  , data' :: Core.Maybe Core.Text
    -- ^ Required. The base64-encoded XML configuration.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConfiguration' value with any optional fields omitted.
mkUpdateConfiguration
    :: Core.Text -- ^ 'configurationId'
    -> UpdateConfiguration
mkUpdateConfiguration configurationId
  = UpdateConfiguration'{configurationId, data' = Core.Nothing,
                         description = Core.Nothing}

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucConfigurationId :: Lens.Lens' UpdateConfiguration Core.Text
ucConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE ucConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

-- | Required. The base64-encoded XML configuration.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucData :: Lens.Lens' UpdateConfiguration (Core.Maybe Core.Text)
ucData = Lens.field @"data'"
{-# INLINEABLE ucData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | The description of the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateConfiguration (Core.Maybe Core.Text)
ucDescription = Lens.field @"description"
{-# INLINEABLE ucDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery UpdateConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateConfiguration where
        toHeaders UpdateConfiguration{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateConfiguration where
        toJSON UpdateConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("data" Core..=) Core.<$> data',
                  ("description" Core..=) Core.<$> description])

instance Core.AWSRequest UpdateConfiguration where
        type Rs UpdateConfiguration = UpdateConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/configurations/" Core.<> Core.toText configurationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateConfigurationResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "created" Core.<*>
                     x Core..:? "id"
                     Core.<*> x Core..:? "latestRevision"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "warnings"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateConfigurationResponse' smart constructor.
data UpdateConfigurationResponse = UpdateConfigurationResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ Required. The Amazon Resource Name (ARN) of the configuration.
  , created :: Core.Maybe Core.UTCTime
    -- ^ Required. The date and time of the configuration.
  , id :: Core.Maybe Core.Text
    -- ^ Required. The unique ID that Amazon MQ generates for the configuration.
  , latestRevision :: Core.Maybe Types.ConfigurationRevision
    -- ^ The latest revision of the configuration.
  , name :: Core.Maybe Core.Text
    -- ^ Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
  , warnings :: Core.Maybe [Types.SanitizationWarning]
    -- ^ The list of the first 20 warnings about the configuration XML elements or attributes that were sanitized.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateConfigurationResponse' value with any optional fields omitted.
mkUpdateConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateConfigurationResponse
mkUpdateConfigurationResponse responseStatus
  = UpdateConfigurationResponse'{arn = Core.Nothing,
                                 created = Core.Nothing, id = Core.Nothing,
                                 latestRevision = Core.Nothing, name = Core.Nothing,
                                 warnings = Core.Nothing, responseStatus}

-- | Required. The Amazon Resource Name (ARN) of the configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsArn :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Core.Text)
ucrrsArn = Lens.field @"arn"
{-# INLINEABLE ucrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Required. The date and time of the configuration.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsCreated :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Core.UTCTime)
ucrrsCreated = Lens.field @"created"
{-# INLINEABLE ucrrsCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsId :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Core.Text)
ucrrsId = Lens.field @"id"
{-# INLINEABLE ucrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The latest revision of the configuration.
--
-- /Note:/ Consider using 'latestRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsLatestRevision :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Types.ConfigurationRevision)
ucrrsLatestRevision = Lens.field @"latestRevision"
{-# INLINEABLE ucrrsLatestRevision #-}
{-# DEPRECATED latestRevision "Use generic-lens or generic-optics with 'latestRevision' instead"  #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsName :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Core.Text)
ucrrsName = Lens.field @"name"
{-# INLINEABLE ucrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The list of the first 20 warnings about the configuration XML elements or attributes that were sanitized.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsWarnings :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe [Types.SanitizationWarning])
ucrrsWarnings = Lens.field @"warnings"
{-# INLINEABLE ucrrsWarnings #-}
{-# DEPRECATED warnings "Use generic-lens or generic-optics with 'warnings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateConfigurationResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
