{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> from an input.
module Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
    (
    -- * Creating a request
      DeleteApplicationInputProcessingConfiguration (..)
    , mkDeleteApplicationInputProcessingConfiguration
    -- ** Request lenses
    , daipcApplicationName
    , daipcCurrentApplicationVersionId
    , daipcInputId

    -- * Destructuring the response
    , DeleteApplicationInputProcessingConfigurationResponse (..)
    , mkDeleteApplicationInputProcessingConfigurationResponse
    -- ** Response lenses
    , daipcrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApplicationInputProcessingConfiguration' smart constructor.
data DeleteApplicationInputProcessingConfiguration = DeleteApplicationInputProcessingConfiguration'
  { applicationName :: Types.ApplicationName
    -- ^ The Kinesis Analytics application name.
  , currentApplicationVersionId :: Core.Natural
    -- ^ The version ID of the Kinesis Analytics application.
  , inputId :: Types.InputId
    -- ^ The ID of the input configuration from which to delete the input processing configuration. You can get a list of the input IDs for an application by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationInputProcessingConfiguration' value with any optional fields omitted.
mkDeleteApplicationInputProcessingConfiguration
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Core.Natural -- ^ 'currentApplicationVersionId'
    -> Types.InputId -- ^ 'inputId'
    -> DeleteApplicationInputProcessingConfiguration
mkDeleteApplicationInputProcessingConfiguration applicationName
  currentApplicationVersionId inputId
  = DeleteApplicationInputProcessingConfiguration'{applicationName,
                                                   currentApplicationVersionId, inputId}

-- | The Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daipcApplicationName :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Types.ApplicationName
daipcApplicationName = Lens.field @"applicationName"
{-# INLINEABLE daipcApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The version ID of the Kinesis Analytics application.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daipcCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Core.Natural
daipcCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# INLINEABLE daipcCurrentApplicationVersionId #-}
{-# DEPRECATED currentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead"  #-}

-- | The ID of the input configuration from which to delete the input processing configuration. You can get a list of the input IDs for an application by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daipcInputId :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Types.InputId
daipcInputId = Lens.field @"inputId"
{-# INLINEABLE daipcInputId #-}
{-# DEPRECATED inputId "Use generic-lens or generic-optics with 'inputId' instead"  #-}

instance Core.ToQuery DeleteApplicationInputProcessingConfiguration
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           DeleteApplicationInputProcessingConfiguration
         where
        toHeaders DeleteApplicationInputProcessingConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "KinesisAnalytics_20150814.DeleteApplicationInputProcessingConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           DeleteApplicationInputProcessingConfiguration
         where
        toJSON DeleteApplicationInputProcessingConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  Core.Just
                    ("CurrentApplicationVersionId" Core..=
                       currentApplicationVersionId),
                  Core.Just ("InputId" Core..= inputId)])

instance Core.AWSRequest
           DeleteApplicationInputProcessingConfiguration
         where
        type Rs DeleteApplicationInputProcessingConfiguration =
             DeleteApplicationInputProcessingConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteApplicationInputProcessingConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApplicationInputProcessingConfigurationResponse' smart constructor.
newtype DeleteApplicationInputProcessingConfigurationResponse = DeleteApplicationInputProcessingConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationInputProcessingConfigurationResponse' value with any optional fields omitted.
mkDeleteApplicationInputProcessingConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteApplicationInputProcessingConfigurationResponse
mkDeleteApplicationInputProcessingConfigurationResponse
  responseStatus
  = DeleteApplicationInputProcessingConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daipcrrsResponseStatus :: Lens.Lens' DeleteApplicationInputProcessingConfigurationResponse Core.Int
daipcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daipcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
