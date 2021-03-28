{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.StartApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified Amazon Kinesis Analytics application. After creating an application, you must exclusively call this operation to start your application.
--
-- After the application starts, it begins consuming the input data, processes it, and writes the output to the configured destination.
-- The application status must be @READY@ for you to start an application. You can get the application status in the console or using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
-- After you start the application, you can stop the application from processing the input by calling the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_StopApplication.html StopApplication> operation.
-- This operation requires permissions to perform the @kinesisanalytics:StartApplication@ action.
module Network.AWS.KinesisAnalytics.StartApplication
    (
    -- * Creating a request
      StartApplication (..)
    , mkStartApplication
    -- ** Request lenses
    , saApplicationName
    , saInputConfigurations

    -- * Destructuring the response
    , StartApplicationResponse (..)
    , mkStartApplicationResponse
    -- ** Response lenses
    , sarrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkStartApplication' smart constructor.
data StartApplication = StartApplication'
  { applicationName :: Types.ApplicationName
    -- ^ Name of the application.
  , inputConfigurations :: [Types.InputConfiguration]
    -- ^ Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartApplication' value with any optional fields omitted.
mkStartApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> StartApplication
mkStartApplication applicationName
  = StartApplication'{applicationName,
                      inputConfigurations = Core.mempty}

-- | Name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saApplicationName :: Lens.Lens' StartApplication Types.ApplicationName
saApplicationName = Lens.field @"applicationName"
{-# INLINEABLE saApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
--
-- /Note:/ Consider using 'inputConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saInputConfigurations :: Lens.Lens' StartApplication [Types.InputConfiguration]
saInputConfigurations = Lens.field @"inputConfigurations"
{-# INLINEABLE saInputConfigurations #-}
{-# DEPRECATED inputConfigurations "Use generic-lens or generic-optics with 'inputConfigurations' instead"  #-}

instance Core.ToQuery StartApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartApplication where
        toHeaders StartApplication{..}
          = Core.pure
              ("X-Amz-Target", "KinesisAnalytics_20150814.StartApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartApplication where
        toJSON StartApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  Core.Just ("InputConfigurations" Core..= inputConfigurations)])

instance Core.AWSRequest StartApplication where
        type Rs StartApplication = StartApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartApplicationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkStartApplicationResponse' smart constructor.
newtype StartApplicationResponse = StartApplicationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartApplicationResponse' value with any optional fields omitted.
mkStartApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartApplicationResponse
mkStartApplicationResponse responseStatus
  = StartApplicationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsResponseStatus :: Lens.Lens' StartApplicationResponse Core.Int
sarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
