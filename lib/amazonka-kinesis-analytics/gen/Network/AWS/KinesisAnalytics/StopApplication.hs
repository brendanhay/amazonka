{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.StopApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the application from processing input data. You can stop an application only if it is in the running state. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the application state. After the application is stopped, Amazon Kinesis Analytics stops reading data from the input, the application stops processing data, and there is no output written to the destination. 
--
-- This operation requires permissions to perform the @kinesisanalytics:StopApplication@ action.
module Network.AWS.KinesisAnalytics.StopApplication
    (
    -- * Creating a request
      StopApplication (..)
    , mkStopApplication
    -- ** Request lenses
    , sApplicationName

    -- * Destructuring the response
    , StopApplicationResponse (..)
    , mkStopApplicationResponse
    -- ** Response lenses
    , srsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkStopApplication' smart constructor.
newtype StopApplication = StopApplication'
  { applicationName :: Types.ApplicationName
    -- ^ Name of the running application to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopApplication' value with any optional fields omitted.
mkStopApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> StopApplication
mkStopApplication applicationName
  = StopApplication'{applicationName}

-- | Name of the running application to stop.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplicationName :: Lens.Lens' StopApplication Types.ApplicationName
sApplicationName = Lens.field @"applicationName"
{-# INLINEABLE sApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

instance Core.ToQuery StopApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopApplication where
        toHeaders StopApplication{..}
          = Core.pure
              ("X-Amz-Target", "KinesisAnalytics_20150814.StopApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopApplication where
        toJSON StopApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName)])

instance Core.AWSRequest StopApplication where
        type Rs StopApplication = StopApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopApplicationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkStopApplicationResponse' smart constructor.
newtype StopApplicationResponse = StopApplicationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopApplicationResponse' value with any optional fields omitted.
mkStopApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopApplicationResponse
mkStopApplicationResponse responseStatus
  = StopApplicationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopApplicationResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
