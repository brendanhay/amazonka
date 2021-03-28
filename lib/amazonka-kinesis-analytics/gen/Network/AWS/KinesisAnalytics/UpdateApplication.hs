{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Kinesis Analytics application. Using this API, you can update application code, input configuration, and output configuration. 
--
-- Note that Amazon Kinesis Analytics updates the @CurrentApplicationVersionId@ each time you update your application. 
-- This operation requires permission for the @kinesisanalytics:UpdateApplication@ action.
module Network.AWS.KinesisAnalytics.UpdateApplication
    (
    -- * Creating a request
      UpdateApplication (..)
    , mkUpdateApplication
    -- ** Request lenses
    , uaApplicationName
    , uaCurrentApplicationVersionId
    , uaApplicationUpdate

    -- * Destructuring the response
    , UpdateApplicationResponse (..)
    , mkUpdateApplicationResponse
    -- ** Response lenses
    , uarrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { applicationName :: Types.ApplicationName
    -- ^ Name of the Amazon Kinesis Analytics application to update.
  , currentApplicationVersionId :: Core.Natural
    -- ^ The current application version ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
  , applicationUpdate :: Types.ApplicationUpdate
    -- ^ Describes application updates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplication' value with any optional fields omitted.
mkUpdateApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Core.Natural -- ^ 'currentApplicationVersionId'
    -> Types.ApplicationUpdate -- ^ 'applicationUpdate'
    -> UpdateApplication
mkUpdateApplication applicationName currentApplicationVersionId
  applicationUpdate
  = UpdateApplication'{applicationName, currentApplicationVersionId,
                       applicationUpdate}

-- | Name of the Amazon Kinesis Analytics application to update.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationName :: Lens.Lens' UpdateApplication Types.ApplicationName
uaApplicationName = Lens.field @"applicationName"
{-# INLINEABLE uaApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The current application version ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaCurrentApplicationVersionId :: Lens.Lens' UpdateApplication Core.Natural
uaCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# INLINEABLE uaCurrentApplicationVersionId #-}
{-# DEPRECATED currentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead"  #-}

-- | Describes application updates.
--
-- /Note:/ Consider using 'applicationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationUpdate :: Lens.Lens' UpdateApplication Types.ApplicationUpdate
uaApplicationUpdate = Lens.field @"applicationUpdate"
{-# INLINEABLE uaApplicationUpdate #-}
{-# DEPRECATED applicationUpdate "Use generic-lens or generic-optics with 'applicationUpdate' instead"  #-}

instance Core.ToQuery UpdateApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApplication where
        toHeaders UpdateApplication{..}
          = Core.pure
              ("X-Amz-Target", "KinesisAnalytics_20150814.UpdateApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApplication where
        toJSON UpdateApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  Core.Just
                    ("CurrentApplicationVersionId" Core..=
                       currentApplicationVersionId),
                  Core.Just ("ApplicationUpdate" Core..= applicationUpdate)])

instance Core.AWSRequest UpdateApplication where
        type Rs UpdateApplication = UpdateApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateApplicationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
newtype UpdateApplicationResponse = UpdateApplicationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResponse' value with any optional fields omitted.
mkUpdateApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateApplicationResponse
mkUpdateApplicationResponse responseStatus
  = UpdateApplicationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateApplicationResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
