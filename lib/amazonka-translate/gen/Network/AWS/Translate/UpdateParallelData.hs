{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.UpdateParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created parallel data resource by importing a new input file from Amazon S3.
module Network.AWS.Translate.UpdateParallelData
    (
    -- * Creating a request
      UpdateParallelData (..)
    , mkUpdateParallelData
    -- ** Request lenses
    , updName
    , updParallelDataConfig
    , updClientToken
    , updDescription

    -- * Destructuring the response
    , UpdateParallelDataResponse (..)
    , mkUpdateParallelDataResponse
    -- ** Response lenses
    , updrrsLatestUpdateAttemptAt
    , updrrsLatestUpdateAttemptStatus
    , updrrsName
    , updrrsStatus
    , updrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkUpdateParallelData' smart constructor.
data UpdateParallelData = UpdateParallelData'
  { name :: Types.ResourceName
    -- ^ The name of the parallel data resource being updated.
  , parallelDataConfig :: Types.ParallelDataConfig
    -- ^ Specifies the format and S3 location of the parallel data input file.
  , clientToken :: Types.ClientTokenString
    -- ^ A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
  , description :: Core.Maybe Types.Description
    -- ^ A custom description for the parallel data resource in Amazon Translate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateParallelData' value with any optional fields omitted.
mkUpdateParallelData
    :: Types.ResourceName -- ^ 'name'
    -> Types.ParallelDataConfig -- ^ 'parallelDataConfig'
    -> Types.ClientTokenString -- ^ 'clientToken'
    -> UpdateParallelData
mkUpdateParallelData name parallelDataConfig clientToken
  = UpdateParallelData'{name, parallelDataConfig, clientToken,
                        description = Core.Nothing}

-- | The name of the parallel data resource being updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updName :: Lens.Lens' UpdateParallelData Types.ResourceName
updName = Lens.field @"name"
{-# INLINEABLE updName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies the format and S3 location of the parallel data input file.
--
-- /Note:/ Consider using 'parallelDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updParallelDataConfig :: Lens.Lens' UpdateParallelData Types.ParallelDataConfig
updParallelDataConfig = Lens.field @"parallelDataConfig"
{-# INLINEABLE updParallelDataConfig #-}
{-# DEPRECATED parallelDataConfig "Use generic-lens or generic-optics with 'parallelDataConfig' instead"  #-}

-- | A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updClientToken :: Lens.Lens' UpdateParallelData Types.ClientTokenString
updClientToken = Lens.field @"clientToken"
{-# INLINEABLE updClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | A custom description for the parallel data resource in Amazon Translate.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDescription :: Lens.Lens' UpdateParallelData (Core.Maybe Types.Description)
updDescription = Lens.field @"description"
{-# INLINEABLE updDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery UpdateParallelData where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateParallelData where
        toHeaders UpdateParallelData{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSShineFrontendService_20170701.UpdateParallelData")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateParallelData where
        toJSON UpdateParallelData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ParallelDataConfig" Core..= parallelDataConfig),
                  Core.Just ("ClientToken" Core..= clientToken),
                  ("Description" Core..=) Core.<$> description])

instance Core.AWSRequest UpdateParallelData where
        type Rs UpdateParallelData = UpdateParallelDataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateParallelDataResponse' Core.<$>
                   (x Core..:? "LatestUpdateAttemptAt") Core.<*>
                     x Core..:? "LatestUpdateAttemptStatus"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateParallelDataResponse' smart constructor.
data UpdateParallelDataResponse = UpdateParallelDataResponse'
  { latestUpdateAttemptAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the most recent update was attempted.
  , latestUpdateAttemptStatus :: Core.Maybe Types.ParallelDataStatus
    -- ^ The status of the parallel data update attempt. When the updated parallel data resource is ready for you to use, the status is @ACTIVE@ .
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the parallel data resource being updated.
  , status :: Core.Maybe Types.ParallelDataStatus
    -- ^ The status of the parallel data resource that you are attempting to update. Your update request is accepted only if this status is either @ACTIVE@ or @FAILED@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateParallelDataResponse' value with any optional fields omitted.
mkUpdateParallelDataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateParallelDataResponse
mkUpdateParallelDataResponse responseStatus
  = UpdateParallelDataResponse'{latestUpdateAttemptAt = Core.Nothing,
                                latestUpdateAttemptStatus = Core.Nothing, name = Core.Nothing,
                                status = Core.Nothing, responseStatus}

-- | The time that the most recent update was attempted.
--
-- /Note:/ Consider using 'latestUpdateAttemptAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrrsLatestUpdateAttemptAt :: Lens.Lens' UpdateParallelDataResponse (Core.Maybe Core.NominalDiffTime)
updrrsLatestUpdateAttemptAt = Lens.field @"latestUpdateAttemptAt"
{-# INLINEABLE updrrsLatestUpdateAttemptAt #-}
{-# DEPRECATED latestUpdateAttemptAt "Use generic-lens or generic-optics with 'latestUpdateAttemptAt' instead"  #-}

-- | The status of the parallel data update attempt. When the updated parallel data resource is ready for you to use, the status is @ACTIVE@ .
--
-- /Note:/ Consider using 'latestUpdateAttemptStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrrsLatestUpdateAttemptStatus :: Lens.Lens' UpdateParallelDataResponse (Core.Maybe Types.ParallelDataStatus)
updrrsLatestUpdateAttemptStatus = Lens.field @"latestUpdateAttemptStatus"
{-# INLINEABLE updrrsLatestUpdateAttemptStatus #-}
{-# DEPRECATED latestUpdateAttemptStatus "Use generic-lens or generic-optics with 'latestUpdateAttemptStatus' instead"  #-}

-- | The name of the parallel data resource being updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrrsName :: Lens.Lens' UpdateParallelDataResponse (Core.Maybe Types.ResourceName)
updrrsName = Lens.field @"name"
{-# INLINEABLE updrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The status of the parallel data resource that you are attempting to update. Your update request is accepted only if this status is either @ACTIVE@ or @FAILED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrrsStatus :: Lens.Lens' UpdateParallelDataResponse (Core.Maybe Types.ParallelDataStatus)
updrrsStatus = Lens.field @"status"
{-# INLINEABLE updrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrrsResponseStatus :: Lens.Lens' UpdateParallelDataResponse Core.Int
updrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE updrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
