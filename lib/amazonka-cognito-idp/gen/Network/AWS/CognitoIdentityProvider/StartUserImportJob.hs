{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.StartUserImportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the user import.
module Network.AWS.CognitoIdentityProvider.StartUserImportJob
    (
    -- * Creating a request
      StartUserImportJob (..)
    , mkStartUserImportJob
    -- ** Request lenses
    , suijUserPoolId
    , suijJobId

    -- * Destructuring the response
    , StartUserImportJobResponse (..)
    , mkStartUserImportJobResponse
    -- ** Response lenses
    , suijrrsUserImportJob
    , suijrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to start the user import job.
--
-- /See:/ 'mkStartUserImportJob' smart constructor.
data StartUserImportJob = StartUserImportJob'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool that the users are being imported into.
  , jobId :: Types.JobId
    -- ^ The job ID for the user import job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartUserImportJob' value with any optional fields omitted.
mkStartUserImportJob
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.JobId -- ^ 'jobId'
    -> StartUserImportJob
mkStartUserImportJob userPoolId jobId
  = StartUserImportJob'{userPoolId, jobId}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suijUserPoolId :: Lens.Lens' StartUserImportJob Types.UserPoolId
suijUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE suijUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The job ID for the user import job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suijJobId :: Lens.Lens' StartUserImportJob Types.JobId
suijJobId = Lens.field @"jobId"
{-# INLINEABLE suijJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery StartUserImportJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartUserImportJob where
        toHeaders StartUserImportJob{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.StartUserImportJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartUserImportJob where
        toJSON StartUserImportJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StartUserImportJob where
        type Rs StartUserImportJob = StartUserImportJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartUserImportJobResponse' Core.<$>
                   (x Core..:? "UserImportJob") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server to the request to start the user import job.
--
-- /See:/ 'mkStartUserImportJobResponse' smart constructor.
data StartUserImportJobResponse = StartUserImportJobResponse'
  { userImportJob :: Core.Maybe Types.UserImportJobType
    -- ^ The job object that represents the user import job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartUserImportJobResponse' value with any optional fields omitted.
mkStartUserImportJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartUserImportJobResponse
mkStartUserImportJobResponse responseStatus
  = StartUserImportJobResponse'{userImportJob = Core.Nothing,
                                responseStatus}

-- | The job object that represents the user import job.
--
-- /Note:/ Consider using 'userImportJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suijrrsUserImportJob :: Lens.Lens' StartUserImportJobResponse (Core.Maybe Types.UserImportJobType)
suijrrsUserImportJob = Lens.field @"userImportJob"
{-# INLINEABLE suijrrsUserImportJob #-}
{-# DEPRECATED userImportJob "Use generic-lens or generic-optics with 'userImportJob' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suijrrsResponseStatus :: Lens.Lens' StartUserImportJobResponse Core.Int
suijrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE suijrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
