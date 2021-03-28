{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user import job.
module Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
    (
    -- * Creating a request
      DescribeUserImportJob (..)
    , mkDescribeUserImportJob
    -- ** Request lenses
    , duijUserPoolId
    , duijJobId

    -- * Destructuring the response
    , DescribeUserImportJobResponse (..)
    , mkDescribeUserImportJobResponse
    -- ** Response lenses
    , duijrrsUserImportJob
    , duijrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe the user import job.
--
-- /See:/ 'mkDescribeUserImportJob' smart constructor.
data DescribeUserImportJob = DescribeUserImportJob'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool that the users are being imported into.
  , jobId :: Types.JobId
    -- ^ The job ID for the user import job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserImportJob' value with any optional fields omitted.
mkDescribeUserImportJob
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.JobId -- ^ 'jobId'
    -> DescribeUserImportJob
mkDescribeUserImportJob userPoolId jobId
  = DescribeUserImportJob'{userPoolId, jobId}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duijUserPoolId :: Lens.Lens' DescribeUserImportJob Types.UserPoolId
duijUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE duijUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The job ID for the user import job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duijJobId :: Lens.Lens' DescribeUserImportJob Types.JobId
duijJobId = Lens.field @"jobId"
{-# INLINEABLE duijJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeUserImportJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeUserImportJob where
        toHeaders DescribeUserImportJob{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DescribeUserImportJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeUserImportJob where
        toJSON DescribeUserImportJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeUserImportJob where
        type Rs DescribeUserImportJob = DescribeUserImportJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeUserImportJobResponse' Core.<$>
                   (x Core..:? "UserImportJob") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server to the request to describe the user import job.
--
-- /See:/ 'mkDescribeUserImportJobResponse' smart constructor.
data DescribeUserImportJobResponse = DescribeUserImportJobResponse'
  { userImportJob :: Core.Maybe Types.UserImportJobType
    -- ^ The job object that represents the user import job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUserImportJobResponse' value with any optional fields omitted.
mkDescribeUserImportJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUserImportJobResponse
mkDescribeUserImportJobResponse responseStatus
  = DescribeUserImportJobResponse'{userImportJob = Core.Nothing,
                                   responseStatus}

-- | The job object that represents the user import job.
--
-- /Note:/ Consider using 'userImportJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duijrrsUserImportJob :: Lens.Lens' DescribeUserImportJobResponse (Core.Maybe Types.UserImportJobType)
duijrrsUserImportJob = Lens.field @"userImportJob"
{-# INLINEABLE duijrrsUserImportJob #-}
{-# DEPRECATED userImportJob "Use generic-lens or generic-optics with 'userImportJob' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duijrrsResponseStatus :: Lens.Lens' DescribeUserImportJobResponse Core.Int
duijrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE duijrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
