{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeIdentityPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage details (for example, data storage) about a particular identity pool.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.DescribeIdentityPoolUsage
    (
    -- * Creating a request
      DescribeIdentityPoolUsage (..)
    , mkDescribeIdentityPoolUsage
    -- ** Request lenses
    , dipuIdentityPoolId

    -- * Destructuring the response
    , DescribeIdentityPoolUsageResponse (..)
    , mkDescribeIdentityPoolUsageResponse
    -- ** Response lenses
    , dipurrsIdentityPoolUsage
    , dipurrsResponseStatus
    ) where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for usage information about the identity pool.
--
-- /See:/ 'mkDescribeIdentityPoolUsage' smart constructor.
newtype DescribeIdentityPoolUsage = DescribeIdentityPoolUsage'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIdentityPoolUsage' value with any optional fields omitted.
mkDescribeIdentityPoolUsage
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> DescribeIdentityPoolUsage
mkDescribeIdentityPoolUsage identityPoolId
  = DescribeIdentityPoolUsage'{identityPoolId}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipuIdentityPoolId :: Lens.Lens' DescribeIdentityPoolUsage Types.IdentityPoolId
dipuIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE dipuIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

instance Core.ToQuery DescribeIdentityPoolUsage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeIdentityPoolUsage where
        toHeaders DescribeIdentityPoolUsage{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeIdentityPoolUsage where
        type Rs DescribeIdentityPoolUsage =
             DescribeIdentityPoolUsageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/identitypools/" Core.<> Core.toText identityPoolId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeIdentityPoolUsageResponse' Core.<$>
                   (x Core..:? "IdentityPoolUsage") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response to a successful DescribeIdentityPoolUsage request.
--
-- /See:/ 'mkDescribeIdentityPoolUsageResponse' smart constructor.
data DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse'
  { identityPoolUsage :: Core.Maybe Types.IdentityPoolUsage
    -- ^ Information about the usage of the identity pool.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeIdentityPoolUsageResponse' value with any optional fields omitted.
mkDescribeIdentityPoolUsageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeIdentityPoolUsageResponse
mkDescribeIdentityPoolUsageResponse responseStatus
  = DescribeIdentityPoolUsageResponse'{identityPoolUsage =
                                         Core.Nothing,
                                       responseStatus}

-- | Information about the usage of the identity pool.
--
-- /Note:/ Consider using 'identityPoolUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipurrsIdentityPoolUsage :: Lens.Lens' DescribeIdentityPoolUsageResponse (Core.Maybe Types.IdentityPoolUsage)
dipurrsIdentityPoolUsage = Lens.field @"identityPoolUsage"
{-# INLINEABLE dipurrsIdentityPoolUsage #-}
{-# DEPRECATED identityPoolUsage "Use generic-lens or generic-optics with 'identityPoolUsage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipurrsResponseStatus :: Lens.Lens' DescribeIdentityPoolUsageResponse Core.Int
dipurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dipurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
