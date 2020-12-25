{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeIdentityUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage information for an identity, including number of datasets and data usage.
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.
module Network.AWS.CognitoSync.DescribeIdentityUsage
  ( -- * Creating a request
    DescribeIdentityUsage (..),
    mkDescribeIdentityUsage,

    -- ** Request lenses
    diuIdentityPoolId,
    diuIdentityId,

    -- * Destructuring the response
    DescribeIdentityUsageResponse (..),
    mkDescribeIdentityUsageResponse,

    -- ** Response lenses
    diurrsIdentityUsage,
    diurrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for information about the usage of an identity pool.
--
-- /See:/ 'mkDescribeIdentityUsage' smart constructor.
data DescribeIdentityUsage = DescribeIdentityUsage'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Types.IdentityPoolId,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Types.IdentityId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIdentityUsage' value with any optional fields omitted.
mkDescribeIdentityUsage ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  -- | 'identityId'
  Types.IdentityId ->
  DescribeIdentityUsage
mkDescribeIdentityUsage identityPoolId identityId =
  DescribeIdentityUsage' {identityPoolId, identityId}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diuIdentityPoolId :: Lens.Lens' DescribeIdentityUsage Types.IdentityPoolId
diuIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED diuIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diuIdentityId :: Lens.Lens' DescribeIdentityUsage Types.IdentityId
diuIdentityId = Lens.field @"identityId"
{-# DEPRECATED diuIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Core.AWSRequest DescribeIdentityUsage where
  type Rs DescribeIdentityUsage = DescribeIdentityUsageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/identities/")
                Core.<> (Core.toText identityId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityUsageResponse'
            Core.<$> (x Core..:? "IdentityUsage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response to a successful DescribeIdentityUsage request.
--
-- /See:/ 'mkDescribeIdentityUsageResponse' smart constructor.
data DescribeIdentityUsageResponse = DescribeIdentityUsageResponse'
  { -- | Usage information for the identity.
    identityUsage :: Core.Maybe Types.IdentityUsage,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeIdentityUsageResponse' value with any optional fields omitted.
mkDescribeIdentityUsageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeIdentityUsageResponse
mkDescribeIdentityUsageResponse responseStatus =
  DescribeIdentityUsageResponse'
    { identityUsage = Core.Nothing,
      responseStatus
    }

-- | Usage information for the identity.
--
-- /Note:/ Consider using 'identityUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diurrsIdentityUsage :: Lens.Lens' DescribeIdentityUsageResponse (Core.Maybe Types.IdentityUsage)
diurrsIdentityUsage = Lens.field @"identityUsage"
{-# DEPRECATED diurrsIdentityUsage "Use generic-lens or generic-optics with 'identityUsage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diurrsResponseStatus :: Lens.Lens' DescribeIdentityUsageResponse Core.Int
diurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
