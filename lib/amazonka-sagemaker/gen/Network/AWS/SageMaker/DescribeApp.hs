{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the app.
module Network.AWS.SageMaker.DescribeApp
    (
    -- * Creating a request
      DescribeApp (..)
    , mkDescribeApp
    -- ** Request lenses
    , daDomainId
    , daUserProfileName
    , daAppType
    , daAppName

    -- * Destructuring the response
    , DescribeAppResponse (..)
    , mkDescribeAppResponse
    -- ** Response lenses
    , darrsAppArn
    , darrsAppName
    , darrsAppType
    , darrsCreationTime
    , darrsDomainId
    , darrsFailureReason
    , darrsLastHealthCheckTimestamp
    , darrsLastUserActivityTimestamp
    , darrsResourceSpec
    , darrsStatus
    , darrsUserProfileName
    , darrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeApp' smart constructor.
data DescribeApp = DescribeApp'
  { domainId :: Types.DomainId
    -- ^ The domain ID.
  , userProfileName :: Types.UserProfileName
    -- ^ The user profile name.
  , appType :: Types.AppType
    -- ^ The type of app.
  , appName :: Types.AppName
    -- ^ The name of the app.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApp' value with any optional fields omitted.
mkDescribeApp
    :: Types.DomainId -- ^ 'domainId'
    -> Types.UserProfileName -- ^ 'userProfileName'
    -> Types.AppType -- ^ 'appType'
    -> Types.AppName -- ^ 'appName'
    -> DescribeApp
mkDescribeApp domainId userProfileName appType appName
  = DescribeApp'{domainId, userProfileName, appType, appName}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDomainId :: Lens.Lens' DescribeApp Types.DomainId
daDomainId = Lens.field @"domainId"
{-# INLINEABLE daDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daUserProfileName :: Lens.Lens' DescribeApp Types.UserProfileName
daUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE daUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppType :: Lens.Lens' DescribeApp Types.AppType
daAppType = Lens.field @"appType"
{-# INLINEABLE daAppType #-}
{-# DEPRECATED appType "Use generic-lens or generic-optics with 'appType' instead"  #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppName :: Lens.Lens' DescribeApp Types.AppName
daAppName = Lens.field @"appName"
{-# INLINEABLE daAppName #-}
{-# DEPRECATED appName "Use generic-lens or generic-optics with 'appName' instead"  #-}

instance Core.ToQuery DescribeApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeApp where
        toHeaders DescribeApp{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeApp") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeApp where
        toJSON DescribeApp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  Core.Just ("UserProfileName" Core..= userProfileName),
                  Core.Just ("AppType" Core..= appType),
                  Core.Just ("AppName" Core..= appName)])

instance Core.AWSRequest DescribeApp where
        type Rs DescribeApp = DescribeAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAppResponse' Core.<$>
                   (x Core..:? "AppArn") Core.<*> x Core..:? "AppName" Core.<*>
                     x Core..:? "AppType"
                     Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "DomainId"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "LastHealthCheckTimestamp"
                     Core.<*> x Core..:? "LastUserActivityTimestamp"
                     Core.<*> x Core..:? "ResourceSpec"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "UserProfileName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAppResponse' smart constructor.
data DescribeAppResponse = DescribeAppResponse'
  { appArn :: Core.Maybe Types.AppArn
    -- ^ The Amazon Resource Name (ARN) of the app.
  , appName :: Core.Maybe Types.AppName
    -- ^ The name of the app.
  , appType :: Core.Maybe Types.AppType
    -- ^ The type of app.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation time.
  , domainId :: Core.Maybe Types.DomainId
    -- ^ The domain ID.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ The failure reason.
  , lastHealthCheckTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp of the last health check.
  , lastUserActivityTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp of the last user's activity.
  , resourceSpec :: Core.Maybe Types.ResourceSpec
    -- ^ The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
  , status :: Core.Maybe Types.AppStatus
    -- ^ The status.
  , userProfileName :: Core.Maybe Types.UserProfileName
    -- ^ The user profile name.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAppResponse' value with any optional fields omitted.
mkDescribeAppResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAppResponse
mkDescribeAppResponse responseStatus
  = DescribeAppResponse'{appArn = Core.Nothing,
                         appName = Core.Nothing, appType = Core.Nothing,
                         creationTime = Core.Nothing, domainId = Core.Nothing,
                         failureReason = Core.Nothing,
                         lastHealthCheckTimestamp = Core.Nothing,
                         lastUserActivityTimestamp = Core.Nothing,
                         resourceSpec = Core.Nothing, status = Core.Nothing,
                         userProfileName = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the app.
--
-- /Note:/ Consider using 'appArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAppArn :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.AppArn)
darrsAppArn = Lens.field @"appArn"
{-# INLINEABLE darrsAppArn #-}
{-# DEPRECATED appArn "Use generic-lens or generic-optics with 'appArn' instead"  #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAppName :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.AppName)
darrsAppName = Lens.field @"appName"
{-# INLINEABLE darrsAppName #-}
{-# DEPRECATED appName "Use generic-lens or generic-optics with 'appName' instead"  #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAppType :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.AppType)
darrsAppType = Lens.field @"appType"
{-# INLINEABLE darrsAppType #-}
{-# DEPRECATED appType "Use generic-lens or generic-optics with 'appType' instead"  #-}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsCreationTime :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.NominalDiffTime)
darrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE darrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsDomainId :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.DomainId)
darrsDomainId = Lens.field @"domainId"
{-# INLINEABLE darrsDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsFailureReason :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.FailureReason)
darrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE darrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The timestamp of the last health check.
--
-- /Note:/ Consider using 'lastHealthCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsLastHealthCheckTimestamp :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.NominalDiffTime)
darrsLastHealthCheckTimestamp = Lens.field @"lastHealthCheckTimestamp"
{-# INLINEABLE darrsLastHealthCheckTimestamp #-}
{-# DEPRECATED lastHealthCheckTimestamp "Use generic-lens or generic-optics with 'lastHealthCheckTimestamp' instead"  #-}

-- | The timestamp of the last user's activity.
--
-- /Note:/ Consider using 'lastUserActivityTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsLastUserActivityTimestamp :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.NominalDiffTime)
darrsLastUserActivityTimestamp = Lens.field @"lastUserActivityTimestamp"
{-# INLINEABLE darrsLastUserActivityTimestamp #-}
{-# DEPRECATED lastUserActivityTimestamp "Use generic-lens or generic-optics with 'lastUserActivityTimestamp' instead"  #-}

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'resourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResourceSpec :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.ResourceSpec)
darrsResourceSpec = Lens.field @"resourceSpec"
{-# INLINEABLE darrsResourceSpec #-}
{-# DEPRECATED resourceSpec "Use generic-lens or generic-optics with 'resourceSpec' instead"  #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsStatus :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.AppStatus)
darrsStatus = Lens.field @"status"
{-# INLINEABLE darrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsUserProfileName :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.UserProfileName)
darrsUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE darrsUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAppResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
