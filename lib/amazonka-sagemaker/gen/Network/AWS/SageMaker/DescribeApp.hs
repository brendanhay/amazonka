{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeApp (..),
    mkDescribeApp,

    -- ** Request lenses
    daDomainId,
    daUserProfileName,
    daAppType,
    daAppName,

    -- * Destructuring the response
    DescribeAppResponse (..),
    mkDescribeAppResponse,

    -- ** Response lenses
    darrsAppArn,
    darrsAppName,
    darrsAppType,
    darrsCreationTime,
    darrsDomainId,
    darrsFailureReason,
    darrsLastHealthCheckTimestamp,
    darrsLastUserActivityTimestamp,
    darrsResourceSpec,
    darrsStatus,
    darrsUserProfileName,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeApp' smart constructor.
data DescribeApp = DescribeApp'
  { -- | The domain ID.
    domainId :: Types.DomainId,
    -- | The user profile name.
    userProfileName :: Types.UserProfileName,
    -- | The type of app.
    appType :: Types.AppType,
    -- | The name of the app.
    appName :: Types.AppName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApp' value with any optional fields omitted.
mkDescribeApp ::
  -- | 'domainId'
  Types.DomainId ->
  -- | 'userProfileName'
  Types.UserProfileName ->
  -- | 'appType'
  Types.AppType ->
  -- | 'appName'
  Types.AppName ->
  DescribeApp
mkDescribeApp domainId userProfileName appType appName =
  DescribeApp' {domainId, userProfileName, appType, appName}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDomainId :: Lens.Lens' DescribeApp Types.DomainId
daDomainId = Lens.field @"domainId"
{-# DEPRECATED daDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daUserProfileName :: Lens.Lens' DescribeApp Types.UserProfileName
daUserProfileName = Lens.field @"userProfileName"
{-# DEPRECATED daUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppType :: Lens.Lens' DescribeApp Types.AppType
daAppType = Lens.field @"appType"
{-# DEPRECATED daAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppName :: Lens.Lens' DescribeApp Types.AppName
daAppName = Lens.field @"appName"
{-# DEPRECATED daAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

instance Core.FromJSON DescribeApp where
  toJSON DescribeApp {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainId" Core..= domainId),
            Core.Just ("UserProfileName" Core..= userProfileName),
            Core.Just ("AppType" Core..= appType),
            Core.Just ("AppName" Core..= appName)
          ]
      )

instance Core.AWSRequest DescribeApp where
  type Rs DescribeApp = DescribeAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeApp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppResponse'
            Core.<$> (x Core..:? "AppArn")
            Core.<*> (x Core..:? "AppName")
            Core.<*> (x Core..:? "AppType")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "DomainId")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "LastHealthCheckTimestamp")
            Core.<*> (x Core..:? "LastUserActivityTimestamp")
            Core.<*> (x Core..:? "ResourceSpec")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "UserProfileName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAppResponse' smart constructor.
data DescribeAppResponse = DescribeAppResponse'
  { -- | The Amazon Resource Name (ARN) of the app.
    appArn :: Core.Maybe Types.AppArn,
    -- | The name of the app.
    appName :: Core.Maybe Types.AppName,
    -- | The type of app.
    appType :: Core.Maybe Types.AppType,
    -- | The creation time.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The domain ID.
    domainId :: Core.Maybe Types.DomainId,
    -- | The failure reason.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The timestamp of the last health check.
    lastHealthCheckTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The timestamp of the last user's activity.
    lastUserActivityTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
    resourceSpec :: Core.Maybe Types.ResourceSpec,
    -- | The status.
    status :: Core.Maybe Types.AppStatus,
    -- | The user profile name.
    userProfileName :: Core.Maybe Types.UserProfileName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAppResponse' value with any optional fields omitted.
mkDescribeAppResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAppResponse
mkDescribeAppResponse responseStatus =
  DescribeAppResponse'
    { appArn = Core.Nothing,
      appName = Core.Nothing,
      appType = Core.Nothing,
      creationTime = Core.Nothing,
      domainId = Core.Nothing,
      failureReason = Core.Nothing,
      lastHealthCheckTimestamp = Core.Nothing,
      lastUserActivityTimestamp = Core.Nothing,
      resourceSpec = Core.Nothing,
      status = Core.Nothing,
      userProfileName = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the app.
--
-- /Note:/ Consider using 'appArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAppArn :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.AppArn)
darrsAppArn = Lens.field @"appArn"
{-# DEPRECATED darrsAppArn "Use generic-lens or generic-optics with 'appArn' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAppName :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.AppName)
darrsAppName = Lens.field @"appName"
{-# DEPRECATED darrsAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAppType :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.AppType)
darrsAppType = Lens.field @"appType"
{-# DEPRECATED darrsAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsCreationTime :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.NominalDiffTime)
darrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED darrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsDomainId :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.DomainId)
darrsDomainId = Lens.field @"domainId"
{-# DEPRECATED darrsDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsFailureReason :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.FailureReason)
darrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED darrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The timestamp of the last health check.
--
-- /Note:/ Consider using 'lastHealthCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsLastHealthCheckTimestamp :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.NominalDiffTime)
darrsLastHealthCheckTimestamp = Lens.field @"lastHealthCheckTimestamp"
{-# DEPRECATED darrsLastHealthCheckTimestamp "Use generic-lens or generic-optics with 'lastHealthCheckTimestamp' instead." #-}

-- | The timestamp of the last user's activity.
--
-- /Note:/ Consider using 'lastUserActivityTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsLastUserActivityTimestamp :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.NominalDiffTime)
darrsLastUserActivityTimestamp = Lens.field @"lastUserActivityTimestamp"
{-# DEPRECATED darrsLastUserActivityTimestamp "Use generic-lens or generic-optics with 'lastUserActivityTimestamp' instead." #-}

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'resourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResourceSpec :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.ResourceSpec)
darrsResourceSpec = Lens.field @"resourceSpec"
{-# DEPRECATED darrsResourceSpec "Use generic-lens or generic-optics with 'resourceSpec' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsStatus :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.AppStatus)
darrsStatus = Lens.field @"status"
{-# DEPRECATED darrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsUserProfileName :: Lens.Lens' DescribeAppResponse (Core.Maybe Types.UserProfileName)
darrsUserProfileName = Lens.field @"userProfileName"
{-# DEPRECATED darrsUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAppResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
