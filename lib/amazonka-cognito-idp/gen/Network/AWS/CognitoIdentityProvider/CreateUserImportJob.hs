{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserImportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the user import job.
module Network.AWS.CognitoIdentityProvider.CreateUserImportJob
  ( -- * Creating a request
    CreateUserImportJob (..),
    mkCreateUserImportJob,

    -- ** Request lenses
    cuijJobName,
    cuijUserPoolId,
    cuijCloudWatchLogsRoleArn,

    -- * Destructuring the response
    CreateUserImportJobResponse (..),
    mkCreateUserImportJobResponse,

    -- ** Response lenses
    cuijrrsUserImportJob,
    cuijrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to create the user import job.
--
-- /See:/ 'mkCreateUserImportJob' smart constructor.
data CreateUserImportJob = CreateUserImportJob'
  { -- | The job name for the user import job.
    jobName :: Types.UserImportJobNameType,
    -- | The user pool ID for the user pool that the users are being imported into.
    userPoolId :: Types.UserPoolId,
    -- | The role ARN for the Amazon CloudWatch Logging role for the user import job.
    cloudWatchLogsRoleArn :: Types.CloudWatchLogsRoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserImportJob' value with any optional fields omitted.
mkCreateUserImportJob ::
  -- | 'jobName'
  Types.UserImportJobNameType ->
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'cloudWatchLogsRoleArn'
  Types.CloudWatchLogsRoleArn ->
  CreateUserImportJob
mkCreateUserImportJob jobName userPoolId cloudWatchLogsRoleArn =
  CreateUserImportJob' {jobName, userPoolId, cloudWatchLogsRoleArn}

-- | The job name for the user import job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijJobName :: Lens.Lens' CreateUserImportJob Types.UserImportJobNameType
cuijJobName = Lens.field @"jobName"
{-# DEPRECATED cuijJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijUserPoolId :: Lens.Lens' CreateUserImportJob Types.UserPoolId
cuijUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED cuijUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The role ARN for the Amazon CloudWatch Logging role for the user import job.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijCloudWatchLogsRoleArn :: Lens.Lens' CreateUserImportJob Types.CloudWatchLogsRoleArn
cuijCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# DEPRECATED cuijCloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead." #-}

instance Core.FromJSON CreateUserImportJob where
  toJSON CreateUserImportJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("CloudWatchLogsRoleArn" Core..= cloudWatchLogsRoleArn)
          ]
      )

instance Core.AWSRequest CreateUserImportJob where
  type Rs CreateUserImportJob = CreateUserImportJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.CreateUserImportJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserImportJobResponse'
            Core.<$> (x Core..:? "UserImportJob")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server to the request to create the user import job.
--
-- /See:/ 'mkCreateUserImportJobResponse' smart constructor.
data CreateUserImportJobResponse = CreateUserImportJobResponse'
  { -- | The job object that represents the user import job.
    userImportJob :: Core.Maybe Types.UserImportJobType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateUserImportJobResponse' value with any optional fields omitted.
mkCreateUserImportJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateUserImportJobResponse
mkCreateUserImportJobResponse responseStatus =
  CreateUserImportJobResponse'
    { userImportJob = Core.Nothing,
      responseStatus
    }

-- | The job object that represents the user import job.
--
-- /Note:/ Consider using 'userImportJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijrrsUserImportJob :: Lens.Lens' CreateUserImportJobResponse (Core.Maybe Types.UserImportJobType)
cuijrrsUserImportJob = Lens.field @"userImportJob"
{-# DEPRECATED cuijrrsUserImportJob "Use generic-lens or generic-optics with 'userImportJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijrrsResponseStatus :: Lens.Lens' CreateUserImportJobResponse Core.Int
cuijrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cuijrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
