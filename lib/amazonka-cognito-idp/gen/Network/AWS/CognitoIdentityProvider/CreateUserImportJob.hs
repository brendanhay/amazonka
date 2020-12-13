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
    cuijUserPoolId,
    cuijJobName,
    cuijCloudWatchLogsRoleARN,

    -- * Destructuring the response
    CreateUserImportJobResponse (..),
    mkCreateUserImportJobResponse,

    -- ** Response lenses
    cuijrsUserImportJob,
    cuijrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to create the user import job.
--
-- /See:/ 'mkCreateUserImportJob' smart constructor.
data CreateUserImportJob = CreateUserImportJob'
  { -- | The user pool ID for the user pool that the users are being imported into.
    userPoolId :: Lude.Text,
    -- | The job name for the user import job.
    jobName :: Lude.Text,
    -- | The role ARN for the Amazon CloudWatch Logging role for the user import job.
    cloudWatchLogsRoleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserImportJob' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool that the users are being imported into.
-- * 'jobName' - The job name for the user import job.
-- * 'cloudWatchLogsRoleARN' - The role ARN for the Amazon CloudWatch Logging role for the user import job.
mkCreateUserImportJob ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'jobName'
  Lude.Text ->
  -- | 'cloudWatchLogsRoleARN'
  Lude.Text ->
  CreateUserImportJob
mkCreateUserImportJob
  pUserPoolId_
  pJobName_
  pCloudWatchLogsRoleARN_ =
    CreateUserImportJob'
      { userPoolId = pUserPoolId_,
        jobName = pJobName_,
        cloudWatchLogsRoleARN = pCloudWatchLogsRoleARN_
      }

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijUserPoolId :: Lens.Lens' CreateUserImportJob Lude.Text
cuijUserPoolId = Lens.lens (userPoolId :: CreateUserImportJob -> Lude.Text) (\s a -> s {userPoolId = a} :: CreateUserImportJob)
{-# DEPRECATED cuijUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The job name for the user import job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijJobName :: Lens.Lens' CreateUserImportJob Lude.Text
cuijJobName = Lens.lens (jobName :: CreateUserImportJob -> Lude.Text) (\s a -> s {jobName = a} :: CreateUserImportJob)
{-# DEPRECATED cuijJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The role ARN for the Amazon CloudWatch Logging role for the user import job.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijCloudWatchLogsRoleARN :: Lens.Lens' CreateUserImportJob Lude.Text
cuijCloudWatchLogsRoleARN = Lens.lens (cloudWatchLogsRoleARN :: CreateUserImportJob -> Lude.Text) (\s a -> s {cloudWatchLogsRoleARN = a} :: CreateUserImportJob)
{-# DEPRECATED cuijCloudWatchLogsRoleARN "Use generic-lens or generic-optics with 'cloudWatchLogsRoleARN' instead." #-}

instance Lude.AWSRequest CreateUserImportJob where
  type Rs CreateUserImportJob = CreateUserImportJobResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserImportJobResponse'
            Lude.<$> (x Lude..?> "UserImportJob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserImportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.CreateUserImportJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserImportJob where
  toJSON CreateUserImportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("JobName" Lude..= jobName),
            Lude.Just ("CloudWatchLogsRoleArn" Lude..= cloudWatchLogsRoleARN)
          ]
      )

instance Lude.ToPath CreateUserImportJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserImportJob where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to the request to create the user import job.
--
-- /See:/ 'mkCreateUserImportJobResponse' smart constructor.
data CreateUserImportJobResponse = CreateUserImportJobResponse'
  { -- | The job object that represents the user import job.
    userImportJob :: Lude.Maybe UserImportJobType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserImportJobResponse' with the minimum fields required to make a request.
--
-- * 'userImportJob' - The job object that represents the user import job.
-- * 'responseStatus' - The response status code.
mkCreateUserImportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserImportJobResponse
mkCreateUserImportJobResponse pResponseStatus_ =
  CreateUserImportJobResponse'
    { userImportJob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job object that represents the user import job.
--
-- /Note:/ Consider using 'userImportJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijrsUserImportJob :: Lens.Lens' CreateUserImportJobResponse (Lude.Maybe UserImportJobType)
cuijrsUserImportJob = Lens.lens (userImportJob :: CreateUserImportJobResponse -> Lude.Maybe UserImportJobType) (\s a -> s {userImportJob = a} :: CreateUserImportJobResponse)
{-# DEPRECATED cuijrsUserImportJob "Use generic-lens or generic-optics with 'userImportJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuijrsResponseStatus :: Lens.Lens' CreateUserImportJobResponse Lude.Int
cuijrsResponseStatus = Lens.lens (responseStatus :: CreateUserImportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserImportJobResponse)
{-# DEPRECATED cuijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
