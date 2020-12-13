{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartUserImportJob (..),
    mkStartUserImportJob,

    -- ** Request lenses
    suijJobId,
    suijUserPoolId,

    -- * Destructuring the response
    StartUserImportJobResponse (..),
    mkStartUserImportJobResponse,

    -- ** Response lenses
    suijrsUserImportJob,
    suijrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to start the user import job.
--
-- /See:/ 'mkStartUserImportJob' smart constructor.
data StartUserImportJob = StartUserImportJob'
  { -- | The job ID for the user import job.
    jobId :: Lude.Text,
    -- | The user pool ID for the user pool that the users are being imported into.
    userPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartUserImportJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID for the user import job.
-- * 'userPoolId' - The user pool ID for the user pool that the users are being imported into.
mkStartUserImportJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  StartUserImportJob
mkStartUserImportJob pJobId_ pUserPoolId_ =
  StartUserImportJob' {jobId = pJobId_, userPoolId = pUserPoolId_}

-- | The job ID for the user import job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suijJobId :: Lens.Lens' StartUserImportJob Lude.Text
suijJobId = Lens.lens (jobId :: StartUserImportJob -> Lude.Text) (\s a -> s {jobId = a} :: StartUserImportJob)
{-# DEPRECATED suijJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suijUserPoolId :: Lens.Lens' StartUserImportJob Lude.Text
suijUserPoolId = Lens.lens (userPoolId :: StartUserImportJob -> Lude.Text) (\s a -> s {userPoolId = a} :: StartUserImportJob)
{-# DEPRECATED suijUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest StartUserImportJob where
  type Rs StartUserImportJob = StartUserImportJobResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartUserImportJobResponse'
            Lude.<$> (x Lude..?> "UserImportJob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartUserImportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.StartUserImportJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartUserImportJob where
  toJSON StartUserImportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobId" Lude..= jobId),
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath StartUserImportJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartUserImportJob where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to the request to start the user import job.
--
-- /See:/ 'mkStartUserImportJobResponse' smart constructor.
data StartUserImportJobResponse = StartUserImportJobResponse'
  { -- | The job object that represents the user import job.
    userImportJob :: Lude.Maybe UserImportJobType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartUserImportJobResponse' with the minimum fields required to make a request.
--
-- * 'userImportJob' - The job object that represents the user import job.
-- * 'responseStatus' - The response status code.
mkStartUserImportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartUserImportJobResponse
mkStartUserImportJobResponse pResponseStatus_ =
  StartUserImportJobResponse'
    { userImportJob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job object that represents the user import job.
--
-- /Note:/ Consider using 'userImportJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suijrsUserImportJob :: Lens.Lens' StartUserImportJobResponse (Lude.Maybe UserImportJobType)
suijrsUserImportJob = Lens.lens (userImportJob :: StartUserImportJobResponse -> Lude.Maybe UserImportJobType) (\s a -> s {userImportJob = a} :: StartUserImportJobResponse)
{-# DEPRECATED suijrsUserImportJob "Use generic-lens or generic-optics with 'userImportJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suijrsResponseStatus :: Lens.Lens' StartUserImportJobResponse Lude.Int
suijrsResponseStatus = Lens.lens (responseStatus :: StartUserImportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartUserImportJobResponse)
{-# DEPRECATED suijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
