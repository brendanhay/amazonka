{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.StopUserImportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the user import job.
module Network.AWS.CognitoIdentityProvider.StopUserImportJob
  ( -- * Creating a request
    StopUserImportJob (..),
    mkStopUserImportJob,

    -- ** Request lenses
    sUserPoolId,
    sJobId,

    -- * Destructuring the response
    StopUserImportJobResponse (..),
    mkStopUserImportJobResponse,

    -- ** Response lenses
    srsUserImportJob,
    srsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to stop the user import job.
--
-- /See:/ 'mkStopUserImportJob' smart constructor.
data StopUserImportJob = StopUserImportJob'
  { userPoolId ::
      Lude.Text,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopUserImportJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID for the user import job.
-- * 'userPoolId' - The user pool ID for the user pool that the users are being imported into.
mkStopUserImportJob ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'jobId'
  Lude.Text ->
  StopUserImportJob
mkStopUserImportJob pUserPoolId_ pJobId_ =
  StopUserImportJob' {userPoolId = pUserPoolId_, jobId = pJobId_}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUserPoolId :: Lens.Lens' StopUserImportJob Lude.Text
sUserPoolId = Lens.lens (userPoolId :: StopUserImportJob -> Lude.Text) (\s a -> s {userPoolId = a} :: StopUserImportJob)
{-# DEPRECATED sUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The job ID for the user import job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sJobId :: Lens.Lens' StopUserImportJob Lude.Text
sJobId = Lens.lens (jobId :: StopUserImportJob -> Lude.Text) (\s a -> s {jobId = a} :: StopUserImportJob)
{-# DEPRECATED sJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest StopUserImportJob where
  type Rs StopUserImportJob = StopUserImportJobResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopUserImportJobResponse'
            Lude.<$> (x Lude..?> "UserImportJob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopUserImportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.StopUserImportJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopUserImportJob where
  toJSON StopUserImportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("JobId" Lude..= jobId)
          ]
      )

instance Lude.ToPath StopUserImportJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopUserImportJob where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to the request to stop the user import job.
--
-- /See:/ 'mkStopUserImportJobResponse' smart constructor.
data StopUserImportJobResponse = StopUserImportJobResponse'
  { userImportJob ::
      Lude.Maybe UserImportJobType,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopUserImportJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'userImportJob' - The job object that represents the user import job.
mkStopUserImportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopUserImportJobResponse
mkStopUserImportJobResponse pResponseStatus_ =
  StopUserImportJobResponse'
    { userImportJob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job object that represents the user import job.
--
-- /Note:/ Consider using 'userImportJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsUserImportJob :: Lens.Lens' StopUserImportJobResponse (Lude.Maybe UserImportJobType)
srsUserImportJob = Lens.lens (userImportJob :: StopUserImportJobResponse -> Lude.Maybe UserImportJobType) (\s a -> s {userImportJob = a} :: StopUserImportJobResponse)
{-# DEPRECATED srsUserImportJob "Use generic-lens or generic-optics with 'userImportJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopUserImportJobResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopUserImportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopUserImportJobResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
