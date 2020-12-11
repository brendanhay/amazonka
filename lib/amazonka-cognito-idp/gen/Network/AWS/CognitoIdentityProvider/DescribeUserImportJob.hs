{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeUserImportJob (..),
    mkDescribeUserImportJob,

    -- ** Request lenses
    duijUserPoolId,
    duijJobId,

    -- * Destructuring the response
    DescribeUserImportJobResponse (..),
    mkDescribeUserImportJobResponse,

    -- ** Response lenses
    duijrsUserImportJob,
    duijrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to describe the user import job.
--
-- /See:/ 'mkDescribeUserImportJob' smart constructor.
data DescribeUserImportJob = DescribeUserImportJob'
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

-- | Creates a value of 'DescribeUserImportJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID for the user import job.
-- * 'userPoolId' - The user pool ID for the user pool that the users are being imported into.
mkDescribeUserImportJob ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'jobId'
  Lude.Text ->
  DescribeUserImportJob
mkDescribeUserImportJob pUserPoolId_ pJobId_ =
  DescribeUserImportJob'
    { userPoolId = pUserPoolId_,
      jobId = pJobId_
    }

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duijUserPoolId :: Lens.Lens' DescribeUserImportJob Lude.Text
duijUserPoolId = Lens.lens (userPoolId :: DescribeUserImportJob -> Lude.Text) (\s a -> s {userPoolId = a} :: DescribeUserImportJob)
{-# DEPRECATED duijUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The job ID for the user import job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duijJobId :: Lens.Lens' DescribeUserImportJob Lude.Text
duijJobId = Lens.lens (jobId :: DescribeUserImportJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeUserImportJob)
{-# DEPRECATED duijJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeUserImportJob where
  type Rs DescribeUserImportJob = DescribeUserImportJobResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserImportJobResponse'
            Lude.<$> (x Lude..?> "UserImportJob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserImportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DescribeUserImportJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUserImportJob where
  toJSON DescribeUserImportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("JobId" Lude..= jobId)
          ]
      )

instance Lude.ToPath DescribeUserImportJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserImportJob where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to the request to describe the user import job.
--
-- /See:/ 'mkDescribeUserImportJobResponse' smart constructor.
data DescribeUserImportJobResponse = DescribeUserImportJobResponse'
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

-- | Creates a value of 'DescribeUserImportJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'userImportJob' - The job object that represents the user import job.
mkDescribeUserImportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserImportJobResponse
mkDescribeUserImportJobResponse pResponseStatus_ =
  DescribeUserImportJobResponse'
    { userImportJob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job object that represents the user import job.
--
-- /Note:/ Consider using 'userImportJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duijrsUserImportJob :: Lens.Lens' DescribeUserImportJobResponse (Lude.Maybe UserImportJobType)
duijrsUserImportJob = Lens.lens (userImportJob :: DescribeUserImportJobResponse -> Lude.Maybe UserImportJobType) (\s a -> s {userImportJob = a} :: DescribeUserImportJobResponse)
{-# DEPRECATED duijrsUserImportJob "Use generic-lens or generic-optics with 'userImportJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duijrsResponseStatus :: Lens.Lens' DescribeUserImportJobResponse Lude.Int
duijrsResponseStatus = Lens.lens (responseStatus :: DescribeUserImportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserImportJobResponse)
{-# DEPRECATED duijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
