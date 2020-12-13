{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DescribeEnvironmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status information for an AWS Cloud9 development environment.
module Network.AWS.Cloud9.DescribeEnvironmentStatus
  ( -- * Creating a request
    DescribeEnvironmentStatus (..),
    mkDescribeEnvironmentStatus,

    -- ** Request lenses
    desEnvironmentId,

    -- * Destructuring the response
    DescribeEnvironmentStatusResponse (..),
    mkDescribeEnvironmentStatusResponse,

    -- ** Response lenses
    desrsStatus,
    desrsMessage,
    desrsResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEnvironmentStatus' smart constructor.
newtype DescribeEnvironmentStatus = DescribeEnvironmentStatus'
  { -- | The ID of the environment to get status information about.
    environmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentStatus' with the minimum fields required to make a request.
--
-- * 'environmentId' - The ID of the environment to get status information about.
mkDescribeEnvironmentStatus ::
  -- | 'environmentId'
  Lude.Text ->
  DescribeEnvironmentStatus
mkDescribeEnvironmentStatus pEnvironmentId_ =
  DescribeEnvironmentStatus' {environmentId = pEnvironmentId_}

-- | The ID of the environment to get status information about.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentId :: Lens.Lens' DescribeEnvironmentStatus Lude.Text
desEnvironmentId = Lens.lens (environmentId :: DescribeEnvironmentStatus -> Lude.Text) (\s a -> s {environmentId = a} :: DescribeEnvironmentStatus)
{-# DEPRECATED desEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest DescribeEnvironmentStatus where
  type
    Rs DescribeEnvironmentStatus =
      DescribeEnvironmentStatusResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEnvironmentStatusResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "message")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEnvironmentStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironmentStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEnvironmentStatus where
  toJSON DescribeEnvironmentStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("environmentId" Lude..= environmentId)]
      )

instance Lude.ToPath DescribeEnvironmentStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEnvironmentStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEnvironmentStatusResponse' smart constructor.
data DescribeEnvironmentStatusResponse = DescribeEnvironmentStatusResponse'
  { -- | The status of the environment. Available values include:
    --
    --
    --     * @connecting@ : The environment is connecting.
    --
    --
    --     * @creating@ : The environment is being created.
    --
    --
    --     * @deleting@ : The environment is being deleted.
    --
    --
    --     * @error@ : The environment is in an error state.
    --
    --
    --     * @ready@ : The environment is ready.
    --
    --
    --     * @stopped@ : The environment is stopped.
    --
    --
    --     * @stopping@ : The environment is stopping.
    status :: Lude.Maybe EnvironmentStatus,
    -- | Any informational message about the status of the environment.
    message :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentStatusResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the environment. Available values include:
--
--
--     * @connecting@ : The environment is connecting.
--
--
--     * @creating@ : The environment is being created.
--
--
--     * @deleting@ : The environment is being deleted.
--
--
--     * @error@ : The environment is in an error state.
--
--
--     * @ready@ : The environment is ready.
--
--
--     * @stopped@ : The environment is stopped.
--
--
--     * @stopping@ : The environment is stopping.
--
--
-- * 'message' - Any informational message about the status of the environment.
-- * 'responseStatus' - The response status code.
mkDescribeEnvironmentStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEnvironmentStatusResponse
mkDescribeEnvironmentStatusResponse pResponseStatus_ =
  DescribeEnvironmentStatusResponse'
    { status = Lude.Nothing,
      message = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the environment. Available values include:
--
--
--     * @connecting@ : The environment is connecting.
--
--
--     * @creating@ : The environment is being created.
--
--
--     * @deleting@ : The environment is being deleted.
--
--
--     * @error@ : The environment is in an error state.
--
--
--     * @ready@ : The environment is ready.
--
--
--     * @stopped@ : The environment is stopped.
--
--
--     * @stopping@ : The environment is stopping.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsStatus :: Lens.Lens' DescribeEnvironmentStatusResponse (Lude.Maybe EnvironmentStatus)
desrsStatus = Lens.lens (status :: DescribeEnvironmentStatusResponse -> Lude.Maybe EnvironmentStatus) (\s a -> s {status = a} :: DescribeEnvironmentStatusResponse)
{-# DEPRECATED desrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Any informational message about the status of the environment.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsMessage :: Lens.Lens' DescribeEnvironmentStatusResponse (Lude.Maybe Lude.Text)
desrsMessage = Lens.lens (message :: DescribeEnvironmentStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DescribeEnvironmentStatusResponse)
{-# DEPRECATED desrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeEnvironmentStatusResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeEnvironmentStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEnvironmentStatusResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
