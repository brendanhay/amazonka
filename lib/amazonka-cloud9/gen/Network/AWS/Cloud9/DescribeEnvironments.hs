{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DescribeEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about AWS Cloud9 development environments.
module Network.AWS.Cloud9.DescribeEnvironments
  ( -- * Creating a request
    DescribeEnvironments (..),
    mkDescribeEnvironments,

    -- ** Request lenses
    deEnvironmentIds,

    -- * Destructuring the response
    DescribeEnvironmentsResponse (..),
    mkDescribeEnvironmentsResponse,

    -- ** Response lenses
    deersEnvironments,
    deersResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEnvironments' smart constructor.
newtype DescribeEnvironments = DescribeEnvironments'
  { environmentIds ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironments' with the minimum fields required to make a request.
--
-- * 'environmentIds' - The IDs of individual environments to get information about.
mkDescribeEnvironments ::
  -- | 'environmentIds'
  Lude.NonEmpty Lude.Text ->
  DescribeEnvironments
mkDescribeEnvironments pEnvironmentIds_ =
  DescribeEnvironments' {environmentIds = pEnvironmentIds_}

-- | The IDs of individual environments to get information about.
--
-- /Note:/ Consider using 'environmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentIds :: Lens.Lens' DescribeEnvironments (Lude.NonEmpty Lude.Text)
deEnvironmentIds = Lens.lens (environmentIds :: DescribeEnvironments -> Lude.NonEmpty Lude.Text) (\s a -> s {environmentIds = a} :: DescribeEnvironments)
{-# DEPRECATED deEnvironmentIds "Use generic-lens or generic-optics with 'environmentIds' instead." #-}

instance Lude.AWSRequest DescribeEnvironments where
  type Rs DescribeEnvironments = DescribeEnvironmentsResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEnvironmentsResponse'
            Lude.<$> (x Lude..?> "environments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEnvironments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironments" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEnvironments where
  toJSON DescribeEnvironments' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("environmentIds" Lude..= environmentIds)]
      )

instance Lude.ToPath DescribeEnvironments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEnvironments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEnvironmentsResponse' smart constructor.
data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'
  { environments ::
      Lude.Maybe [Environment],
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentsResponse' with the minimum fields required to make a request.
--
-- * 'environments' - Information about the environments that are returned.
-- * 'responseStatus' - The response status code.
mkDescribeEnvironmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEnvironmentsResponse
mkDescribeEnvironmentsResponse pResponseStatus_ =
  DescribeEnvironmentsResponse'
    { environments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the environments that are returned.
--
-- /Note:/ Consider using 'environments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersEnvironments :: Lens.Lens' DescribeEnvironmentsResponse (Lude.Maybe [Environment])
deersEnvironments = Lens.lens (environments :: DescribeEnvironmentsResponse -> Lude.Maybe [Environment]) (\s a -> s {environments = a} :: DescribeEnvironmentsResponse)
{-# DEPRECATED deersEnvironments "Use generic-lens or generic-optics with 'environments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersResponseStatus :: Lens.Lens' DescribeEnvironmentsResponse Lude.Int
deersResponseStatus = Lens.lens (responseStatus :: DescribeEnvironmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEnvironmentsResponse)
{-# DEPRECATED deersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
