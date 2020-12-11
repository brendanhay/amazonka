{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current state of the specified instance identifier. It tracks the instance while it is being created and returns an error status if applicable.
--
-- If an instance is not created successfully, the instance status reason field returns details relevant to the reason. The instance in a failed state is returned only for 24 hours after the CreateInstance API was invoked.
module Network.AWS.Connect.DescribeInstance
  ( -- * Creating a request
    DescribeInstance (..),
    mkDescribeInstance,

    -- ** Request lenses
    diInstanceId,

    -- * Destructuring the response
    DescribeInstanceResponse (..),
    mkDescribeInstanceResponse,

    -- ** Response lenses
    dirsInstance,
    dirsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstance' smart constructor.
newtype DescribeInstance = DescribeInstance'
  { instanceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkDescribeInstance ::
  -- | 'instanceId'
  Lude.Text ->
  DescribeInstance
mkDescribeInstance pInstanceId_ =
  DescribeInstance' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceId :: Lens.Lens' DescribeInstance Lude.Text
diInstanceId = Lens.lens (instanceId :: DescribeInstance -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstance)
{-# DEPRECATED diInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest DescribeInstance where
  type Rs DescribeInstance = DescribeInstanceResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstanceResponse'
            Lude.<$> (x Lude..?> "Instance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeInstance where
  toPath DescribeInstance' {..} =
    Lude.mconcat ["/instance/", Lude.toBS instanceId]

instance Lude.ToQuery DescribeInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInstanceResponse' smart constructor.
data DescribeInstanceResponse = DescribeInstanceResponse'
  { instance' ::
      Lude.Maybe Instance,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceResponse' with the minimum fields required to make a request.
--
-- * 'instance'' - The name of the instance.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceResponse
mkDescribeInstanceResponse pResponseStatus_ =
  DescribeInstanceResponse'
    { instance' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the instance.
--
-- /Note:/ Consider using 'instance'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsInstance :: Lens.Lens' DescribeInstanceResponse (Lude.Maybe Instance)
dirsInstance = Lens.lens (instance' :: DescribeInstanceResponse -> Lude.Maybe Instance) (\s a -> s {instance' = a} :: DescribeInstanceResponse)
{-# DEPRECATED dirsInstance "Use generic-lens or generic-optics with 'instance'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DescribeInstanceResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
