{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DescribeContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties of the requested container. This request is commonly used to retrieve the endpoint of a container. An endpoint is a value assigned by the service when a new container is created. A container's endpoint does not change after it has been assigned. The @DescribeContainer@ request returns a single @Container@ object based on @ContainerName@ . To return all @Container@ objects that are associated with a specified AWS account, use 'ListContainers' .
module Network.AWS.MediaStore.DescribeContainer
  ( -- * Creating a request
    DescribeContainer (..),
    mkDescribeContainer,

    -- ** Request lenses
    dContainerName,

    -- * Destructuring the response
    DescribeContainerResponse (..),
    mkDescribeContainerResponse,

    -- ** Response lenses
    drsContainer,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeContainer' smart constructor.
newtype DescribeContainer = DescribeContainer'
  { containerName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContainer' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container to query.
mkDescribeContainer ::
  DescribeContainer
mkDescribeContainer =
  DescribeContainer' {containerName = Lude.Nothing}

-- | The name of the container to query.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContainerName :: Lens.Lens' DescribeContainer (Lude.Maybe Lude.Text)
dContainerName = Lens.lens (containerName :: DescribeContainer -> Lude.Maybe Lude.Text) (\s a -> s {containerName = a} :: DescribeContainer)
{-# DEPRECATED dContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest DescribeContainer where
  type Rs DescribeContainer = DescribeContainerResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeContainerResponse'
            Lude.<$> (x Lude..?> "Container") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeContainer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.DescribeContainer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeContainer where
  toJSON DescribeContainer' {..} =
    Lude.object
      (Lude.catMaybes [("ContainerName" Lude..=) Lude.<$> containerName])

instance Lude.ToPath DescribeContainer where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeContainer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeContainerResponse' smart constructor.
data DescribeContainerResponse = DescribeContainerResponse'
  { container ::
      Lude.Maybe Container,
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

-- | Creates a value of 'DescribeContainerResponse' with the minimum fields required to make a request.
--
-- * 'container' - The name of the queried container.
-- * 'responseStatus' - The response status code.
mkDescribeContainerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeContainerResponse
mkDescribeContainerResponse pResponseStatus_ =
  DescribeContainerResponse'
    { container = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the queried container.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsContainer :: Lens.Lens' DescribeContainerResponse (Lude.Maybe Container)
drsContainer = Lens.lens (container :: DescribeContainerResponse -> Lude.Maybe Container) (\s a -> s {container = a} :: DescribeContainerResponse)
{-# DEPRECATED drsContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeContainerResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeContainerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeContainerResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
