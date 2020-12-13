{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerService
  ( -- * Creating a request
    DeleteContainerService (..),
    mkDeleteContainerService,

    -- ** Request lenses
    dcsServiceName,

    -- * Destructuring the response
    DeleteContainerServiceResponse (..),
    mkDeleteContainerServiceResponse,

    -- ** Response lenses
    dcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteContainerService' smart constructor.
newtype DeleteContainerService = DeleteContainerService'
  { -- | The name of the container service to delete.
    serviceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContainerService' with the minimum fields required to make a request.
--
-- * 'serviceName' - The name of the container service to delete.
mkDeleteContainerService ::
  -- | 'serviceName'
  Lude.Text ->
  DeleteContainerService
mkDeleteContainerService pServiceName_ =
  DeleteContainerService' {serviceName = pServiceName_}

-- | The name of the container service to delete.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsServiceName :: Lens.Lens' DeleteContainerService Lude.Text
dcsServiceName = Lens.lens (serviceName :: DeleteContainerService -> Lude.Text) (\s a -> s {serviceName = a} :: DeleteContainerService)
{-# DEPRECATED dcsServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest DeleteContainerService where
  type Rs DeleteContainerService = DeleteContainerServiceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteContainerServiceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteContainerService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteContainerService" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteContainerService where
  toJSON DeleteContainerService' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("serviceName" Lude..= serviceName)])

instance Lude.ToPath DeleteContainerService where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteContainerService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteContainerServiceResponse' smart constructor.
newtype DeleteContainerServiceResponse = DeleteContainerServiceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContainerServiceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteContainerServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteContainerServiceResponse
mkDeleteContainerServiceResponse pResponseStatus_ =
  DeleteContainerServiceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsResponseStatus :: Lens.Lens' DeleteContainerServiceResponse Lude.Int
dcsrsResponseStatus = Lens.lens (responseStatus :: DeleteContainerServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteContainerServiceResponse)
{-# DEPRECATED dcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
