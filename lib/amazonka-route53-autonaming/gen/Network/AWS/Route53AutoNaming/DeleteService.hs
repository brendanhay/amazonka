{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.DeleteService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service. If the service still contains one or more registered instances, the request fails.
module Network.AWS.Route53AutoNaming.DeleteService
  ( -- * Creating a request
    DeleteService (..),
    mkDeleteService,

    -- ** Request lenses
    dsId,

    -- * Destructuring the response
    DeleteServiceResponse (..),
    mkDeleteServiceResponse,

    -- ** Response lenses
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkDeleteService' smart constructor.
newtype DeleteService = DeleteService'
  { -- | The ID of the service that you want to delete.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteService' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the service that you want to delete.
mkDeleteService ::
  -- | 'id'
  Lude.Text ->
  DeleteService
mkDeleteService pId_ = DeleteService' {id = pId_}

-- | The ID of the service that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsId :: Lens.Lens' DeleteService Lude.Text
dsId = Lens.lens (id :: DeleteService -> Lude.Text) (\s a -> s {id = a} :: DeleteService)
{-# DEPRECATED dsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteService where
  type Rs DeleteService = DeleteServiceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteServiceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.DeleteService" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteService where
  toJSON DeleteService' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Id" Lude..= id)])

instance Lude.ToPath DeleteService where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteServiceResponse' smart constructor.
newtype DeleteServiceResponse = DeleteServiceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServiceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteServiceResponse
mkDeleteServiceResponse pResponseStatus_ =
  DeleteServiceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteServiceResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteServiceResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
