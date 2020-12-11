{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an application.
module Network.AWS.Pinpoint.DeleteApp
  ( -- * Creating a request
    DeleteApp (..),
    mkDeleteApp,

    -- ** Request lenses
    daApplicationId,

    -- * Destructuring the response
    DeleteAppResponse (..),
    mkDeleteAppResponse,

    -- ** Response lenses
    darsResponseStatus,
    darsApplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteApp' smart constructor.
newtype DeleteApp = DeleteApp' {applicationId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteApp ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteApp
mkDeleteApp pApplicationId_ =
  DeleteApp' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationId :: Lens.Lens' DeleteApp Lude.Text
daApplicationId = Lens.lens (applicationId :: DeleteApp -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteApp)
{-# DEPRECATED daApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAppResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteApp where
  toPath DeleteApp' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId]

instance Lude.ToQuery DeleteApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  { responseStatus ::
      Lude.Int,
    applicationResponse :: ApplicationResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
--
-- * 'applicationResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'applicationResponse'
  ApplicationResponse ->
  DeleteAppResponse
mkDeleteAppResponse pResponseStatus_ pApplicationResponse_ =
  DeleteAppResponse'
    { responseStatus = pResponseStatus_,
      applicationResponse = pApplicationResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DeleteAppResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DeleteAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAppResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsApplicationResponse :: Lens.Lens' DeleteAppResponse ApplicationResponse
darsApplicationResponse = Lens.lens (applicationResponse :: DeleteAppResponse -> ApplicationResponse) (\s a -> s {applicationResponse = a} :: DeleteAppResponse)
{-# DEPRECATED darsApplicationResponse "Use generic-lens or generic-optics with 'applicationResponse' instead." #-}
