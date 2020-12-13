{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific operation. Operations include events such as when you create an instance, allocate a static IP, attach a static IP, and so on.
module Network.AWS.Lightsail.GetOperation
  ( -- * Creating a request
    GetOperation (..),
    mkGetOperation,

    -- ** Request lenses
    goOperationId,

    -- * Destructuring the response
    GetOperationResponse (..),
    mkGetOperationResponse,

    -- ** Response lenses
    gorsOperation,
    gorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOperation' smart constructor.
newtype GetOperation = GetOperation'
  { -- | A GUID used to identify the operation.
    operationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperation' with the minimum fields required to make a request.
--
-- * 'operationId' - A GUID used to identify the operation.
mkGetOperation ::
  -- | 'operationId'
  Lude.Text ->
  GetOperation
mkGetOperation pOperationId_ =
  GetOperation' {operationId = pOperationId_}

-- | A GUID used to identify the operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goOperationId :: Lens.Lens' GetOperation Lude.Text
goOperationId = Lens.lens (operationId :: GetOperation -> Lude.Text) (\s a -> s {operationId = a} :: GetOperation)
{-# DEPRECATED goOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Lude.AWSRequest GetOperation where
  type Rs GetOperation = GetOperationResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOperation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetOperation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOperation where
  toJSON GetOperation' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("operationId" Lude..= operationId)])

instance Lude.ToPath GetOperation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOperation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperationResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkGetOperationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOperationResponse
mkGetOperationResponse pResponseStatus_ =
  GetOperationResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsOperation :: Lens.Lens' GetOperationResponse (Lude.Maybe Operation)
gorsOperation = Lens.lens (operation :: GetOperationResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: GetOperationResponse)
{-# DEPRECATED gorsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsResponseStatus :: Lens.Lens' GetOperationResponse Lude.Int
gorsResponseStatus = Lens.lens (responseStatus :: GetOperationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOperationResponse)
{-# DEPRECATED gorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
