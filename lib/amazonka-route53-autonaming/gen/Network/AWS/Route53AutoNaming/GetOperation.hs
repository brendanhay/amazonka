{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about any operation that returns an operation ID in the response, such as a @CreateService@ request.
module Network.AWS.Route53AutoNaming.GetOperation
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkGetOperation' smart constructor.
newtype GetOperation = GetOperation' {operationId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperation' with the minimum fields required to make a request.
--
-- * 'operationId' - The ID of the operation that you want to get more information about.
mkGetOperation ::
  -- | 'operationId'
  Lude.Text ->
  GetOperation
mkGetOperation pOperationId_ =
  GetOperation' {operationId = pOperationId_}

-- | The ID of the operation that you want to get more information about.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goOperationId :: Lens.Lens' GetOperation Lude.Text
goOperationId = Lens.lens (operationId :: GetOperation -> Lude.Text) (\s a -> s {operationId = a} :: GetOperation)
{-# DEPRECATED goOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Lude.AWSRequest GetOperation where
  type Rs GetOperation = GetOperationResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Lude.<$> (x Lude..?> "Operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOperation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.GetOperation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOperation where
  toJSON GetOperation' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("OperationId" Lude..= operationId)])

instance Lude.ToPath GetOperation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOperation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { operation ::
      Lude.Maybe Operation,
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

-- | Creates a value of 'GetOperationResponse' with the minimum fields required to make a request.
--
-- * 'operation' - A complex type that contains information about the operation.
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

-- | A complex type that contains information about the operation.
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
