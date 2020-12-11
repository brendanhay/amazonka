{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway group.
module Network.AWS.AlexaBusiness.DeleteGatewayGroup
  ( -- * Creating a request
    DeleteGatewayGroup (..),
    mkDeleteGatewayGroup,

    -- ** Request lenses
    dggGatewayGroupARN,

    -- * Destructuring the response
    DeleteGatewayGroupResponse (..),
    mkDeleteGatewayGroupResponse,

    -- ** Response lenses
    dggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGatewayGroup' smart constructor.
newtype DeleteGatewayGroup = DeleteGatewayGroup'
  { gatewayGroupARN ::
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

-- | Creates a value of 'DeleteGatewayGroup' with the minimum fields required to make a request.
--
-- * 'gatewayGroupARN' - The ARN of the gateway group to delete.
mkDeleteGatewayGroup ::
  -- | 'gatewayGroupARN'
  Lude.Text ->
  DeleteGatewayGroup
mkDeleteGatewayGroup pGatewayGroupARN_ =
  DeleteGatewayGroup' {gatewayGroupARN = pGatewayGroupARN_}

-- | The ARN of the gateway group to delete.
--
-- /Note:/ Consider using 'gatewayGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dggGatewayGroupARN :: Lens.Lens' DeleteGatewayGroup Lude.Text
dggGatewayGroupARN = Lens.lens (gatewayGroupARN :: DeleteGatewayGroup -> Lude.Text) (\s a -> s {gatewayGroupARN = a} :: DeleteGatewayGroup)
{-# DEPRECATED dggGatewayGroupARN "Use generic-lens or generic-optics with 'gatewayGroupARN' instead." #-}

instance Lude.AWSRequest DeleteGatewayGroup where
  type Rs DeleteGatewayGroup = DeleteGatewayGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteGatewayGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGatewayGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteGatewayGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteGatewayGroup where
  toJSON DeleteGatewayGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("GatewayGroupArn" Lude..= gatewayGroupARN)]
      )

instance Lude.ToPath DeleteGatewayGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGatewayGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGatewayGroupResponse' smart constructor.
newtype DeleteGatewayGroupResponse = DeleteGatewayGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGatewayGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteGatewayGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGatewayGroupResponse
mkDeleteGatewayGroupResponse pResponseStatus_ =
  DeleteGatewayGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dggrsResponseStatus :: Lens.Lens' DeleteGatewayGroupResponse Lude.Int
dggrsResponseStatus = Lens.lens (responseStatus :: DeleteGatewayGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGatewayGroupResponse)
{-# DEPRECATED dggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
