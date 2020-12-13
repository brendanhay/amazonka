{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified fleet.
module Network.AWS.AppStream.DeleteFleet
  ( -- * Creating a request
    DeleteFleet (..),
    mkDeleteFleet,

    -- ** Request lenses
    dfName,

    -- * Destructuring the response
    DeleteFleetResponse (..),
    mkDeleteFleetResponse,

    -- ** Response lenses
    dffrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFleet' smart constructor.
newtype DeleteFleet = DeleteFleet'
  { -- | The name of the fleet.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFleet' with the minimum fields required to make a request.
--
-- * 'name' - The name of the fleet.
mkDeleteFleet ::
  -- | 'name'
  Lude.Text ->
  DeleteFleet
mkDeleteFleet pName_ = DeleteFleet' {name = pName_}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' DeleteFleet Lude.Text
dfName = Lens.lens (name :: DeleteFleet -> Lude.Text) (\s a -> s {name = a} :: DeleteFleet)
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteFleet where
  type Rs DeleteFleet = DeleteFleetResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteFleetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DeleteFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteFleet where
  toJSON DeleteFleet' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFleetResponse' smart constructor.
newtype DeleteFleetResponse = DeleteFleetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFleetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFleetResponse
mkDeleteFleetResponse pResponseStatus_ =
  DeleteFleetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dffrsResponseStatus :: Lens.Lens' DeleteFleetResponse Lude.Int
dffrsResponseStatus = Lens.lens (responseStatus :: DeleteFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFleetResponse)
{-# DEPRECATED dffrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
