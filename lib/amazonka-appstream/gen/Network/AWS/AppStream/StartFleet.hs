{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StartFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified fleet.
module Network.AWS.AppStream.StartFleet
  ( -- * Creating a request
    StartFleet (..),
    mkStartFleet,

    -- ** Request lenses
    sffName,

    -- * Destructuring the response
    StartFleetResponse (..),
    mkStartFleetResponse,

    -- ** Response lenses
    sfrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartFleet' smart constructor.
newtype StartFleet = StartFleet'
  { -- | The name of the fleet.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartFleet' with the minimum fields required to make a request.
--
-- * 'name' - The name of the fleet.
mkStartFleet ::
  -- | 'name'
  Lude.Text ->
  StartFleet
mkStartFleet pName_ = StartFleet' {name = pName_}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sffName :: Lens.Lens' StartFleet Lude.Text
sffName = Lens.lens (name :: StartFleet -> Lude.Text) (\s a -> s {name = a} :: StartFleet)
{-# DEPRECATED sffName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StartFleet where
  type Rs StartFleet = StartFleetResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartFleetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.StartFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartFleet where
  toJSON StartFleet' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StartFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery StartFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartFleetResponse' smart constructor.
newtype StartFleetResponse = StartFleetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartFleetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartFleetResponse
mkStartFleetResponse pResponseStatus_ =
  StartFleetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsResponseStatus :: Lens.Lens' StartFleetResponse Lude.Int
sfrsResponseStatus = Lens.lens (responseStatus :: StartFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartFleetResponse)
{-# DEPRECATED sfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
