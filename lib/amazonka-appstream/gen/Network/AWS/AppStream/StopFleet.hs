{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StopFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified fleet.
module Network.AWS.AppStream.StopFleet
  ( -- * Creating a request
    StopFleet (..),
    mkStopFleet,

    -- ** Request lenses
    sfName,

    -- * Destructuring the response
    StopFleetResponse (..),
    mkStopFleetResponse,

    -- ** Response lenses
    storsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopFleet' smart constructor.
newtype StopFleet = StopFleet' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopFleet' with the minimum fields required to make a request.
--
-- * 'name' - The name of the fleet.
mkStopFleet ::
  -- | 'name'
  Lude.Text ->
  StopFleet
mkStopFleet pName_ = StopFleet' {name = pName_}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' StopFleet Lude.Text
sfName = Lens.lens (name :: StopFleet -> Lude.Text) (\s a -> s {name = a} :: StopFleet)
{-# DEPRECATED sfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StopFleet where
  type Rs StopFleet = StopFleetResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopFleetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.StopFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopFleet where
  toJSON StopFleet' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StopFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery StopFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopFleetResponse' smart constructor.
newtype StopFleetResponse = StopFleetResponse'
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

-- | Creates a value of 'StopFleetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopFleetResponse
mkStopFleetResponse pResponseStatus_ =
  StopFleetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
storsResponseStatus :: Lens.Lens' StopFleetResponse Lude.Int
storsResponseStatus = Lens.lens (responseStatus :: StopFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopFleetResponse)
{-# DEPRECATED storsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
