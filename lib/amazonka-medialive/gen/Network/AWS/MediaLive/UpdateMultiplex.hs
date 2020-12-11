{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a multiplex.
module Network.AWS.MediaLive.UpdateMultiplex
  ( -- * Creating a request
    UpdateMultiplex (..),
    mkUpdateMultiplex,

    -- ** Request lenses
    umName,
    umMultiplexSettings,
    umMultiplexId,

    -- * Destructuring the response
    UpdateMultiplexResponse (..),
    mkUpdateMultiplexResponse,

    -- ** Response lenses
    umrsMultiplex,
    umrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to update a multiplex.
--
-- /See:/ 'mkUpdateMultiplex' smart constructor.
data UpdateMultiplex = UpdateMultiplex'
  { name ::
      Lude.Maybe Lude.Text,
    multiplexSettings :: Lude.Maybe MultiplexSettings,
    multiplexId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMultiplex' with the minimum fields required to make a request.
--
-- * 'multiplexId' - ID of the multiplex to update.
-- * 'multiplexSettings' - The new settings for a multiplex.
-- * 'name' - Name of the multiplex.
mkUpdateMultiplex ::
  -- | 'multiplexId'
  Lude.Text ->
  UpdateMultiplex
mkUpdateMultiplex pMultiplexId_ =
  UpdateMultiplex'
    { name = Lude.Nothing,
      multiplexSettings = Lude.Nothing,
      multiplexId = pMultiplexId_
    }

-- | Name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umName :: Lens.Lens' UpdateMultiplex (Lude.Maybe Lude.Text)
umName = Lens.lens (name :: UpdateMultiplex -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateMultiplex)
{-# DEPRECATED umName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new settings for a multiplex.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umMultiplexSettings :: Lens.Lens' UpdateMultiplex (Lude.Maybe MultiplexSettings)
umMultiplexSettings = Lens.lens (multiplexSettings :: UpdateMultiplex -> Lude.Maybe MultiplexSettings) (\s a -> s {multiplexSettings = a} :: UpdateMultiplex)
{-# DEPRECATED umMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | ID of the multiplex to update.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umMultiplexId :: Lens.Lens' UpdateMultiplex Lude.Text
umMultiplexId = Lens.lens (multiplexId :: UpdateMultiplex -> Lude.Text) (\s a -> s {multiplexId = a} :: UpdateMultiplex)
{-# DEPRECATED umMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Lude.AWSRequest UpdateMultiplex where
  type Rs UpdateMultiplex = UpdateMultiplexResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMultiplexResponse'
            Lude.<$> (x Lude..?> "multiplex") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMultiplex where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMultiplex where
  toJSON UpdateMultiplex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("name" Lude..=) Lude.<$> name,
            ("multiplexSettings" Lude..=) Lude.<$> multiplexSettings
          ]
      )

instance Lude.ToPath UpdateMultiplex where
  toPath UpdateMultiplex' {..} =
    Lude.mconcat ["/prod/multiplexes/", Lude.toBS multiplexId]

instance Lude.ToQuery UpdateMultiplex where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for UpdateMultiplexResponse
--
-- /See:/ 'mkUpdateMultiplexResponse' smart constructor.
data UpdateMultiplexResponse = UpdateMultiplexResponse'
  { multiplex ::
      Lude.Maybe Multiplex,
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

-- | Creates a value of 'UpdateMultiplexResponse' with the minimum fields required to make a request.
--
-- * 'multiplex' - The updated multiplex.
-- * 'responseStatus' - The response status code.
mkUpdateMultiplexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMultiplexResponse
mkUpdateMultiplexResponse pResponseStatus_ =
  UpdateMultiplexResponse'
    { multiplex = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated multiplex.
--
-- /Note:/ Consider using 'multiplex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrsMultiplex :: Lens.Lens' UpdateMultiplexResponse (Lude.Maybe Multiplex)
umrsMultiplex = Lens.lens (multiplex :: UpdateMultiplexResponse -> Lude.Maybe Multiplex) (\s a -> s {multiplex = a} :: UpdateMultiplexResponse)
{-# DEPRECATED umrsMultiplex "Use generic-lens or generic-optics with 'multiplex' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrsResponseStatus :: Lens.Lens' UpdateMultiplexResponse Lude.Int
umrsResponseStatus = Lens.lens (responseStatus :: UpdateMultiplexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMultiplexResponse)
{-# DEPRECATED umrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
