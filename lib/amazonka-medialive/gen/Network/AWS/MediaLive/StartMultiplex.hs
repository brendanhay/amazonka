{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.StartMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start (run) the multiplex. Starting the multiplex does not start the channels. You must explicitly start each channel.
module Network.AWS.MediaLive.StartMultiplex
  ( -- * Creating a request
    StartMultiplex (..),
    mkStartMultiplex,

    -- ** Request lenses
    sMultiplexId,

    -- * Destructuring the response
    StartMultiplexResponse (..),
    mkStartMultiplexResponse,

    -- ** Response lenses
    starsState,
    starsARN,
    starsPipelinesRunningCount,
    starsAvailabilityZones,
    starsProgramCount,
    starsDestinations,
    starsName,
    starsId,
    starsMultiplexSettings,
    starsTags,
    starsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for StartMultiplexRequest
--
-- /See:/ 'mkStartMultiplex' smart constructor.
newtype StartMultiplex = StartMultiplex' {multiplexId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMultiplex' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the multiplex.
mkStartMultiplex ::
  -- | 'multiplexId'
  Lude.Text ->
  StartMultiplex
mkStartMultiplex pMultiplexId_ =
  StartMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMultiplexId :: Lens.Lens' StartMultiplex Lude.Text
sMultiplexId = Lens.lens (multiplexId :: StartMultiplex -> Lude.Text) (\s a -> s {multiplexId = a} :: StartMultiplex)
{-# DEPRECATED sMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Lude.AWSRequest StartMultiplex where
  type Rs StartMultiplex = StartMultiplexResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMultiplexResponse'
            Lude.<$> (x Lude..?> "state")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "pipelinesRunningCount")
            Lude.<*> (x Lude..?> "availabilityZones" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "programCount")
            Lude.<*> (x Lude..?> "destinations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "multiplexSettings")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMultiplex where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMultiplex where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath StartMultiplex where
  toPath StartMultiplex' {..} =
    Lude.mconcat
      ["/prod/multiplexes/", Lude.toBS multiplexId, "/start"]

instance Lude.ToQuery StartMultiplex where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for StartMultiplexResponse
--
-- /See:/ 'mkStartMultiplexResponse' smart constructor.
data StartMultiplexResponse = StartMultiplexResponse'
  { state ::
      Lude.Maybe MultiplexState,
    arn :: Lude.Maybe Lude.Text,
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    availabilityZones :: Lude.Maybe [Lude.Text],
    programCount :: Lude.Maybe Lude.Int,
    destinations ::
      Lude.Maybe [MultiplexOutputDestination],
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    multiplexSettings ::
      Lude.Maybe MultiplexSettings,
    tags ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'StartMultiplexResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The unique arn of the multiplex.
-- * 'availabilityZones' - A list of availability zones for the multiplex.
-- * 'destinations' - A list of the multiplex output destinations.
-- * 'id' - The unique id of the multiplex.
-- * 'multiplexSettings' - Configuration for a multiplex event.
-- * 'name' - The name of the multiplex.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'programCount' - The number of programs in the multiplex.
-- * 'responseStatus' - The response status code.
-- * 'state' - The current state of the multiplex.
-- * 'tags' - A collection of key-value pairs.
mkStartMultiplexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMultiplexResponse
mkStartMultiplexResponse pResponseStatus_ =
  StartMultiplexResponse'
    { state = Lude.Nothing,
      arn = Lude.Nothing,
      pipelinesRunningCount = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      programCount = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      multiplexSettings = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsState :: Lens.Lens' StartMultiplexResponse (Lude.Maybe MultiplexState)
starsState = Lens.lens (state :: StartMultiplexResponse -> Lude.Maybe MultiplexState) (\s a -> s {state = a} :: StartMultiplexResponse)
{-# DEPRECATED starsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsARN :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Text)
starsARN = Lens.lens (arn :: StartMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: StartMultiplexResponse)
{-# DEPRECATED starsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsPipelinesRunningCount :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Int)
starsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: StartMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: StartMultiplexResponse)
{-# DEPRECATED starsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsAvailabilityZones :: Lens.Lens' StartMultiplexResponse (Lude.Maybe [Lude.Text])
starsAvailabilityZones = Lens.lens (availabilityZones :: StartMultiplexResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: StartMultiplexResponse)
{-# DEPRECATED starsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsProgramCount :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Int)
starsProgramCount = Lens.lens (programCount :: StartMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {programCount = a} :: StartMultiplexResponse)
{-# DEPRECATED starsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsDestinations :: Lens.Lens' StartMultiplexResponse (Lude.Maybe [MultiplexOutputDestination])
starsDestinations = Lens.lens (destinations :: StartMultiplexResponse -> Lude.Maybe [MultiplexOutputDestination]) (\s a -> s {destinations = a} :: StartMultiplexResponse)
{-# DEPRECATED starsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsName :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Text)
starsName = Lens.lens (name :: StartMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StartMultiplexResponse)
{-# DEPRECATED starsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsId :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Text)
starsId = Lens.lens (id :: StartMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: StartMultiplexResponse)
{-# DEPRECATED starsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsMultiplexSettings :: Lens.Lens' StartMultiplexResponse (Lude.Maybe MultiplexSettings)
starsMultiplexSettings = Lens.lens (multiplexSettings :: StartMultiplexResponse -> Lude.Maybe MultiplexSettings) (\s a -> s {multiplexSettings = a} :: StartMultiplexResponse)
{-# DEPRECATED starsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsTags :: Lens.Lens' StartMultiplexResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
starsTags = Lens.lens (tags :: StartMultiplexResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: StartMultiplexResponse)
{-# DEPRECATED starsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsResponseStatus :: Lens.Lens' StartMultiplexResponse Lude.Int
starsResponseStatus = Lens.lens (responseStatus :: StartMultiplexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMultiplexResponse)
{-# DEPRECATED starsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
