{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    smMultiplexId,

    -- * Destructuring the response
    StartMultiplexResponse (..),
    mkStartMultiplexResponse,

    -- ** Response lenses
    smfrsState,
    smfrsARN,
    smfrsPipelinesRunningCount,
    smfrsAvailabilityZones,
    smfrsProgramCount,
    smfrsDestinations,
    smfrsName,
    smfrsId,
    smfrsMultiplexSettings,
    smfrsTags,
    smfrsResponseStatus,
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
newtype StartMultiplex = StartMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
smMultiplexId :: Lens.Lens' StartMultiplex Lude.Text
smMultiplexId = Lens.lens (multiplexId :: StartMultiplex -> Lude.Text) (\s a -> s {multiplexId = a} :: StartMultiplex)
{-# DEPRECATED smMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

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
  { -- | The current state of the multiplex.
    state :: Lude.Maybe MultiplexState,
    -- | The unique arn of the multiplex.
    arn :: Lude.Maybe Lude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    -- | A list of availability zones for the multiplex.
    availabilityZones :: Lude.Maybe [Lude.Text],
    -- | The number of programs in the multiplex.
    programCount :: Lude.Maybe Lude.Int,
    -- | A list of the multiplex output destinations.
    destinations :: Lude.Maybe [MultiplexOutputDestination],
    -- | The name of the multiplex.
    name :: Lude.Maybe Lude.Text,
    -- | The unique id of the multiplex.
    id :: Lude.Maybe Lude.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: Lude.Maybe MultiplexSettings,
    -- | A collection of key-value pairs.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMultiplexResponse' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the multiplex.
-- * 'arn' - The unique arn of the multiplex.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'availabilityZones' - A list of availability zones for the multiplex.
-- * 'programCount' - The number of programs in the multiplex.
-- * 'destinations' - A list of the multiplex output destinations.
-- * 'name' - The name of the multiplex.
-- * 'id' - The unique id of the multiplex.
-- * 'multiplexSettings' - Configuration for a multiplex event.
-- * 'tags' - A collection of key-value pairs.
-- * 'responseStatus' - The response status code.
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
smfrsState :: Lens.Lens' StartMultiplexResponse (Lude.Maybe MultiplexState)
smfrsState = Lens.lens (state :: StartMultiplexResponse -> Lude.Maybe MultiplexState) (\s a -> s {state = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsARN :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Text)
smfrsARN = Lens.lens (arn :: StartMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsPipelinesRunningCount :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Int)
smfrsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: StartMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsAvailabilityZones :: Lens.Lens' StartMultiplexResponse (Lude.Maybe [Lude.Text])
smfrsAvailabilityZones = Lens.lens (availabilityZones :: StartMultiplexResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsProgramCount :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Int)
smfrsProgramCount = Lens.lens (programCount :: StartMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {programCount = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsDestinations :: Lens.Lens' StartMultiplexResponse (Lude.Maybe [MultiplexOutputDestination])
smfrsDestinations = Lens.lens (destinations :: StartMultiplexResponse -> Lude.Maybe [MultiplexOutputDestination]) (\s a -> s {destinations = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsName :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Text)
smfrsName = Lens.lens (name :: StartMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsId :: Lens.Lens' StartMultiplexResponse (Lude.Maybe Lude.Text)
smfrsId = Lens.lens (id :: StartMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsMultiplexSettings :: Lens.Lens' StartMultiplexResponse (Lude.Maybe MultiplexSettings)
smfrsMultiplexSettings = Lens.lens (multiplexSettings :: StartMultiplexResponse -> Lude.Maybe MultiplexSettings) (\s a -> s {multiplexSettings = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsTags :: Lens.Lens' StartMultiplexResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
smfrsTags = Lens.lens (tags :: StartMultiplexResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smfrsResponseStatus :: Lens.Lens' StartMultiplexResponse Lude.Int
smfrsResponseStatus = Lens.lens (responseStatus :: StartMultiplexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMultiplexResponse)
{-# DEPRECATED smfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
