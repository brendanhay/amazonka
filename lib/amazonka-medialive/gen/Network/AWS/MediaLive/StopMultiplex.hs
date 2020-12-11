{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.StopMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running multiplex. If the multiplex isn't running, this action has no effect.
module Network.AWS.MediaLive.StopMultiplex
  ( -- * Creating a request
    StopMultiplex (..),
    mkStopMultiplex,

    -- ** Request lenses
    smMultiplexId,

    -- * Destructuring the response
    StopMultiplexResponse (..),
    mkStopMultiplexResponse,

    -- ** Response lenses
    smrsState,
    smrsARN,
    smrsPipelinesRunningCount,
    smrsAvailabilityZones,
    smrsProgramCount,
    smrsDestinations,
    smrsName,
    smrsId,
    smrsMultiplexSettings,
    smrsTags,
    smrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for StopMultiplexRequest
--
-- /See:/ 'mkStopMultiplex' smart constructor.
newtype StopMultiplex = StopMultiplex' {multiplexId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopMultiplex' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the multiplex.
mkStopMultiplex ::
  -- | 'multiplexId'
  Lude.Text ->
  StopMultiplex
mkStopMultiplex pMultiplexId_ =
  StopMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMultiplexId :: Lens.Lens' StopMultiplex Lude.Text
smMultiplexId = Lens.lens (multiplexId :: StopMultiplex -> Lude.Text) (\s a -> s {multiplexId = a} :: StopMultiplex)
{-# DEPRECATED smMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Lude.AWSRequest StopMultiplex where
  type Rs StopMultiplex = StopMultiplexResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopMultiplexResponse'
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

instance Lude.ToHeaders StopMultiplex where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopMultiplex where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath StopMultiplex where
  toPath StopMultiplex' {..} =
    Lude.mconcat
      ["/prod/multiplexes/", Lude.toBS multiplexId, "/stop"]

instance Lude.ToQuery StopMultiplex where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for StopMultiplexResponse
--
-- /See:/ 'mkStopMultiplexResponse' smart constructor.
data StopMultiplexResponse = StopMultiplexResponse'
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
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'StopMultiplexResponse' with the minimum fields required to make a request.
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
mkStopMultiplexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopMultiplexResponse
mkStopMultiplexResponse pResponseStatus_ =
  StopMultiplexResponse'
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
smrsState :: Lens.Lens' StopMultiplexResponse (Lude.Maybe MultiplexState)
smrsState = Lens.lens (state :: StopMultiplexResponse -> Lude.Maybe MultiplexState) (\s a -> s {state = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsARN :: Lens.Lens' StopMultiplexResponse (Lude.Maybe Lude.Text)
smrsARN = Lens.lens (arn :: StopMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsPipelinesRunningCount :: Lens.Lens' StopMultiplexResponse (Lude.Maybe Lude.Int)
smrsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: StopMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsAvailabilityZones :: Lens.Lens' StopMultiplexResponse (Lude.Maybe [Lude.Text])
smrsAvailabilityZones = Lens.lens (availabilityZones :: StopMultiplexResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsProgramCount :: Lens.Lens' StopMultiplexResponse (Lude.Maybe Lude.Int)
smrsProgramCount = Lens.lens (programCount :: StopMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {programCount = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsDestinations :: Lens.Lens' StopMultiplexResponse (Lude.Maybe [MultiplexOutputDestination])
smrsDestinations = Lens.lens (destinations :: StopMultiplexResponse -> Lude.Maybe [MultiplexOutputDestination]) (\s a -> s {destinations = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsName :: Lens.Lens' StopMultiplexResponse (Lude.Maybe Lude.Text)
smrsName = Lens.lens (name :: StopMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsId :: Lens.Lens' StopMultiplexResponse (Lude.Maybe Lude.Text)
smrsId = Lens.lens (id :: StopMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsMultiplexSettings :: Lens.Lens' StopMultiplexResponse (Lude.Maybe MultiplexSettings)
smrsMultiplexSettings = Lens.lens (multiplexSettings :: StopMultiplexResponse -> Lude.Maybe MultiplexSettings) (\s a -> s {multiplexSettings = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsTags :: Lens.Lens' StopMultiplexResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
smrsTags = Lens.lens (tags :: StopMultiplexResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsResponseStatus :: Lens.Lens' StopMultiplexResponse Lude.Int
smrsResponseStatus = Lens.lens (responseStatus :: StopMultiplexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopMultiplexResponse)
{-# DEPRECATED smrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
