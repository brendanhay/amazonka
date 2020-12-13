{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a multiplex.
module Network.AWS.MediaLive.DescribeMultiplex
  ( -- * Creating a request
    DescribeMultiplex (..),
    mkDescribeMultiplex,

    -- ** Request lenses
    dmMultiplexId,

    -- * Destructuring the response
    DescribeMultiplexResponse (..),
    mkDescribeMultiplexResponse,

    -- ** Response lenses
    dmrsState,
    dmrsARN,
    dmrsPipelinesRunningCount,
    dmrsAvailabilityZones,
    dmrsProgramCount,
    dmrsDestinations,
    dmrsName,
    dmrsId,
    dmrsMultiplexSettings,
    dmrsTags,
    dmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeMultiplexRequest
--
-- /See:/ 'mkDescribeMultiplex' smart constructor.
newtype DescribeMultiplex = DescribeMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMultiplex' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the multiplex.
mkDescribeMultiplex ::
  -- | 'multiplexId'
  Lude.Text ->
  DescribeMultiplex
mkDescribeMultiplex pMultiplexId_ =
  DescribeMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmMultiplexId :: Lens.Lens' DescribeMultiplex Lude.Text
dmMultiplexId = Lens.lens (multiplexId :: DescribeMultiplex -> Lude.Text) (\s a -> s {multiplexId = a} :: DescribeMultiplex)
{-# DEPRECATED dmMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Lude.AWSRequest DescribeMultiplex where
  type Rs DescribeMultiplex = DescribeMultiplexResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMultiplexResponse'
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

instance Lude.ToHeaders DescribeMultiplex where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeMultiplex where
  toPath DescribeMultiplex' {..} =
    Lude.mconcat ["/prod/multiplexes/", Lude.toBS multiplexId]

instance Lude.ToQuery DescribeMultiplex where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeMultiplexResponse
--
-- /See:/ 'mkDescribeMultiplexResponse' smart constructor.
data DescribeMultiplexResponse = DescribeMultiplexResponse'
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

-- | Creates a value of 'DescribeMultiplexResponse' with the minimum fields required to make a request.
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
mkDescribeMultiplexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMultiplexResponse
mkDescribeMultiplexResponse pResponseStatus_ =
  DescribeMultiplexResponse'
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
dmrsState :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe MultiplexState)
dmrsState = Lens.lens (state :: DescribeMultiplexResponse -> Lude.Maybe MultiplexState) (\s a -> s {state = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsARN :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe Lude.Text)
dmrsARN = Lens.lens (arn :: DescribeMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsPipelinesRunningCount :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe Lude.Int)
dmrsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: DescribeMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsAvailabilityZones :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe [Lude.Text])
dmrsAvailabilityZones = Lens.lens (availabilityZones :: DescribeMultiplexResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsProgramCount :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe Lude.Int)
dmrsProgramCount = Lens.lens (programCount :: DescribeMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {programCount = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsDestinations :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe [MultiplexOutputDestination])
dmrsDestinations = Lens.lens (destinations :: DescribeMultiplexResponse -> Lude.Maybe [MultiplexOutputDestination]) (\s a -> s {destinations = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsName :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe Lude.Text)
dmrsName = Lens.lens (name :: DescribeMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsId :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe Lude.Text)
dmrsId = Lens.lens (id :: DescribeMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsMultiplexSettings :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe MultiplexSettings)
dmrsMultiplexSettings = Lens.lens (multiplexSettings :: DescribeMultiplexResponse -> Lude.Maybe MultiplexSettings) (\s a -> s {multiplexSettings = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsTags :: Lens.Lens' DescribeMultiplexResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dmrsTags = Lens.lens (tags :: DescribeMultiplexResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsResponseStatus :: Lens.Lens' DescribeMultiplexResponse Lude.Int
dmrsResponseStatus = Lens.lens (responseStatus :: DescribeMultiplexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMultiplexResponse)
{-# DEPRECATED dmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
