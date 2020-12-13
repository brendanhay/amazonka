{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a multiplex. The multiplex must be idle.
module Network.AWS.MediaLive.DeleteMultiplex
  ( -- * Creating a request
    DeleteMultiplex (..),
    mkDeleteMultiplex,

    -- ** Request lenses
    dMultiplexId,

    -- * Destructuring the response
    DeleteMultiplexResponse (..),
    mkDeleteMultiplexResponse,

    -- ** Response lenses
    dmfrsState,
    dmfrsARN,
    dmfrsPipelinesRunningCount,
    dmfrsAvailabilityZones,
    dmfrsProgramCount,
    dmfrsDestinations,
    dmfrsName,
    dmfrsId,
    dmfrsMultiplexSettings,
    dmfrsTags,
    dmfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DeleteMultiplexRequest
--
-- /See:/ 'mkDeleteMultiplex' smart constructor.
newtype DeleteMultiplex = DeleteMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMultiplex' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the multiplex.
mkDeleteMultiplex ::
  -- | 'multiplexId'
  Lude.Text ->
  DeleteMultiplex
mkDeleteMultiplex pMultiplexId_ =
  DeleteMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMultiplexId :: Lens.Lens' DeleteMultiplex Lude.Text
dMultiplexId = Lens.lens (multiplexId :: DeleteMultiplex -> Lude.Text) (\s a -> s {multiplexId = a} :: DeleteMultiplex)
{-# DEPRECATED dMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Lude.AWSRequest DeleteMultiplex where
  type Rs DeleteMultiplex = DeleteMultiplexResponse
  request = Req.delete mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteMultiplexResponse'
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

instance Lude.ToHeaders DeleteMultiplex where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteMultiplex where
  toPath DeleteMultiplex' {..} =
    Lude.mconcat ["/prod/multiplexes/", Lude.toBS multiplexId]

instance Lude.ToQuery DeleteMultiplex where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DeleteMultiplexResponse
--
-- /See:/ 'mkDeleteMultiplexResponse' smart constructor.
data DeleteMultiplexResponse = DeleteMultiplexResponse'
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

-- | Creates a value of 'DeleteMultiplexResponse' with the minimum fields required to make a request.
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
mkDeleteMultiplexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMultiplexResponse
mkDeleteMultiplexResponse pResponseStatus_ =
  DeleteMultiplexResponse'
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
dmfrsState :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe MultiplexState)
dmfrsState = Lens.lens (state :: DeleteMultiplexResponse -> Lude.Maybe MultiplexState) (\s a -> s {state = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsARN :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Text)
dmfrsARN = Lens.lens (arn :: DeleteMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsPipelinesRunningCount :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Int)
dmfrsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: DeleteMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsAvailabilityZones :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe [Lude.Text])
dmfrsAvailabilityZones = Lens.lens (availabilityZones :: DeleteMultiplexResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsProgramCount :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Int)
dmfrsProgramCount = Lens.lens (programCount :: DeleteMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {programCount = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsDestinations :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe [MultiplexOutputDestination])
dmfrsDestinations = Lens.lens (destinations :: DeleteMultiplexResponse -> Lude.Maybe [MultiplexOutputDestination]) (\s a -> s {destinations = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsName :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Text)
dmfrsName = Lens.lens (name :: DeleteMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsId :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Text)
dmfrsId = Lens.lens (id :: DeleteMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsMultiplexSettings :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe MultiplexSettings)
dmfrsMultiplexSettings = Lens.lens (multiplexSettings :: DeleteMultiplexResponse -> Lude.Maybe MultiplexSettings) (\s a -> s {multiplexSettings = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsTags :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dmfrsTags = Lens.lens (tags :: DeleteMultiplexResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsResponseStatus :: Lens.Lens' DeleteMultiplexResponse Lude.Int
dmfrsResponseStatus = Lens.lens (responseStatus :: DeleteMultiplexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMultiplexResponse)
{-# DEPRECATED dmfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
