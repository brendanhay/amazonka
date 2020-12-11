{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    delrsState,
    delrsARN,
    delrsPipelinesRunningCount,
    delrsAvailabilityZones,
    delrsProgramCount,
    delrsDestinations,
    delrsName,
    delrsId,
    delrsMultiplexSettings,
    delrsTags,
    delrsResponseStatus,
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
  { multiplexId ::
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
  { state ::
      Lude.Maybe MultiplexState,
    arn :: Lude.Maybe Lude.Text,
    pipelinesRunningCount ::
      Lude.Maybe Lude.Int,
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

-- | Creates a value of 'DeleteMultiplexResponse' with the minimum fields required to make a request.
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
delrsState :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe MultiplexState)
delrsState = Lens.lens (state :: DeleteMultiplexResponse -> Lude.Maybe MultiplexState) (\s a -> s {state = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsARN :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Text)
delrsARN = Lens.lens (arn :: DeleteMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsPipelinesRunningCount :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Int)
delrsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: DeleteMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsAvailabilityZones :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe [Lude.Text])
delrsAvailabilityZones = Lens.lens (availabilityZones :: DeleteMultiplexResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsProgramCount :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Int)
delrsProgramCount = Lens.lens (programCount :: DeleteMultiplexResponse -> Lude.Maybe Lude.Int) (\s a -> s {programCount = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsDestinations :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe [MultiplexOutputDestination])
delrsDestinations = Lens.lens (destinations :: DeleteMultiplexResponse -> Lude.Maybe [MultiplexOutputDestination]) (\s a -> s {destinations = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsName :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Text)
delrsName = Lens.lens (name :: DeleteMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsId :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe Lude.Text)
delrsId = Lens.lens (id :: DeleteMultiplexResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsMultiplexSettings :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe MultiplexSettings)
delrsMultiplexSettings = Lens.lens (multiplexSettings :: DeleteMultiplexResponse -> Lude.Maybe MultiplexSettings) (\s a -> s {multiplexSettings = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsTags :: Lens.Lens' DeleteMultiplexResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
delrsTags = Lens.lens (tags :: DeleteMultiplexResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteMultiplexResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteMultiplexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMultiplexResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
