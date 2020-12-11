-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Multiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Multiplex
  ( Multiplex (..),

    -- * Smart constructor
    mkMultiplex,

    -- * Lenses
    mState,
    mARN,
    mPipelinesRunningCount,
    mAvailabilityZones,
    mProgramCount,
    mDestinations,
    mName,
    mId,
    mMultiplexSettings,
    mTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexOutputDestination
import Network.AWS.MediaLive.Types.MultiplexSettings
import Network.AWS.MediaLive.Types.MultiplexState
import qualified Network.AWS.Prelude as Lude

-- | The multiplex object.
--
-- /See:/ 'mkMultiplex' smart constructor.
data Multiplex = Multiplex'
  { state :: Lude.Maybe MultiplexState,
    arn :: Lude.Maybe Lude.Text,
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    availabilityZones :: Lude.Maybe [Lude.Text],
    programCount :: Lude.Maybe Lude.Int,
    destinations :: Lude.Maybe [MultiplexOutputDestination],
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    multiplexSettings :: Lude.Maybe MultiplexSettings,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Multiplex' with the minimum fields required to make a request.
--
-- * 'arn' - The unique arn of the multiplex.
-- * 'availabilityZones' - A list of availability zones for the multiplex.
-- * 'destinations' - A list of the multiplex output destinations.
-- * 'id' - The unique id of the multiplex.
-- * 'multiplexSettings' - Configuration for a multiplex event.
-- * 'name' - The name of the multiplex.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'programCount' - The number of programs in the multiplex.
-- * 'state' - The current state of the multiplex.
-- * 'tags' - A collection of key-value pairs.
mkMultiplex ::
  Multiplex
mkMultiplex =
  Multiplex'
    { state = Lude.Nothing,
      arn = Lude.Nothing,
      pipelinesRunningCount = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      programCount = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      multiplexSettings = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mState :: Lens.Lens' Multiplex (Lude.Maybe MultiplexState)
mState = Lens.lens (state :: Multiplex -> Lude.Maybe MultiplexState) (\s a -> s {state = a} :: Multiplex)
{-# DEPRECATED mState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mARN :: Lens.Lens' Multiplex (Lude.Maybe Lude.Text)
mARN = Lens.lens (arn :: Multiplex -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Multiplex)
{-# DEPRECATED mARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPipelinesRunningCount :: Lens.Lens' Multiplex (Lude.Maybe Lude.Int)
mPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: Multiplex -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: Multiplex)
{-# DEPRECATED mPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAvailabilityZones :: Lens.Lens' Multiplex (Lude.Maybe [Lude.Text])
mAvailabilityZones = Lens.lens (availabilityZones :: Multiplex -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: Multiplex)
{-# DEPRECATED mAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mProgramCount :: Lens.Lens' Multiplex (Lude.Maybe Lude.Int)
mProgramCount = Lens.lens (programCount :: Multiplex -> Lude.Maybe Lude.Int) (\s a -> s {programCount = a} :: Multiplex)
{-# DEPRECATED mProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDestinations :: Lens.Lens' Multiplex (Lude.Maybe [MultiplexOutputDestination])
mDestinations = Lens.lens (destinations :: Multiplex -> Lude.Maybe [MultiplexOutputDestination]) (\s a -> s {destinations = a} :: Multiplex)
{-# DEPRECATED mDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mName :: Lens.Lens' Multiplex (Lude.Maybe Lude.Text)
mName = Lens.lens (name :: Multiplex -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Multiplex)
{-# DEPRECATED mName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mId :: Lens.Lens' Multiplex (Lude.Maybe Lude.Text)
mId = Lens.lens (id :: Multiplex -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Multiplex)
{-# DEPRECATED mId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMultiplexSettings :: Lens.Lens' Multiplex (Lude.Maybe MultiplexSettings)
mMultiplexSettings = Lens.lens (multiplexSettings :: Multiplex -> Lude.Maybe MultiplexSettings) (\s a -> s {multiplexSettings = a} :: Multiplex)
{-# DEPRECATED mMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTags :: Lens.Lens' Multiplex (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
mTags = Lens.lens (tags :: Multiplex -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: Multiplex)
{-# DEPRECATED mTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Multiplex where
  parseJSON =
    Lude.withObject
      "Multiplex"
      ( \x ->
          Multiplex'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "pipelinesRunningCount")
            Lude.<*> (x Lude..:? "availabilityZones" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "programCount")
            Lude.<*> (x Lude..:? "destinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "multiplexSettings")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
