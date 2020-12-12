{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSummary
  ( MultiplexSummary (..),

    -- * Smart constructor
    mkMultiplexSummary,

    -- * Lenses
    msState,
    msARN,
    msPipelinesRunningCount,
    msAvailabilityZones,
    msProgramCount,
    msName,
    msId,
    msMultiplexSettings,
    msTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexSettingsSummary
import Network.AWS.MediaLive.Types.MultiplexState
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for MultiplexSummary
--
-- /See:/ 'mkMultiplexSummary' smart constructor.
data MultiplexSummary = MultiplexSummary'
  { state ::
      Lude.Maybe MultiplexState,
    arn :: Lude.Maybe Lude.Text,
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    availabilityZones :: Lude.Maybe [Lude.Text],
    programCount :: Lude.Maybe Lude.Int,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    multiplexSettings :: Lude.Maybe MultiplexSettingsSummary,
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

-- | Creates a value of 'MultiplexSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The unique arn of the multiplex.
-- * 'availabilityZones' - A list of availability zones for the multiplex.
-- * 'id' - The unique id of the multiplex.
-- * 'multiplexSettings' - Configuration for a multiplex event.
-- * 'name' - The name of the multiplex.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'programCount' - The number of programs in the multiplex.
-- * 'state' - The current state of the multiplex.
-- * 'tags' - A collection of key-value pairs.
mkMultiplexSummary ::
  MultiplexSummary
mkMultiplexSummary =
  MultiplexSummary'
    { state = Lude.Nothing,
      arn = Lude.Nothing,
      pipelinesRunningCount = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      programCount = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      multiplexSettings = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msState :: Lens.Lens' MultiplexSummary (Lude.Maybe MultiplexState)
msState = Lens.lens (state :: MultiplexSummary -> Lude.Maybe MultiplexState) (\s a -> s {state = a} :: MultiplexSummary)
{-# DEPRECATED msState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msARN :: Lens.Lens' MultiplexSummary (Lude.Maybe Lude.Text)
msARN = Lens.lens (arn :: MultiplexSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: MultiplexSummary)
{-# DEPRECATED msARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPipelinesRunningCount :: Lens.Lens' MultiplexSummary (Lude.Maybe Lude.Int)
msPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: MultiplexSummary -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: MultiplexSummary)
{-# DEPRECATED msPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAvailabilityZones :: Lens.Lens' MultiplexSummary (Lude.Maybe [Lude.Text])
msAvailabilityZones = Lens.lens (availabilityZones :: MultiplexSummary -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: MultiplexSummary)
{-# DEPRECATED msAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msProgramCount :: Lens.Lens' MultiplexSummary (Lude.Maybe Lude.Int)
msProgramCount = Lens.lens (programCount :: MultiplexSummary -> Lude.Maybe Lude.Int) (\s a -> s {programCount = a} :: MultiplexSummary)
{-# DEPRECATED msProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msName :: Lens.Lens' MultiplexSummary (Lude.Maybe Lude.Text)
msName = Lens.lens (name :: MultiplexSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MultiplexSummary)
{-# DEPRECATED msName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msId :: Lens.Lens' MultiplexSummary (Lude.Maybe Lude.Text)
msId = Lens.lens (id :: MultiplexSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: MultiplexSummary)
{-# DEPRECATED msId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMultiplexSettings :: Lens.Lens' MultiplexSummary (Lude.Maybe MultiplexSettingsSummary)
msMultiplexSettings = Lens.lens (multiplexSettings :: MultiplexSummary -> Lude.Maybe MultiplexSettingsSummary) (\s a -> s {multiplexSettings = a} :: MultiplexSummary)
{-# DEPRECATED msMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTags :: Lens.Lens' MultiplexSummary (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
msTags = Lens.lens (tags :: MultiplexSummary -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: MultiplexSummary)
{-# DEPRECATED msTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON MultiplexSummary where
  parseJSON =
    Lude.withObject
      "MultiplexSummary"
      ( \x ->
          MultiplexSummary'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "pipelinesRunningCount")
            Lude.<*> (x Lude..:? "availabilityZones" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "programCount")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "multiplexSettings")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
