-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.ReplayDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ReplayDestination
  ( ReplayDestination (..),

    -- * Smart constructor
    mkReplayDestination,

    -- * Lenses
    rdFilterARNs,
    rdARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A @ReplayDestination@ object that contains details about a replay.
--
-- /See:/ 'mkReplayDestination' smart constructor.
data ReplayDestination = ReplayDestination'
  { filterARNs ::
      Lude.Maybe [Lude.Text],
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplayDestination' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the event bus to replay event to. You can replay events only to the event bus specified to create the archive.
-- * 'filterARNs' - A list of ARNs for rules to replay events to.
mkReplayDestination ::
  -- | 'arn'
  Lude.Text ->
  ReplayDestination
mkReplayDestination pARN_ =
  ReplayDestination' {filterARNs = Lude.Nothing, arn = pARN_}

-- | A list of ARNs for rules to replay events to.
--
-- /Note:/ Consider using 'filterARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdFilterARNs :: Lens.Lens' ReplayDestination (Lude.Maybe [Lude.Text])
rdFilterARNs = Lens.lens (filterARNs :: ReplayDestination -> Lude.Maybe [Lude.Text]) (\s a -> s {filterARNs = a} :: ReplayDestination)
{-# DEPRECATED rdFilterARNs "Use generic-lens or generic-optics with 'filterARNs' instead." #-}

-- | The ARN of the event bus to replay event to. You can replay events only to the event bus specified to create the archive.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdARN :: Lens.Lens' ReplayDestination Lude.Text
rdARN = Lens.lens (arn :: ReplayDestination -> Lude.Text) (\s a -> s {arn = a} :: ReplayDestination)
{-# DEPRECATED rdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON ReplayDestination where
  parseJSON =
    Lude.withObject
      "ReplayDestination"
      ( \x ->
          ReplayDestination'
            Lude.<$> (x Lude..:? "FilterArns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Arn")
      )

instance Lude.ToJSON ReplayDestination where
  toJSON ReplayDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterArns" Lude..=) Lude.<$> filterARNs,
            Lude.Just ("Arn" Lude..= arn)
          ]
      )
