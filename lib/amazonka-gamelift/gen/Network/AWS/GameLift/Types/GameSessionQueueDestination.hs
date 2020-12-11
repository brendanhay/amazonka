-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionQueueDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionQueueDestination
  ( GameSessionQueueDestination (..),

    -- * Smart constructor
    mkGameSessionQueueDestination,

    -- * Lenses
    gsqdDestinationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Fleet designated in a game session queue. Requests for new game sessions in the queue are fulfilled by starting a new game session on any destination that is configured for a queue.
--
--
--     * 'CreateGameSessionQueue'
--
--
--     * 'DescribeGameSessionQueues'
--
--
--     * 'UpdateGameSessionQueue'
--
--
--     * 'DeleteGameSessionQueue'
--
--
--
-- /See:/ 'mkGameSessionQueueDestination' smart constructor.
newtype GameSessionQueueDestination = GameSessionQueueDestination'
  { destinationARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameSessionQueueDestination' with the minimum fields required to make a request.
--
-- * 'destinationARN' - The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a Region name, provide a unique identifier across all Regions.
mkGameSessionQueueDestination ::
  GameSessionQueueDestination
mkGameSessionQueueDestination =
  GameSessionQueueDestination' {destinationARN = Lude.Nothing}

-- | The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a Region name, provide a unique identifier across all Regions.
--
-- /Note:/ Consider using 'destinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqdDestinationARN :: Lens.Lens' GameSessionQueueDestination (Lude.Maybe Lude.Text)
gsqdDestinationARN = Lens.lens (destinationARN :: GameSessionQueueDestination -> Lude.Maybe Lude.Text) (\s a -> s {destinationARN = a} :: GameSessionQueueDestination)
{-# DEPRECATED gsqdDestinationARN "Use generic-lens or generic-optics with 'destinationARN' instead." #-}

instance Lude.FromJSON GameSessionQueueDestination where
  parseJSON =
    Lude.withObject
      "GameSessionQueueDestination"
      ( \x ->
          GameSessionQueueDestination'
            Lude.<$> (x Lude..:? "DestinationArn")
      )

instance Lude.ToJSON GameSessionQueueDestination where
  toJSON GameSessionQueueDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DestinationArn" Lude..=) Lude.<$> destinationARN]
      )
