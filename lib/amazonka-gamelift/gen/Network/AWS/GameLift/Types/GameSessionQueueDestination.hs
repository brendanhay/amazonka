{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionQueueDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameSessionQueueDestination
  ( GameSessionQueueDestination (..)
  -- * Smart constructor
  , mkGameSessionQueueDestination
  -- * Lenses
  , gsqdDestinationArn
  ) where

import qualified Network.AWS.GameLift.Types.DestinationArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { destinationArn :: Core.Maybe Types.DestinationArn
    -- ^ The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a Region name, provide a unique identifier across all Regions. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GameSessionQueueDestination' value with any optional fields omitted.
mkGameSessionQueueDestination
    :: GameSessionQueueDestination
mkGameSessionQueueDestination
  = GameSessionQueueDestination'{destinationArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a Region name, provide a unique identifier across all Regions. 
--
-- /Note:/ Consider using 'destinationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqdDestinationArn :: Lens.Lens' GameSessionQueueDestination (Core.Maybe Types.DestinationArn)
gsqdDestinationArn = Lens.field @"destinationArn"
{-# INLINEABLE gsqdDestinationArn #-}
{-# DEPRECATED destinationArn "Use generic-lens or generic-optics with 'destinationArn' instead"  #-}

instance Core.FromJSON GameSessionQueueDestination where
        toJSON GameSessionQueueDestination{..}
          = Core.object
              (Core.catMaybes
                 [("DestinationArn" Core..=) Core.<$> destinationArn])

instance Core.FromJSON GameSessionQueueDestination where
        parseJSON
          = Core.withObject "GameSessionQueueDestination" Core.$
              \ x ->
                GameSessionQueueDestination' Core.<$> (x Core..:? "DestinationArn")
