{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.OnSuccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.OnSuccess
  ( OnSuccess (..)
  -- * Smart constructor
  , mkOnSuccess
  -- * Lenses
  , osDestination
  ) where

import qualified Network.AWS.Lambda.Types.Destination as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A destination for events that were processed successfully.
--
-- /See:/ 'mkOnSuccess' smart constructor.
newtype OnSuccess = OnSuccess'
  { destination :: Core.Maybe Types.Destination
    -- ^ The Amazon Resource Name (ARN) of the destination resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OnSuccess' value with any optional fields omitted.
mkOnSuccess
    :: OnSuccess
mkOnSuccess = OnSuccess'{destination = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDestination :: Lens.Lens' OnSuccess (Core.Maybe Types.Destination)
osDestination = Lens.field @"destination"
{-# INLINEABLE osDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

instance Core.FromJSON OnSuccess where
        toJSON OnSuccess{..}
          = Core.object
              (Core.catMaybes [("Destination" Core..=) Core.<$> destination])

instance Core.FromJSON OnSuccess where
        parseJSON
          = Core.withObject "OnSuccess" Core.$
              \ x -> OnSuccess' Core.<$> (x Core..:? "Destination")
