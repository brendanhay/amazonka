{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputLocationRef
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.OutputLocationRef
  ( OutputLocationRef (..)
  -- * Smart constructor
  , mkOutputLocationRef
  -- * Lenses
  , olrDestinationRefId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Reference to an OutputDestination ID defined in the channel
--
-- /See:/ 'mkOutputLocationRef' smart constructor.
newtype OutputLocationRef = OutputLocationRef'
  { destinationRefId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputLocationRef' value with any optional fields omitted.
mkOutputLocationRef
    :: OutputLocationRef
mkOutputLocationRef
  = OutputLocationRef'{destinationRefId = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'destinationRefId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olrDestinationRefId :: Lens.Lens' OutputLocationRef (Core.Maybe Core.Text)
olrDestinationRefId = Lens.field @"destinationRefId"
{-# INLINEABLE olrDestinationRefId #-}
{-# DEPRECATED destinationRefId "Use generic-lens or generic-optics with 'destinationRefId' instead"  #-}

instance Core.FromJSON OutputLocationRef where
        toJSON OutputLocationRef{..}
          = Core.object
              (Core.catMaybes
                 [("destinationRefId" Core..=) Core.<$> destinationRefId])

instance Core.FromJSON OutputLocationRef where
        parseJSON
          = Core.withObject "OutputLocationRef" Core.$
              \ x -> OutputLocationRef' Core.<$> (x Core..:? "destinationRefId")
