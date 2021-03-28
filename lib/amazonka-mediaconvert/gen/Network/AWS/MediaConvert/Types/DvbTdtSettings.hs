{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbTdtSettings
  ( DvbTdtSettings (..)
  -- * Smart constructor
  , mkDvbTdtSettings
  -- * Lenses
  , dtsTdtInterval
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
--
-- /See:/ 'mkDvbTdtSettings' smart constructor.
newtype DvbTdtSettings = DvbTdtSettings'
  { tdtInterval :: Core.Maybe Core.Natural
    -- ^ The number of milliseconds between instances of this table in the output transport stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DvbTdtSettings' value with any optional fields omitted.
mkDvbTdtSettings
    :: DvbTdtSettings
mkDvbTdtSettings = DvbTdtSettings'{tdtInterval = Core.Nothing}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'tdtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsTdtInterval :: Lens.Lens' DvbTdtSettings (Core.Maybe Core.Natural)
dtsTdtInterval = Lens.field @"tdtInterval"
{-# INLINEABLE dtsTdtInterval #-}
{-# DEPRECATED tdtInterval "Use generic-lens or generic-optics with 'tdtInterval' instead"  #-}

instance Core.FromJSON DvbTdtSettings where
        toJSON DvbTdtSettings{..}
          = Core.object
              (Core.catMaybes [("tdtInterval" Core..=) Core.<$> tdtInterval])

instance Core.FromJSON DvbTdtSettings where
        parseJSON
          = Core.withObject "DvbTdtSettings" Core.$
              \ x -> DvbTdtSettings' Core.<$> (x Core..:? "tdtInterval")
