{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ProgressEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ProgressEvent
  ( ProgressEvent (..)
  -- * Smart constructor
  , mkProgressEvent
  -- * Lenses
  , peDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Progress as Types

-- | This data type contains information about the progress event of an operation.
--
-- /See:/ 'mkProgressEvent' smart constructor.
newtype ProgressEvent = ProgressEvent'
  { details :: Core.Maybe Types.Progress
    -- ^ The Progress event details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProgressEvent' value with any optional fields omitted.
mkProgressEvent
    :: ProgressEvent
mkProgressEvent = ProgressEvent'{details = Core.Nothing}

-- | The Progress event details.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peDetails :: Lens.Lens' ProgressEvent (Core.Maybe Types.Progress)
peDetails = Lens.field @"details"
{-# INLINEABLE peDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

instance Core.FromXML ProgressEvent where
        parseXML x = ProgressEvent' Core.<$> (x Core..@? "Details")
