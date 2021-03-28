{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeStatusInfo
  ( VolumeStatusInfo (..)
  -- * Smart constructor
  , mkVolumeStatusInfo
  -- * Lenses
  , vsiDetails
  , vsiStatus
  ) where

import qualified Network.AWS.EC2.Types.VolumeStatusDetails as Types
import qualified Network.AWS.EC2.Types.VolumeStatusInfoStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of a volume.
--
-- /See:/ 'mkVolumeStatusInfo' smart constructor.
data VolumeStatusInfo = VolumeStatusInfo'
  { details :: Core.Maybe [Types.VolumeStatusDetails]
    -- ^ The details of the volume status.
  , status :: Core.Maybe Types.VolumeStatusInfoStatus
    -- ^ The status of the volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeStatusInfo' value with any optional fields omitted.
mkVolumeStatusInfo
    :: VolumeStatusInfo
mkVolumeStatusInfo
  = VolumeStatusInfo'{details = Core.Nothing, status = Core.Nothing}

-- | The details of the volume status.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiDetails :: Lens.Lens' VolumeStatusInfo (Core.Maybe [Types.VolumeStatusDetails])
vsiDetails = Lens.field @"details"
{-# INLINEABLE vsiDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The status of the volume.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiStatus :: Lens.Lens' VolumeStatusInfo (Core.Maybe Types.VolumeStatusInfoStatus)
vsiStatus = Lens.field @"status"
{-# INLINEABLE vsiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML VolumeStatusInfo where
        parseXML x
          = VolumeStatusInfo' Core.<$>
              (x Core..@? "details" Core..<@> Core.parseXMLList "item") Core.<*>
                x Core..@? "status"
