{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusDetails
  ( VolumeStatusDetails (..),

    -- * Smart constructor
    mkVolumeStatusDetails,

    -- * Lenses
    vsdName,
    vsdStatus,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.VolumeStatusName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a volume status.
--
-- /See:/ 'mkVolumeStatusDetails' smart constructor.
data VolumeStatusDetails = VolumeStatusDetails'
  { -- | The name of the volume status.
    name :: Core.Maybe Types.VolumeStatusName,
    -- | The intended status of the volume status.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeStatusDetails' value with any optional fields omitted.
mkVolumeStatusDetails ::
  VolumeStatusDetails
mkVolumeStatusDetails =
  VolumeStatusDetails' {name = Core.Nothing, status = Core.Nothing}

-- | The name of the volume status.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsdName :: Lens.Lens' VolumeStatusDetails (Core.Maybe Types.VolumeStatusName)
vsdName = Lens.field @"name"
{-# DEPRECATED vsdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The intended status of the volume status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsdStatus :: Lens.Lens' VolumeStatusDetails (Core.Maybe Types.String)
vsdStatus = Lens.field @"status"
{-# DEPRECATED vsdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML VolumeStatusDetails where
  parseXML x =
    VolumeStatusDetails'
      Core.<$> (x Core..@? "name") Core.<*> (x Core..@? "status")
