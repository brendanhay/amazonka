{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeDetail
  ( VolumeDetail (..)
  -- * Smart constructor
  , mkVolumeDetail
  -- * Lenses
  , vdSize
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an EBS volume.
--
-- /See:/ 'mkVolumeDetail' smart constructor.
newtype VolumeDetail = VolumeDetail'
  { size :: Core.Integer
    -- ^ The size of the volume, in GiB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeDetail' value with any optional fields omitted.
mkVolumeDetail
    :: Core.Integer -- ^ 'size'
    -> VolumeDetail
mkVolumeDetail size = VolumeDetail'{size}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdSize :: Lens.Lens' VolumeDetail Core.Integer
vdSize = Lens.field @"size"
{-# INLINEABLE vdSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

instance Core.ToQuery VolumeDetail where
        toQuery VolumeDetail{..} = Core.toQueryPair "Size" size
