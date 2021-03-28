{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EbsInstanceBlockDeviceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EbsInstanceBlockDeviceSpecification
  ( EbsInstanceBlockDeviceSpecification (..)
  -- * Smart constructor
  , mkEbsInstanceBlockDeviceSpecification
  -- * Lenses
  , eibdsDeleteOnTermination
  , eibdsVolumeId
  ) where

import qualified Network.AWS.EC2.Types.VolumeId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes information used to set up an EBS volume specified in a block device mapping.
--
-- /See:/ 'mkEbsInstanceBlockDeviceSpecification' smart constructor.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification'
  { deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the volume is deleted on instance termination.
  , volumeId :: Core.Maybe Types.VolumeId
    -- ^ The ID of the EBS volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsInstanceBlockDeviceSpecification' value with any optional fields omitted.
mkEbsInstanceBlockDeviceSpecification
    :: EbsInstanceBlockDeviceSpecification
mkEbsInstanceBlockDeviceSpecification
  = EbsInstanceBlockDeviceSpecification'{deleteOnTermination =
                                           Core.Nothing,
                                         volumeId = Core.Nothing}

-- | Indicates whether the volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdsDeleteOnTermination :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Core.Maybe Core.Bool)
eibdsDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE eibdsDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | The ID of the EBS volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdsVolumeId :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Core.Maybe Types.VolumeId)
eibdsVolumeId = Lens.field @"volumeId"
{-# INLINEABLE eibdsVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

instance Core.ToQuery EbsInstanceBlockDeviceSpecification where
        toQuery EbsInstanceBlockDeviceSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "DeleteOnTermination")
              deleteOnTermination
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VolumeId") volumeId
