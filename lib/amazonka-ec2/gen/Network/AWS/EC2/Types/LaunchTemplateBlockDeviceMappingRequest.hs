{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
  ( LaunchTemplateBlockDeviceMappingRequest (..),

    -- * Smart constructor
    mkLaunchTemplateBlockDeviceMappingRequest,

    -- * Lenses
    ltbdmrDeviceName,
    ltbdmrEbs,
    ltbdmrNoDevice,
    ltbdmrVirtualName,
  )
where

import qualified Network.AWS.EC2.Types.LaunchTemplateEbsBlockDeviceRequest as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device mapping.
--
-- /See:/ 'mkLaunchTemplateBlockDeviceMappingRequest' smart constructor.
data LaunchTemplateBlockDeviceMappingRequest = LaunchTemplateBlockDeviceMappingRequest'
  { -- | The device name (for example, /dev/sdh or xvdh).
    deviceName :: Core.Maybe Types.String,
    -- | Parameters used to automatically set up EBS volumes when the instance is launched.
    ebs :: Core.Maybe Types.LaunchTemplateEbsBlockDeviceRequest,
    -- | Suppresses the specified device included in the block device mapping of the AMI.
    noDevice :: Core.Maybe Types.String,
    -- | The virtual device name (ephemeralN). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for ephemeral0 and ephemeral1. The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
    virtualName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateBlockDeviceMappingRequest' value with any optional fields omitted.
mkLaunchTemplateBlockDeviceMappingRequest ::
  LaunchTemplateBlockDeviceMappingRequest
mkLaunchTemplateBlockDeviceMappingRequest =
  LaunchTemplateBlockDeviceMappingRequest'
    { deviceName =
        Core.Nothing,
      ebs = Core.Nothing,
      noDevice = Core.Nothing,
      virtualName = Core.Nothing
    }

-- | The device name (for example, /dev/sdh or xvdh).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmrDeviceName :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Core.Maybe Types.String)
ltbdmrDeviceName = Lens.field @"deviceName"
{-# DEPRECATED ltbdmrDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmrEbs :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Core.Maybe Types.LaunchTemplateEbsBlockDeviceRequest)
ltbdmrEbs = Lens.field @"ebs"
{-# DEPRECATED ltbdmrEbs "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | Suppresses the specified device included in the block device mapping of the AMI.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmrNoDevice :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Core.Maybe Types.String)
ltbdmrNoDevice = Lens.field @"noDevice"
{-# DEPRECATED ltbdmrNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | The virtual device name (ephemeralN). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for ephemeral0 and ephemeral1. The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmrVirtualName :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Core.Maybe Types.String)
ltbdmrVirtualName = Lens.field @"virtualName"
{-# DEPRECATED ltbdmrVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}
