{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
  ( ScheduledInstancesBlockDeviceMapping (..),

    -- * Smart constructor
    mkScheduledInstancesBlockDeviceMapping,

    -- * Lenses
    sibdmDeviceName,
    sibdmEbs,
    sibdmNoDevice,
    sibdmVirtualName,
  )
where

import qualified Network.AWS.EC2.Types.ScheduledInstancesEbs as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device mapping for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesBlockDeviceMapping' smart constructor.
data ScheduledInstancesBlockDeviceMapping = ScheduledInstancesBlockDeviceMapping'
  { -- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
    deviceName :: Core.Maybe Types.String,
    -- | Parameters used to set up EBS volumes automatically when the instance is launched.
    ebs :: Core.Maybe Types.ScheduledInstancesEbs,
    -- | Suppresses the specified device included in the block device mapping of the AMI.
    noDevice :: Core.Maybe Types.String,
    -- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with two available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
    --
    -- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
    virtualName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstancesBlockDeviceMapping' value with any optional fields omitted.
mkScheduledInstancesBlockDeviceMapping ::
  ScheduledInstancesBlockDeviceMapping
mkScheduledInstancesBlockDeviceMapping =
  ScheduledInstancesBlockDeviceMapping'
    { deviceName = Core.Nothing,
      ebs = Core.Nothing,
      noDevice = Core.Nothing,
      virtualName = Core.Nothing
    }

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibdmDeviceName :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Core.Maybe Types.String)
sibdmDeviceName = Lens.field @"deviceName"
{-# DEPRECATED sibdmDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | Parameters used to set up EBS volumes automatically when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibdmEbs :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Core.Maybe Types.ScheduledInstancesEbs)
sibdmEbs = Lens.field @"ebs"
{-# DEPRECATED sibdmEbs "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | Suppresses the specified device included in the block device mapping of the AMI.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibdmNoDevice :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Core.Maybe Types.String)
sibdmNoDevice = Lens.field @"noDevice"
{-# DEPRECATED sibdmNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with two available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibdmVirtualName :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Core.Maybe Types.String)
sibdmVirtualName = Lens.field @"virtualName"
{-# DEPRECATED sibdmVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}
