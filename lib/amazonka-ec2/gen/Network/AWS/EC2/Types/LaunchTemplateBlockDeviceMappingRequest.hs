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
    ltbdmrVirtualName,
    ltbdmrNoDevice,
    ltbdmrEBS,
    ltbdmrDeviceName,
  )
where

import Network.AWS.EC2.Types.LaunchTemplateEBSBlockDeviceRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device mapping.
--
-- /See:/ 'mkLaunchTemplateBlockDeviceMappingRequest' smart constructor.
data LaunchTemplateBlockDeviceMappingRequest = LaunchTemplateBlockDeviceMappingRequest'
  { virtualName ::
      Lude.Maybe
        Lude.Text,
    noDevice ::
      Lude.Maybe
        Lude.Text,
    ebs ::
      Lude.Maybe
        LaunchTemplateEBSBlockDeviceRequest,
    deviceName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateBlockDeviceMappingRequest' with the minimum fields required to make a request.
--
-- * 'deviceName' - The device name (for example, /dev/sdh or xvdh).
-- * 'ebs' - Parameters used to automatically set up EBS volumes when the instance is launched.
-- * 'noDevice' - Suppresses the specified device included in the block device mapping of the AMI.
-- * 'virtualName' - The virtual device name (ephemeralN). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for ephemeral0 and ephemeral1. The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
mkLaunchTemplateBlockDeviceMappingRequest ::
  LaunchTemplateBlockDeviceMappingRequest
mkLaunchTemplateBlockDeviceMappingRequest =
  LaunchTemplateBlockDeviceMappingRequest'
    { virtualName =
        Lude.Nothing,
      noDevice = Lude.Nothing,
      ebs = Lude.Nothing,
      deviceName = Lude.Nothing
    }

-- | The virtual device name (ephemeralN). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for ephemeral0 and ephemeral1. The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmrVirtualName :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Lude.Maybe Lude.Text)
ltbdmrVirtualName = Lens.lens (virtualName :: LaunchTemplateBlockDeviceMappingRequest -> Lude.Maybe Lude.Text) (\s a -> s {virtualName = a} :: LaunchTemplateBlockDeviceMappingRequest)
{-# DEPRECATED ltbdmrVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}

-- | Suppresses the specified device included in the block device mapping of the AMI.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmrNoDevice :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Lude.Maybe Lude.Text)
ltbdmrNoDevice = Lens.lens (noDevice :: LaunchTemplateBlockDeviceMappingRequest -> Lude.Maybe Lude.Text) (\s a -> s {noDevice = a} :: LaunchTemplateBlockDeviceMappingRequest)
{-# DEPRECATED ltbdmrNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmrEBS :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Lude.Maybe LaunchTemplateEBSBlockDeviceRequest)
ltbdmrEBS = Lens.lens (ebs :: LaunchTemplateBlockDeviceMappingRequest -> Lude.Maybe LaunchTemplateEBSBlockDeviceRequest) (\s a -> s {ebs = a} :: LaunchTemplateBlockDeviceMappingRequest)
{-# DEPRECATED ltbdmrEBS "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | The device name (for example, /dev/sdh or xvdh).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmrDeviceName :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Lude.Maybe Lude.Text)
ltbdmrDeviceName = Lens.lens (deviceName :: LaunchTemplateBlockDeviceMappingRequest -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: LaunchTemplateBlockDeviceMappingRequest)
{-# DEPRECATED ltbdmrDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.ToQuery LaunchTemplateBlockDeviceMappingRequest where
  toQuery LaunchTemplateBlockDeviceMappingRequest' {..} =
    Lude.mconcat
      [ "VirtualName" Lude.=: virtualName,
        "NoDevice" Lude.=: noDevice,
        "Ebs" Lude.=: ebs,
        "DeviceName" Lude.=: deviceName
      ]
