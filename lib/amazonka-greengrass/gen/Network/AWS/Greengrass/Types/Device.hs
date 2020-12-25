{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Device
  ( Device (..),

    -- * Smart constructor
    mkDevice,

    -- * Lenses
    dThingArn,
    dId,
    dCertificateArn,
    dSyncShadow,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a device.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { -- | The thing ARN of the device.
    thingArn :: Core.Text,
    -- | A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
    id :: Core.Text,
    -- | The ARN of the certificate associated with the device.
    certificateArn :: Core.Text,
    -- | If true, the device's local shadow will be automatically synced with the cloud.
    syncShadow :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Device' value with any optional fields omitted.
mkDevice ::
  -- | 'thingArn'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  -- | 'certificateArn'
  Core.Text ->
  Device
mkDevice thingArn id certificateArn =
  Device' {thingArn, id, certificateArn, syncShadow = Core.Nothing}

-- | The thing ARN of the device.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingArn :: Lens.Lens' Device Core.Text
dThingArn = Lens.field @"thingArn"
{-# DEPRECATED dThingArn "Use generic-lens or generic-optics with 'thingArn' instead." #-}

-- | A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Device Core.Text
dId = Lens.field @"id"
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN of the certificate associated with the device.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateArn :: Lens.Lens' Device Core.Text
dCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED dCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | If true, the device's local shadow will be automatically synced with the cloud.
--
-- /Note:/ Consider using 'syncShadow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSyncShadow :: Lens.Lens' Device (Core.Maybe Core.Bool)
dSyncShadow = Lens.field @"syncShadow"
{-# DEPRECATED dSyncShadow "Use generic-lens or generic-optics with 'syncShadow' instead." #-}

instance Core.FromJSON Device where
  toJSON Device {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ThingArn" Core..= thingArn),
            Core.Just ("Id" Core..= id),
            Core.Just ("CertificateArn" Core..= certificateArn),
            ("SyncShadow" Core..=) Core.<$> syncShadow
          ]
      )

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject "Device" Core.$
      \x ->
        Device'
          Core.<$> (x Core..: "ThingArn")
          Core.<*> (x Core..: "Id")
          Core.<*> (x Core..: "CertificateArn")
          Core.<*> (x Core..:? "SyncShadow")
