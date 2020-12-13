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
    dCertificateARN,
    dThingARN,
    dSyncShadow,
    dId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a device.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { -- | The ARN of the certificate associated with the device.
    certificateARN :: Lude.Text,
    -- | The thing ARN of the device.
    thingARN :: Lude.Text,
    -- | If true, the device's local shadow will be automatically synced with the cloud.
    syncShadow :: Lude.Maybe Lude.Bool,
    -- | A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the certificate associated with the device.
-- * 'thingARN' - The thing ARN of the device.
-- * 'syncShadow' - If true, the device's local shadow will be automatically synced with the cloud.
-- * 'id' - A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
mkDevice ::
  -- | 'certificateARN'
  Lude.Text ->
  -- | 'thingARN'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  Device
mkDevice pCertificateARN_ pThingARN_ pId_ =
  Device'
    { certificateARN = pCertificateARN_,
      thingARN = pThingARN_,
      syncShadow = Lude.Nothing,
      id = pId_
    }

-- | The ARN of the certificate associated with the device.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateARN :: Lens.Lens' Device Lude.Text
dCertificateARN = Lens.lens (certificateARN :: Device -> Lude.Text) (\s a -> s {certificateARN = a} :: Device)
{-# DEPRECATED dCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The thing ARN of the device.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingARN :: Lens.Lens' Device Lude.Text
dThingARN = Lens.lens (thingARN :: Device -> Lude.Text) (\s a -> s {thingARN = a} :: Device)
{-# DEPRECATED dThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | If true, the device's local shadow will be automatically synced with the cloud.
--
-- /Note:/ Consider using 'syncShadow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSyncShadow :: Lens.Lens' Device (Lude.Maybe Lude.Bool)
dSyncShadow = Lens.lens (syncShadow :: Device -> Lude.Maybe Lude.Bool) (\s a -> s {syncShadow = a} :: Device)
{-# DEPRECATED dSyncShadow "Use generic-lens or generic-optics with 'syncShadow' instead." #-}

-- | A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Device Lude.Text
dId = Lens.lens (id :: Device -> Lude.Text) (\s a -> s {id = a} :: Device)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Device where
  parseJSON =
    Lude.withObject
      "Device"
      ( \x ->
          Device'
            Lude.<$> (x Lude..: "CertificateArn")
            Lude.<*> (x Lude..: "ThingArn")
            Lude.<*> (x Lude..:? "SyncShadow")
            Lude.<*> (x Lude..: "Id")
      )

instance Lude.ToJSON Device where
  toJSON Device' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CertificateArn" Lude..= certificateARN),
            Lude.Just ("ThingArn" Lude..= thingARN),
            ("SyncShadow" Lude..=) Lude.<$> syncShadow,
            Lude.Just ("Id" Lude..= id)
          ]
      )
