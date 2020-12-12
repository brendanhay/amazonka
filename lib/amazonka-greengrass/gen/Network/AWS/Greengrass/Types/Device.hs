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
    dSyncShadow,
    dThingARN,
    dId,
    dCertificateARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a device.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { syncShadow :: Lude.Maybe Lude.Bool,
    thingARN :: Lude.Text,
    id :: Lude.Text,
    certificateARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the certificate associated with the device.
-- * 'id' - A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
-- * 'syncShadow' - If true, the device's local shadow will be automatically synced with the cloud.
-- * 'thingARN' - The thing ARN of the device.
mkDevice ::
  -- | 'thingARN'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'certificateARN'
  Lude.Text ->
  Device
mkDevice pThingARN_ pId_ pCertificateARN_ =
  Device'
    { syncShadow = Lude.Nothing,
      thingARN = pThingARN_,
      id = pId_,
      certificateARN = pCertificateARN_
    }

-- | If true, the device's local shadow will be automatically synced with the cloud.
--
-- /Note:/ Consider using 'syncShadow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSyncShadow :: Lens.Lens' Device (Lude.Maybe Lude.Bool)
dSyncShadow = Lens.lens (syncShadow :: Device -> Lude.Maybe Lude.Bool) (\s a -> s {syncShadow = a} :: Device)
{-# DEPRECATED dSyncShadow "Use generic-lens or generic-optics with 'syncShadow' instead." #-}

-- | The thing ARN of the device.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingARN :: Lens.Lens' Device Lude.Text
dThingARN = Lens.lens (thingARN :: Device -> Lude.Text) (\s a -> s {thingARN = a} :: Device)
{-# DEPRECATED dThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Device Lude.Text
dId = Lens.lens (id :: Device -> Lude.Text) (\s a -> s {id = a} :: Device)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN of the certificate associated with the device.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateARN :: Lens.Lens' Device Lude.Text
dCertificateARN = Lens.lens (certificateARN :: Device -> Lude.Text) (\s a -> s {certificateARN = a} :: Device)
{-# DEPRECATED dCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.FromJSON Device where
  parseJSON =
    Lude.withObject
      "Device"
      ( \x ->
          Device'
            Lude.<$> (x Lude..:? "SyncShadow")
            Lude.<*> (x Lude..: "ThingArn")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "CertificateArn")
      )

instance Lude.ToJSON Device where
  toJSON Device' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SyncShadow" Lude..=) Lude.<$> syncShadow,
            Lude.Just ("ThingArn" Lude..= thingARN),
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("CertificateArn" Lude..= certificateARN)
          ]
      )
