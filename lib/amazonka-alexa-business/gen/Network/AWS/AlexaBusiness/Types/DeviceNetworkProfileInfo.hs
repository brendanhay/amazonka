{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
  ( DeviceNetworkProfileInfo (..),

    -- * Smart constructor
    mkDeviceNetworkProfileInfo,

    -- * Lenses
    dnpiCertificateARN,
    dnpiNetworkProfileARN,
    dnpiCertificateExpirationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about a device's network profile.
--
-- /See:/ 'mkDeviceNetworkProfileInfo' smart constructor.
data DeviceNetworkProfileInfo = DeviceNetworkProfileInfo'
  { -- | The ARN of the certificate associated with a device.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the network profile associated with a device.
    networkProfileARN :: Lude.Maybe Lude.Text,
    -- | The time (in epoch) when the certificate expires.
    certificateExpirationTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceNetworkProfileInfo' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the certificate associated with a device.
-- * 'networkProfileARN' - The ARN of the network profile associated with a device.
-- * 'certificateExpirationTime' - The time (in epoch) when the certificate expires.
mkDeviceNetworkProfileInfo ::
  DeviceNetworkProfileInfo
mkDeviceNetworkProfileInfo =
  DeviceNetworkProfileInfo'
    { certificateARN = Lude.Nothing,
      networkProfileARN = Lude.Nothing,
      certificateExpirationTime = Lude.Nothing
    }

-- | The ARN of the certificate associated with a device.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpiCertificateARN :: Lens.Lens' DeviceNetworkProfileInfo (Lude.Maybe Lude.Text)
dnpiCertificateARN = Lens.lens (certificateARN :: DeviceNetworkProfileInfo -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: DeviceNetworkProfileInfo)
{-# DEPRECATED dnpiCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpiNetworkProfileARN :: Lens.Lens' DeviceNetworkProfileInfo (Lude.Maybe Lude.Text)
dnpiNetworkProfileARN = Lens.lens (networkProfileARN :: DeviceNetworkProfileInfo -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileARN = a} :: DeviceNetworkProfileInfo)
{-# DEPRECATED dnpiNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

-- | The time (in epoch) when the certificate expires.
--
-- /Note:/ Consider using 'certificateExpirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpiCertificateExpirationTime :: Lens.Lens' DeviceNetworkProfileInfo (Lude.Maybe Lude.Timestamp)
dnpiCertificateExpirationTime = Lens.lens (certificateExpirationTime :: DeviceNetworkProfileInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {certificateExpirationTime = a} :: DeviceNetworkProfileInfo)
{-# DEPRECATED dnpiCertificateExpirationTime "Use generic-lens or generic-optics with 'certificateExpirationTime' instead." #-}

instance Lude.FromJSON DeviceNetworkProfileInfo where
  parseJSON =
    Lude.withObject
      "DeviceNetworkProfileInfo"
      ( \x ->
          DeviceNetworkProfileInfo'
            Lude.<$> (x Lude..:? "CertificateArn")
            Lude.<*> (x Lude..:? "NetworkProfileArn")
            Lude.<*> (x Lude..:? "CertificateExpirationTime")
      )
