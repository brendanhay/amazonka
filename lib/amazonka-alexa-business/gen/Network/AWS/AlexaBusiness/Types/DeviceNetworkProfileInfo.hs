{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
  ( DeviceNetworkProfileInfo (..)
  -- * Smart constructor
  , mkDeviceNetworkProfileInfo
  -- * Lenses
  , dnpiCertificateArn
  , dnpiCertificateExpirationTime
  , dnpiNetworkProfileArn
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed information about a device's network profile.
--
-- /See:/ 'mkDeviceNetworkProfileInfo' smart constructor.
data DeviceNetworkProfileInfo = DeviceNetworkProfileInfo'
  { certificateArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the certificate associated with a device.
  , certificateExpirationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time (in epoch) when the certificate expires.
  , networkProfileArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the network profile associated with a device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeviceNetworkProfileInfo' value with any optional fields omitted.
mkDeviceNetworkProfileInfo
    :: DeviceNetworkProfileInfo
mkDeviceNetworkProfileInfo
  = DeviceNetworkProfileInfo'{certificateArn = Core.Nothing,
                              certificateExpirationTime = Core.Nothing,
                              networkProfileArn = Core.Nothing}

-- | The ARN of the certificate associated with a device.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpiCertificateArn :: Lens.Lens' DeviceNetworkProfileInfo (Core.Maybe Types.Arn)
dnpiCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE dnpiCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The time (in epoch) when the certificate expires.
--
-- /Note:/ Consider using 'certificateExpirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpiCertificateExpirationTime :: Lens.Lens' DeviceNetworkProfileInfo (Core.Maybe Core.NominalDiffTime)
dnpiCertificateExpirationTime = Lens.field @"certificateExpirationTime"
{-# INLINEABLE dnpiCertificateExpirationTime #-}
{-# DEPRECATED certificateExpirationTime "Use generic-lens or generic-optics with 'certificateExpirationTime' instead"  #-}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpiNetworkProfileArn :: Lens.Lens' DeviceNetworkProfileInfo (Core.Maybe Types.Arn)
dnpiNetworkProfileArn = Lens.field @"networkProfileArn"
{-# INLINEABLE dnpiNetworkProfileArn #-}
{-# DEPRECATED networkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead"  #-}

instance Core.FromJSON DeviceNetworkProfileInfo where
        parseJSON
          = Core.withObject "DeviceNetworkProfileInfo" Core.$
              \ x ->
                DeviceNetworkProfileInfo' Core.<$>
                  (x Core..:? "CertificateArn") Core.<*>
                    x Core..:? "CertificateExpirationTime"
                    Core.<*> x Core..:? "NetworkProfileArn"
