{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SigningProfileParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SigningProfileParameter
  ( SigningProfileParameter (..),

    -- * Smart constructor
    mkSigningProfileParameter,

    -- * Lenses
    sppCertificateArn,
    sppCertificatePathOnDevice,
    sppPlatform,
  )
where

import qualified Network.AWS.IoT.Types.CertificateArn as Types
import qualified Network.AWS.IoT.Types.CertificatePathOnDevice as Types
import qualified Network.AWS.IoT.Types.Platform as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the code-signing profile.
--
-- /See:/ 'mkSigningProfileParameter' smart constructor.
data SigningProfileParameter = SigningProfileParameter'
  { -- | Certificate ARN.
    certificateArn :: Core.Maybe Types.CertificateArn,
    -- | The location of the code-signing certificate on your device.
    certificatePathOnDevice :: Core.Maybe Types.CertificatePathOnDevice,
    -- | The hardware platform of your device.
    platform :: Core.Maybe Types.Platform
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SigningProfileParameter' value with any optional fields omitted.
mkSigningProfileParameter ::
  SigningProfileParameter
mkSigningProfileParameter =
  SigningProfileParameter'
    { certificateArn = Core.Nothing,
      certificatePathOnDevice = Core.Nothing,
      platform = Core.Nothing
    }

-- | Certificate ARN.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppCertificateArn :: Lens.Lens' SigningProfileParameter (Core.Maybe Types.CertificateArn)
sppCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED sppCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The location of the code-signing certificate on your device.
--
-- /Note:/ Consider using 'certificatePathOnDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppCertificatePathOnDevice :: Lens.Lens' SigningProfileParameter (Core.Maybe Types.CertificatePathOnDevice)
sppCertificatePathOnDevice = Lens.field @"certificatePathOnDevice"
{-# DEPRECATED sppCertificatePathOnDevice "Use generic-lens or generic-optics with 'certificatePathOnDevice' instead." #-}

-- | The hardware platform of your device.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPlatform :: Lens.Lens' SigningProfileParameter (Core.Maybe Types.Platform)
sppPlatform = Lens.field @"platform"
{-# DEPRECATED sppPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Core.FromJSON SigningProfileParameter where
  toJSON SigningProfileParameter {..} =
    Core.object
      ( Core.catMaybes
          [ ("certificateArn" Core..=) Core.<$> certificateArn,
            ("certificatePathOnDevice" Core..=)
              Core.<$> certificatePathOnDevice,
            ("platform" Core..=) Core.<$> platform
          ]
      )

instance Core.FromJSON SigningProfileParameter where
  parseJSON =
    Core.withObject "SigningProfileParameter" Core.$
      \x ->
        SigningProfileParameter'
          Core.<$> (x Core..:? "certificateArn")
          Core.<*> (x Core..:? "certificatePathOnDevice")
          Core.<*> (x Core..:? "platform")
