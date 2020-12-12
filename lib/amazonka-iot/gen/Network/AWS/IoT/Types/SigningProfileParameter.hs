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
    sppPlatform,
    sppCertificateARN,
    sppCertificatePathOnDevice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the code-signing profile.
--
-- /See:/ 'mkSigningProfileParameter' smart constructor.
data SigningProfileParameter = SigningProfileParameter'
  { platform ::
      Lude.Maybe Lude.Text,
    certificateARN :: Lude.Maybe Lude.Text,
    certificatePathOnDevice ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SigningProfileParameter' with the minimum fields required to make a request.
--
-- * 'certificateARN' - Certificate ARN.
-- * 'certificatePathOnDevice' - The location of the code-signing certificate on your device.
-- * 'platform' - The hardware platform of your device.
mkSigningProfileParameter ::
  SigningProfileParameter
mkSigningProfileParameter =
  SigningProfileParameter'
    { platform = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificatePathOnDevice = Lude.Nothing
    }

-- | The hardware platform of your device.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPlatform :: Lens.Lens' SigningProfileParameter (Lude.Maybe Lude.Text)
sppPlatform = Lens.lens (platform :: SigningProfileParameter -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: SigningProfileParameter)
{-# DEPRECATED sppPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Certificate ARN.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppCertificateARN :: Lens.Lens' SigningProfileParameter (Lude.Maybe Lude.Text)
sppCertificateARN = Lens.lens (certificateARN :: SigningProfileParameter -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: SigningProfileParameter)
{-# DEPRECATED sppCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The location of the code-signing certificate on your device.
--
-- /Note:/ Consider using 'certificatePathOnDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppCertificatePathOnDevice :: Lens.Lens' SigningProfileParameter (Lude.Maybe Lude.Text)
sppCertificatePathOnDevice = Lens.lens (certificatePathOnDevice :: SigningProfileParameter -> Lude.Maybe Lude.Text) (\s a -> s {certificatePathOnDevice = a} :: SigningProfileParameter)
{-# DEPRECATED sppCertificatePathOnDevice "Use generic-lens or generic-optics with 'certificatePathOnDevice' instead." #-}

instance Lude.FromJSON SigningProfileParameter where
  parseJSON =
    Lude.withObject
      "SigningProfileParameter"
      ( \x ->
          SigningProfileParameter'
            Lude.<$> (x Lude..:? "platform")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "certificatePathOnDevice")
      )

instance Lude.ToJSON SigningProfileParameter where
  toJSON SigningProfileParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("platform" Lude..=) Lude.<$> platform,
            ("certificateArn" Lude..=) Lude.<$> certificateARN,
            ("certificatePathOnDevice" Lude..=)
              Lude.<$> certificatePathOnDevice
          ]
      )
