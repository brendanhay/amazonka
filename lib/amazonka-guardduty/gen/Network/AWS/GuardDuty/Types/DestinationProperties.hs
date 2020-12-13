{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DestinationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DestinationProperties
  ( DestinationProperties (..),

    -- * Smart constructor
    mkDestinationProperties,

    -- * Lenses
    dpKMSKeyARN,
    dpDestinationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the Amazon Resource Name (ARN) of the resource to publish to, such as an S3 bucket, and the ARN of the KMS key to use to encrypt published findings.
--
-- /See:/ 'mkDestinationProperties' smart constructor.
data DestinationProperties = DestinationProperties'
  { -- | The ARN of the KMS key to use for encryption.
    kmsKeyARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the resource to publish to.
    destinationARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DestinationProperties' with the minimum fields required to make a request.
--
-- * 'kmsKeyARN' - The ARN of the KMS key to use for encryption.
-- * 'destinationARN' - The ARN of the resource to publish to.
mkDestinationProperties ::
  DestinationProperties
mkDestinationProperties =
  DestinationProperties'
    { kmsKeyARN = Lude.Nothing,
      destinationARN = Lude.Nothing
    }

-- | The ARN of the KMS key to use for encryption.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpKMSKeyARN :: Lens.Lens' DestinationProperties (Lude.Maybe Lude.Text)
dpKMSKeyARN = Lens.lens (kmsKeyARN :: DestinationProperties -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: DestinationProperties)
{-# DEPRECATED dpKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The ARN of the resource to publish to.
--
-- /Note:/ Consider using 'destinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDestinationARN :: Lens.Lens' DestinationProperties (Lude.Maybe Lude.Text)
dpDestinationARN = Lens.lens (destinationARN :: DestinationProperties -> Lude.Maybe Lude.Text) (\s a -> s {destinationARN = a} :: DestinationProperties)
{-# DEPRECATED dpDestinationARN "Use generic-lens or generic-optics with 'destinationARN' instead." #-}

instance Lude.FromJSON DestinationProperties where
  parseJSON =
    Lude.withObject
      "DestinationProperties"
      ( \x ->
          DestinationProperties'
            Lude.<$> (x Lude..:? "kmsKeyArn") Lude.<*> (x Lude..:? "destinationArn")
      )

instance Lude.ToJSON DestinationProperties where
  toJSON DestinationProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("kmsKeyArn" Lude..=) Lude.<$> kmsKeyARN,
            ("destinationArn" Lude..=) Lude.<$> destinationARN
          ]
      )
