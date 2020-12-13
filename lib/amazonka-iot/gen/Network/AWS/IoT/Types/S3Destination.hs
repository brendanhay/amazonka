{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.S3Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Destination
  ( S3Destination (..),

    -- * Smart constructor
    mkS3Destination,

    -- * Lenses
    sdPrefix,
    sdBucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the location of updated firmware in S3.
--
-- /See:/ 'mkS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The S3 prefix.
    prefix :: Lude.Maybe Lude.Text,
    -- | The S3 bucket that contains the updated firmware.
    bucket :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Destination' with the minimum fields required to make a request.
--
-- * 'prefix' - The S3 prefix.
-- * 'bucket' - The S3 bucket that contains the updated firmware.
mkS3Destination ::
  S3Destination
mkS3Destination =
  S3Destination' {prefix = Lude.Nothing, bucket = Lude.Nothing}

-- | The S3 prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrefix :: Lens.Lens' S3Destination (Lude.Maybe Lude.Text)
sdPrefix = Lens.lens (prefix :: S3Destination -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: S3Destination)
{-# DEPRECATED sdPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The S3 bucket that contains the updated firmware.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBucket :: Lens.Lens' S3Destination (Lude.Maybe Lude.Text)
sdBucket = Lens.lens (bucket :: S3Destination -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: S3Destination)
{-# DEPRECATED sdBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.FromJSON S3Destination where
  parseJSON =
    Lude.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Lude.<$> (x Lude..:? "prefix") Lude.<*> (x Lude..:? "bucket")
      )

instance Lude.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("prefix" Lude..=) Lude.<$> prefix,
            ("bucket" Lude..=) Lude.<$> bucket
          ]
      )
