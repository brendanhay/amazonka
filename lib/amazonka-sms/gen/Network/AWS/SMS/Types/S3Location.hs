{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slBucket,
    slKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Location of an Amazon S3 object.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The Amazon S3 bucket name.
    bucket :: Lude.Maybe Lude.Text,
    -- | The Amazon S3 bucket key.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- * 'bucket' - The Amazon S3 bucket name.
-- * 'key' - The Amazon S3 bucket key.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location' {bucket = Lude.Nothing, key = Lude.Nothing}

-- | The Amazon S3 bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slBucket = Lens.lens (bucket :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: S3Location)
{-# DEPRECATED slBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The Amazon S3 bucket key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slKey = Lens.lens (key :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: S3Location)
{-# DEPRECATED slKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON S3Location where
  parseJSON =
    Lude.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Lude.<$> (x Lude..:? "bucket") Lude.<*> (x Lude..:? "key")
      )

instance Lude.ToJSON S3Location where
  toJSON S3Location' {..} =
    Lude.object
      ( Lude.catMaybes
          [("bucket" Lude..=) Lude.<$> bucket, ("key" Lude..=) Lude.<$> key]
      )
