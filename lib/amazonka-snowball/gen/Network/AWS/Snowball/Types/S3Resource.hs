-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.S3Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.S3Resource
  ( S3Resource (..),

    -- * Smart constructor
    mkS3Resource,

    -- * Lenses
    srKeyRange,
    srBucketARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.KeyRange

-- | Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into. For export jobs, this object can have an optional @KeyRange@ value. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
-- /See:/ 'mkS3Resource' smart constructor.
data S3Resource = S3Resource'
  { keyRange :: Lude.Maybe KeyRange,
    bucketARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Resource' with the minimum fields required to make a request.
--
-- * 'bucketARN' - The Amazon Resource Name (ARN) of an Amazon S3 bucket.
-- * 'keyRange' - For export jobs, you can provide an optional @KeyRange@ within a specific Amazon S3 bucket. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
mkS3Resource ::
  S3Resource
mkS3Resource =
  S3Resource' {keyRange = Lude.Nothing, bucketARN = Lude.Nothing}

-- | For export jobs, you can provide an optional @KeyRange@ within a specific Amazon S3 bucket. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
-- /Note:/ Consider using 'keyRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srKeyRange :: Lens.Lens' S3Resource (Lude.Maybe KeyRange)
srKeyRange = Lens.lens (keyRange :: S3Resource -> Lude.Maybe KeyRange) (\s a -> s {keyRange = a} :: S3Resource)
{-# DEPRECATED srKeyRange "Use generic-lens or generic-optics with 'keyRange' instead." #-}

-- | The Amazon Resource Name (ARN) of an Amazon S3 bucket.
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srBucketARN :: Lens.Lens' S3Resource (Lude.Maybe Lude.Text)
srBucketARN = Lens.lens (bucketARN :: S3Resource -> Lude.Maybe Lude.Text) (\s a -> s {bucketARN = a} :: S3Resource)
{-# DEPRECATED srBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

instance Lude.FromJSON S3Resource where
  parseJSON =
    Lude.withObject
      "S3Resource"
      ( \x ->
          S3Resource'
            Lude.<$> (x Lude..:? "KeyRange") Lude.<*> (x Lude..:? "BucketArn")
      )

instance Lude.ToJSON S3Resource where
  toJSON S3Resource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyRange" Lude..=) Lude.<$> keyRange,
            ("BucketArn" Lude..=) Lude.<$> bucketARN
          ]
      )
