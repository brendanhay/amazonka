{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryDestination
  ( InventoryDestination (..),

    -- * Smart constructor
    mkInventoryDestination,

    -- * Lenses
    idS3BucketDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryS3BucketDestination

-- | Specifies the inventory configuration for an Amazon S3 bucket.
--
-- /See:/ 'mkInventoryDestination' smart constructor.
newtype InventoryDestination = InventoryDestination'
  { s3BucketDestination ::
      InventoryS3BucketDestination
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryDestination' with the minimum fields required to make a request.
--
-- * 's3BucketDestination' - Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
mkInventoryDestination ::
  -- | 's3BucketDestination'
  InventoryS3BucketDestination ->
  InventoryDestination
mkInventoryDestination pS3BucketDestination_ =
  InventoryDestination'
    { s3BucketDestination =
        pS3BucketDestination_
    }

-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
--
-- /Note:/ Consider using 's3BucketDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idS3BucketDestination :: Lens.Lens' InventoryDestination InventoryS3BucketDestination
idS3BucketDestination = Lens.lens (s3BucketDestination :: InventoryDestination -> InventoryS3BucketDestination) (\s a -> s {s3BucketDestination = a} :: InventoryDestination)
{-# DEPRECATED idS3BucketDestination "Use generic-lens or generic-optics with 's3BucketDestination' instead." #-}

instance Lude.FromXML InventoryDestination where
  parseXML x =
    InventoryDestination' Lude.<$> (x Lude..@ "S3BucketDestination")

instance Lude.ToXML InventoryDestination where
  toXML InventoryDestination' {..} =
    Lude.mconcat ["S3BucketDestination" Lude.@= s3BucketDestination]
