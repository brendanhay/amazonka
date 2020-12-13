{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StorageLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StorageLocation
  ( StorageLocation (..),

    -- * Smart constructor
    mkStorageLocation,

    -- * Lenses
    slBucket,
    slKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a storage location in Amazon S3.
--
-- /See:/ 'mkStorageLocation' smart constructor.
data StorageLocation = StorageLocation'
  { -- | The name of the S3 bucket.
    bucket :: Lude.Maybe Lude.Text,
    -- | The key.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StorageLocation' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the S3 bucket.
-- * 'key' - The key.
mkStorageLocation ::
  StorageLocation
mkStorageLocation =
  StorageLocation' {bucket = Lude.Nothing, key = Lude.Nothing}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' StorageLocation (Lude.Maybe Lude.Text)
slBucket = Lens.lens (bucket :: StorageLocation -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: StorageLocation)
{-# DEPRECATED slBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' StorageLocation (Lude.Maybe Lude.Text)
slKey = Lens.lens (key :: StorageLocation -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: StorageLocation)
{-# DEPRECATED slKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToQuery StorageLocation where
  toQuery StorageLocation' {..} =
    Lude.mconcat ["Bucket" Lude.=: bucket, "Key" Lude.=: key]
