{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Storage
  ( Storage (..),

    -- * Smart constructor
    mkStorage,

    -- * Lenses
    sS3,
  )
where

import Network.AWS.EC2.Types.S3Storage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the storage location for an instance store-backed AMI.
--
-- /See:/ 'mkStorage' smart constructor.
newtype Storage = Storage'
  { -- | An Amazon S3 storage location.
    s3 :: Lude.Maybe S3Storage
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Storage' with the minimum fields required to make a request.
--
-- * 's3' - An Amazon S3 storage location.
mkStorage ::
  Storage
mkStorage = Storage' {s3 = Lude.Nothing}

-- | An Amazon S3 storage location.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sS3 :: Lens.Lens' Storage (Lude.Maybe S3Storage)
sS3 = Lens.lens (s3 :: Storage -> Lude.Maybe S3Storage) (\s a -> s {s3 = a} :: Storage)
{-# DEPRECATED sS3 "Use generic-lens or generic-optics with 's3' instead." #-}

instance Lude.FromXML Storage where
  parseXML x = Storage' Lude.<$> (x Lude..@? "S3")

instance Lude.ToQuery Storage where
  toQuery Storage' {..} = Lude.mconcat ["S3" Lude.=: s3]
