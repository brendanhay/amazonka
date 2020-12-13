{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Bucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Bucket
  ( Bucket (..),

    -- * Smart constructor
    mkBucket,

    -- * Lenses
    bName,
    bCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | In terms of implementation, a Bucket is a resource. An Amazon S3 bucket name is globally unique, and the namespace is shared by all AWS accounts.
--
-- /See:/ 'mkBucket' smart constructor.
data Bucket = Bucket'
  { -- | The name of the bucket.
    name :: BucketName,
    -- | Date the bucket was created.
    creationDate :: Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Bucket' with the minimum fields required to make a request.
--
-- * 'name' - The name of the bucket.
-- * 'creationDate' - Date the bucket was created.
mkBucket ::
  -- | 'name'
  BucketName ->
  -- | 'creationDate'
  Lude.DateTime ->
  Bucket
mkBucket pName_ pCreationDate_ =
  Bucket' {name = pName_, creationDate = pCreationDate_}

-- | The name of the bucket.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bName :: Lens.Lens' Bucket BucketName
bName = Lens.lens (name :: Bucket -> BucketName) (\s a -> s {name = a} :: Bucket)
{-# DEPRECATED bName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Date the bucket was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreationDate :: Lens.Lens' Bucket Lude.DateTime
bCreationDate = Lens.lens (creationDate :: Bucket -> Lude.DateTime) (\s a -> s {creationDate = a} :: Bucket)
{-# DEPRECATED bCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromXML Bucket where
  parseXML x =
    Bucket'
      Lude.<$> (x Lude..@ "Name") Lude.<*> (x Lude..@ "CreationDate")
