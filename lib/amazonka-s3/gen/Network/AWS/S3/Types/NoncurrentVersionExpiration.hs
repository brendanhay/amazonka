{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NoncurrentVersionExpiration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NoncurrentVersionExpiration
  ( NoncurrentVersionExpiration (..),

    -- * Smart constructor
    mkNoncurrentVersionExpiration,

    -- * Lenses
    nveNoncurrentDays,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
--
-- /See:/ 'mkNoncurrentVersionExpiration' smart constructor.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration'
  { noncurrentDays ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NoncurrentVersionExpiration' with the minimum fields required to make a request.
--
-- * 'noncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
mkNoncurrentVersionExpiration ::
  -- | 'noncurrentDays'
  Lude.Int ->
  NoncurrentVersionExpiration
mkNoncurrentVersionExpiration pNoncurrentDays_ =
  NoncurrentVersionExpiration' {noncurrentDays = pNoncurrentDays_}

-- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'noncurrentDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nveNoncurrentDays :: Lens.Lens' NoncurrentVersionExpiration Lude.Int
nveNoncurrentDays = Lens.lens (noncurrentDays :: NoncurrentVersionExpiration -> Lude.Int) (\s a -> s {noncurrentDays = a} :: NoncurrentVersionExpiration)
{-# DEPRECATED nveNoncurrentDays "Use generic-lens or generic-optics with 'noncurrentDays' instead." #-}

instance Lude.FromXML NoncurrentVersionExpiration where
  parseXML x =
    NoncurrentVersionExpiration'
      Lude.<$> (x Lude..@ "NoncurrentDays")

instance Lude.ToXML NoncurrentVersionExpiration where
  toXML NoncurrentVersionExpiration' {..} =
    Lude.mconcat ["NoncurrentDays" Lude.@= noncurrentDays]
