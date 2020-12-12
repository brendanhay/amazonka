{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NoncurrentVersionTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NoncurrentVersionTransition
  ( NoncurrentVersionTransition (..),

    -- * Smart constructor
    mkNoncurrentVersionTransition,

    -- * Lenses
    nvtNoncurrentDays,
    nvtStorageClass,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.TransitionStorageClass

-- | Container for the transition rule that describes when noncurrent objects transition to the @STANDARD_IA@ , @ONEZONE_IA@ , @INTELLIGENT_TIERING@ , @GLACIER@ , or @DEEP_ARCHIVE@ storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the @STANDARD_IA@ , @ONEZONE_IA@ , @INTELLIGENT_TIERING@ , @GLACIER@ , or @DEEP_ARCHIVE@ storage class at a specific period in the object's lifetime.
--
-- /See:/ 'mkNoncurrentVersionTransition' smart constructor.
data NoncurrentVersionTransition = NoncurrentVersionTransition'
  { noncurrentDays ::
      Lude.Int,
    storageClass ::
      TransitionStorageClass
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NoncurrentVersionTransition' with the minimum fields required to make a request.
--
-- * 'noncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'storageClass' - The class of storage used to store the object.
mkNoncurrentVersionTransition ::
  -- | 'noncurrentDays'
  Lude.Int ->
  -- | 'storageClass'
  TransitionStorageClass ->
  NoncurrentVersionTransition
mkNoncurrentVersionTransition pNoncurrentDays_ pStorageClass_ =
  NoncurrentVersionTransition'
    { noncurrentDays = pNoncurrentDays_,
      storageClass = pStorageClass_
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'noncurrentDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvtNoncurrentDays :: Lens.Lens' NoncurrentVersionTransition Lude.Int
nvtNoncurrentDays = Lens.lens (noncurrentDays :: NoncurrentVersionTransition -> Lude.Int) (\s a -> s {noncurrentDays = a} :: NoncurrentVersionTransition)
{-# DEPRECATED nvtNoncurrentDays "Use generic-lens or generic-optics with 'noncurrentDays' instead." #-}

-- | The class of storage used to store the object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvtStorageClass :: Lens.Lens' NoncurrentVersionTransition TransitionStorageClass
nvtStorageClass = Lens.lens (storageClass :: NoncurrentVersionTransition -> TransitionStorageClass) (\s a -> s {storageClass = a} :: NoncurrentVersionTransition)
{-# DEPRECATED nvtStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

instance Lude.FromXML NoncurrentVersionTransition where
  parseXML x =
    NoncurrentVersionTransition'
      Lude.<$> (x Lude..@ "NoncurrentDays") Lude.<*> (x Lude..@ "StorageClass")

instance Lude.ToXML NoncurrentVersionTransition where
  toXML NoncurrentVersionTransition' {..} =
    Lude.mconcat
      [ "NoncurrentDays" Lude.@= noncurrentDays,
        "StorageClass" Lude.@= storageClass
      ]
