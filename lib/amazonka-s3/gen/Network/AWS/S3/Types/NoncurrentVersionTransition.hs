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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.TransitionStorageClass as Types

-- | Container for the transition rule that describes when noncurrent objects transition to the @STANDARD_IA@ , @ONEZONE_IA@ , @INTELLIGENT_TIERING@ , @GLACIER@ , or @DEEP_ARCHIVE@ storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the @STANDARD_IA@ , @ONEZONE_IA@ , @INTELLIGENT_TIERING@ , @GLACIER@ , or @DEEP_ARCHIVE@ storage class at a specific period in the object's lifetime.
--
-- /See:/ 'mkNoncurrentVersionTransition' smart constructor.
data NoncurrentVersionTransition = NoncurrentVersionTransition'
  { -- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
    noncurrentDays :: Core.Int,
    -- | The class of storage used to store the object.
    storageClass :: Types.TransitionStorageClass
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NoncurrentVersionTransition' value with any optional fields omitted.
mkNoncurrentVersionTransition ::
  -- | 'noncurrentDays'
  Core.Int ->
  -- | 'storageClass'
  Types.TransitionStorageClass ->
  NoncurrentVersionTransition
mkNoncurrentVersionTransition noncurrentDays storageClass =
  NoncurrentVersionTransition' {noncurrentDays, storageClass}

-- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'noncurrentDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvtNoncurrentDays :: Lens.Lens' NoncurrentVersionTransition Core.Int
nvtNoncurrentDays = Lens.field @"noncurrentDays"
{-# DEPRECATED nvtNoncurrentDays "Use generic-lens or generic-optics with 'noncurrentDays' instead." #-}

-- | The class of storage used to store the object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvtStorageClass :: Lens.Lens' NoncurrentVersionTransition Types.TransitionStorageClass
nvtStorageClass = Lens.field @"storageClass"
{-# DEPRECATED nvtStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

instance Core.ToXML NoncurrentVersionTransition where
  toXML NoncurrentVersionTransition {..} =
    Core.toXMLNode "NoncurrentDays" noncurrentDays
      Core.<> Core.toXMLNode "StorageClass" storageClass

instance Core.FromXML NoncurrentVersionTransition where
  parseXML x =
    NoncurrentVersionTransition'
      Core.<$> (x Core..@ "NoncurrentDays") Core.<*> (x Core..@ "StorageClass")
