-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.InvalidationBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.InvalidationBatch
  ( InvalidationBatch (..),

    -- * Smart constructor
    mkInvalidationBatch,

    -- * Lenses
    ibPaths,
    ibCallerReference,
  )
where

import Network.AWS.CloudFront.Types.Paths
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An invalidation batch.
--
-- /See:/ 'mkInvalidationBatch' smart constructor.
data InvalidationBatch = InvalidationBatch'
  { paths :: Paths,
    callerReference :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InvalidationBatch' with the minimum fields required to make a request.
--
-- * 'callerReference' - A value that you specify to uniquely identify an invalidation request. CloudFront uses the value to prevent you from accidentally resubmitting an identical request. Whenever you create a new invalidation request, you must specify a new value for @CallerReference@ and change other values in the request as applicable. One way to ensure that the value of @CallerReference@ is unique is to use a @timestamp@ , for example, @20120301090000@ .
--
-- If you make a second invalidation request with the same value for @CallerReference@ , and if the rest of the request is the same, CloudFront doesn't create a new invalidation request. Instead, CloudFront returns information about the invalidation request that you previously created with the same @CallerReference@ .
-- If @CallerReference@ is a value you already sent in a previous invalidation batch request but the content of any @Path@ is different from the original request, CloudFront returns an @InvalidationBatchAlreadyExists@ error.
-- * 'paths' - A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
mkInvalidationBatch ::
  -- | 'paths'
  Paths ->
  -- | 'callerReference'
  Lude.Text ->
  InvalidationBatch
mkInvalidationBatch pPaths_ pCallerReference_ =
  InvalidationBatch'
    { paths = pPaths_,
      callerReference = pCallerReference_
    }

-- | A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'paths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibPaths :: Lens.Lens' InvalidationBatch Paths
ibPaths = Lens.lens (paths :: InvalidationBatch -> Paths) (\s a -> s {paths = a} :: InvalidationBatch)
{-# DEPRECATED ibPaths "Use generic-lens or generic-optics with 'paths' instead." #-}

-- | A value that you specify to uniquely identify an invalidation request. CloudFront uses the value to prevent you from accidentally resubmitting an identical request. Whenever you create a new invalidation request, you must specify a new value for @CallerReference@ and change other values in the request as applicable. One way to ensure that the value of @CallerReference@ is unique is to use a @timestamp@ , for example, @20120301090000@ .
--
-- If you make a second invalidation request with the same value for @CallerReference@ , and if the rest of the request is the same, CloudFront doesn't create a new invalidation request. Instead, CloudFront returns information about the invalidation request that you previously created with the same @CallerReference@ .
-- If @CallerReference@ is a value you already sent in a previous invalidation batch request but the content of any @Path@ is different from the original request, CloudFront returns an @InvalidationBatchAlreadyExists@ error.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibCallerReference :: Lens.Lens' InvalidationBatch Lude.Text
ibCallerReference = Lens.lens (callerReference :: InvalidationBatch -> Lude.Text) (\s a -> s {callerReference = a} :: InvalidationBatch)
{-# DEPRECATED ibCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

instance Lude.FromXML InvalidationBatch where
  parseXML x =
    InvalidationBatch'
      Lude.<$> (x Lude..@ "Paths") Lude.<*> (x Lude..@ "CallerReference")

instance Lude.ToXML InvalidationBatch where
  toXML InvalidationBatch' {..} =
    Lude.mconcat
      ["Paths" Lude.@= paths, "CallerReference" Lude.@= callerReference]
