{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.InvalidationBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.InvalidationBatch
  ( InvalidationBatch (..)
  -- * Smart constructor
  , mkInvalidationBatch
  -- * Lenses
  , ibPaths
  , ibCallerReference
  ) where

import qualified Network.AWS.CloudFront.Types.Paths as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An invalidation batch.
--
-- /See:/ 'mkInvalidationBatch' smart constructor.
data InvalidationBatch = InvalidationBatch'
  { paths :: Types.Paths
    -- ^ A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ . 
  , callerReference :: Core.Text
    -- ^ A value that you specify to uniquely identify an invalidation request. CloudFront uses the value to prevent you from accidentally resubmitting an identical request. Whenever you create a new invalidation request, you must specify a new value for @CallerReference@ and change other values in the request as applicable. One way to ensure that the value of @CallerReference@ is unique is to use a @timestamp@ , for example, @20120301090000@ .
--
-- If you make a second invalidation request with the same value for @CallerReference@ , and if the rest of the request is the same, CloudFront doesn't create a new invalidation request. Instead, CloudFront returns information about the invalidation request that you previously created with the same @CallerReference@ .
-- If @CallerReference@ is a value you already sent in a previous invalidation batch request but the content of any @Path@ is different from the original request, CloudFront returns an @InvalidationBatchAlreadyExists@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InvalidationBatch' value with any optional fields omitted.
mkInvalidationBatch
    :: Types.Paths -- ^ 'paths'
    -> Core.Text -- ^ 'callerReference'
    -> InvalidationBatch
mkInvalidationBatch paths callerReference
  = InvalidationBatch'{paths, callerReference}

-- | A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ . 
--
-- /Note:/ Consider using 'paths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibPaths :: Lens.Lens' InvalidationBatch Types.Paths
ibPaths = Lens.field @"paths"
{-# INLINEABLE ibPaths #-}
{-# DEPRECATED paths "Use generic-lens or generic-optics with 'paths' instead"  #-}

-- | A value that you specify to uniquely identify an invalidation request. CloudFront uses the value to prevent you from accidentally resubmitting an identical request. Whenever you create a new invalidation request, you must specify a new value for @CallerReference@ and change other values in the request as applicable. One way to ensure that the value of @CallerReference@ is unique is to use a @timestamp@ , for example, @20120301090000@ .
--
-- If you make a second invalidation request with the same value for @CallerReference@ , and if the rest of the request is the same, CloudFront doesn't create a new invalidation request. Instead, CloudFront returns information about the invalidation request that you previously created with the same @CallerReference@ .
-- If @CallerReference@ is a value you already sent in a previous invalidation batch request but the content of any @Path@ is different from the original request, CloudFront returns an @InvalidationBatchAlreadyExists@ error.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibCallerReference :: Lens.Lens' InvalidationBatch Core.Text
ibCallerReference = Lens.field @"callerReference"
{-# INLINEABLE ibCallerReference #-}
{-# DEPRECATED callerReference "Use generic-lens or generic-optics with 'callerReference' instead"  #-}

instance Core.ToXML InvalidationBatch where
        toXML InvalidationBatch{..}
          = Core.toXMLElement "Paths" paths Core.<>
              Core.toXMLElement "CallerReference" callerReference

instance Core.FromXML InvalidationBatch where
        parseXML x
          = InvalidationBatch' Core.<$>
              (x Core..@ "Paths") Core.<*> x Core..@ "CallerReference"
