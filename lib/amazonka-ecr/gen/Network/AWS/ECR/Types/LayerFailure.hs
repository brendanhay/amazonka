{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LayerFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.LayerFailure
  ( LayerFailure (..)
  -- * Smart constructor
  , mkLayerFailure
  -- * Lenses
  , lfFailureCode
  , lfFailureReason
  , lfLayerDigest
  ) where

import qualified Network.AWS.ECR.Types.BatchedOperationLayerDigest as Types
import qualified Network.AWS.ECR.Types.LayerFailureCode as Types
import qualified Network.AWS.ECR.Types.LayerFailureReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an Amazon ECR image layer failure.
--
-- /See:/ 'mkLayerFailure' smart constructor.
data LayerFailure = LayerFailure'
  { failureCode :: Core.Maybe Types.LayerFailureCode
    -- ^ The failure code associated with the failure.
  , failureReason :: Core.Maybe Types.LayerFailureReason
    -- ^ The reason for the failure.
  , layerDigest :: Core.Maybe Types.BatchedOperationLayerDigest
    -- ^ The layer digest associated with the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LayerFailure' value with any optional fields omitted.
mkLayerFailure
    :: LayerFailure
mkLayerFailure
  = LayerFailure'{failureCode = Core.Nothing,
                  failureReason = Core.Nothing, layerDigest = Core.Nothing}

-- | The failure code associated with the failure.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFailureCode :: Lens.Lens' LayerFailure (Core.Maybe Types.LayerFailureCode)
lfFailureCode = Lens.field @"failureCode"
{-# INLINEABLE lfFailureCode #-}
{-# DEPRECATED failureCode "Use generic-lens or generic-optics with 'failureCode' instead"  #-}

-- | The reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFailureReason :: Lens.Lens' LayerFailure (Core.Maybe Types.LayerFailureReason)
lfFailureReason = Lens.field @"failureReason"
{-# INLINEABLE lfFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The layer digest associated with the failure.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLayerDigest :: Lens.Lens' LayerFailure (Core.Maybe Types.BatchedOperationLayerDigest)
lfLayerDigest = Lens.field @"layerDigest"
{-# INLINEABLE lfLayerDigest #-}
{-# DEPRECATED layerDigest "Use generic-lens or generic-optics with 'layerDigest' instead"  #-}

instance Core.FromJSON LayerFailure where
        parseJSON
          = Core.withObject "LayerFailure" Core.$
              \ x ->
                LayerFailure' Core.<$>
                  (x Core..:? "failureCode") Core.<*> x Core..:? "failureReason"
                    Core.<*> x Core..:? "layerDigest"
