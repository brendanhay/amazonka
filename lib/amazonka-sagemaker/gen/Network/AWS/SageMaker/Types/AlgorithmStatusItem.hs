{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AlgorithmStatusItem
  ( AlgorithmStatusItem (..)
  -- * Smart constructor
  , mkAlgorithmStatusItem
  -- * Lenses
  , asiName
  , asiStatus
  , asiFailureReason
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DetailedAlgorithmStatus as Types
import qualified Network.AWS.SageMaker.Types.EntityName as Types

-- | Represents the overall status of an algorithm.
--
-- /See:/ 'mkAlgorithmStatusItem' smart constructor.
data AlgorithmStatusItem = AlgorithmStatusItem'
  { name :: Types.EntityName
    -- ^ The name of the algorithm for which the overall status is being reported.
  , status :: Types.DetailedAlgorithmStatus
    -- ^ The current status.
  , failureReason :: Core.Maybe Core.Text
    -- ^ if the overall status is @Failed@ , the reason for the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlgorithmStatusItem' value with any optional fields omitted.
mkAlgorithmStatusItem
    :: Types.EntityName -- ^ 'name'
    -> Types.DetailedAlgorithmStatus -- ^ 'status'
    -> AlgorithmStatusItem
mkAlgorithmStatusItem name status
  = AlgorithmStatusItem'{name, status, failureReason = Core.Nothing}

-- | The name of the algorithm for which the overall status is being reported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiName :: Lens.Lens' AlgorithmStatusItem Types.EntityName
asiName = Lens.field @"name"
{-# INLINEABLE asiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The current status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiStatus :: Lens.Lens' AlgorithmStatusItem Types.DetailedAlgorithmStatus
asiStatus = Lens.field @"status"
{-# INLINEABLE asiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | if the overall status is @Failed@ , the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiFailureReason :: Lens.Lens' AlgorithmStatusItem (Core.Maybe Core.Text)
asiFailureReason = Lens.field @"failureReason"
{-# INLINEABLE asiFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

instance Core.FromJSON AlgorithmStatusItem where
        parseJSON
          = Core.withObject "AlgorithmStatusItem" Core.$
              \ x ->
                AlgorithmStatusItem' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Status" Core.<*>
                    x Core..:? "FailureReason"
