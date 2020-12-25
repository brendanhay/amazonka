{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmStatusItem
  ( AlgorithmStatusItem (..),

    -- * Smart constructor
    mkAlgorithmStatusItem,

    -- * Lenses
    asiName,
    asiStatus,
    asiFailureReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DetailedAlgorithmStatus as Types
import qualified Network.AWS.SageMaker.Types.EntityName as Types
import qualified Network.AWS.SageMaker.Types.String as Types

-- | Represents the overall status of an algorithm.
--
-- /See:/ 'mkAlgorithmStatusItem' smart constructor.
data AlgorithmStatusItem = AlgorithmStatusItem'
  { -- | The name of the algorithm for which the overall status is being reported.
    name :: Types.EntityName,
    -- | The current status.
    status :: Types.DetailedAlgorithmStatus,
    -- | if the overall status is @Failed@ , the reason for the failure.
    failureReason :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlgorithmStatusItem' value with any optional fields omitted.
mkAlgorithmStatusItem ::
  -- | 'name'
  Types.EntityName ->
  -- | 'status'
  Types.DetailedAlgorithmStatus ->
  AlgorithmStatusItem
mkAlgorithmStatusItem name status =
  AlgorithmStatusItem' {name, status, failureReason = Core.Nothing}

-- | The name of the algorithm for which the overall status is being reported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiName :: Lens.Lens' AlgorithmStatusItem Types.EntityName
asiName = Lens.field @"name"
{-# DEPRECATED asiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The current status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiStatus :: Lens.Lens' AlgorithmStatusItem Types.DetailedAlgorithmStatus
asiStatus = Lens.field @"status"
{-# DEPRECATED asiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | if the overall status is @Failed@ , the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiFailureReason :: Lens.Lens' AlgorithmStatusItem (Core.Maybe Types.String)
asiFailureReason = Lens.field @"failureReason"
{-# DEPRECATED asiFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

instance Core.FromJSON AlgorithmStatusItem where
  parseJSON =
    Core.withObject "AlgorithmStatusItem" Core.$
      \x ->
        AlgorithmStatusItem'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "Status")
          Core.<*> (x Core..:? "FailureReason")
