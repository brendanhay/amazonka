{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EvaluationResultIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.EvaluationResultIdentifier
  ( EvaluationResultIdentifier (..)
  -- * Smart constructor
  , mkEvaluationResultIdentifier
  -- * Lenses
  , eriEvaluationResultQualifier
  , eriOrderingTimestamp
  ) where

import qualified Network.AWS.Config.Types.EvaluationResultQualifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Uniquely identifies an evaluation result.
--
-- /See:/ 'mkEvaluationResultIdentifier' smart constructor.
data EvaluationResultIdentifier = EvaluationResultIdentifier'
  { evaluationResultQualifier :: Core.Maybe Types.EvaluationResultQualifier
    -- ^ Identifies an AWS Config rule used to evaluate an AWS resource, and provides the type and ID of the evaluated resource.
  , orderingTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the event that triggered the evaluation of your AWS resources. The time can indicate when AWS Config delivered a configuration item change notification, or it can indicate when AWS Config delivered the configuration snapshot, depending on which event triggered the evaluation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EvaluationResultIdentifier' value with any optional fields omitted.
mkEvaluationResultIdentifier
    :: EvaluationResultIdentifier
mkEvaluationResultIdentifier
  = EvaluationResultIdentifier'{evaluationResultQualifier =
                                  Core.Nothing,
                                orderingTimestamp = Core.Nothing}

-- | Identifies an AWS Config rule used to evaluate an AWS resource, and provides the type and ID of the evaluated resource.
--
-- /Note:/ Consider using 'evaluationResultQualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eriEvaluationResultQualifier :: Lens.Lens' EvaluationResultIdentifier (Core.Maybe Types.EvaluationResultQualifier)
eriEvaluationResultQualifier = Lens.field @"evaluationResultQualifier"
{-# INLINEABLE eriEvaluationResultQualifier #-}
{-# DEPRECATED evaluationResultQualifier "Use generic-lens or generic-optics with 'evaluationResultQualifier' instead"  #-}

-- | The time of the event that triggered the evaluation of your AWS resources. The time can indicate when AWS Config delivered a configuration item change notification, or it can indicate when AWS Config delivered the configuration snapshot, depending on which event triggered the evaluation.
--
-- /Note:/ Consider using 'orderingTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eriOrderingTimestamp :: Lens.Lens' EvaluationResultIdentifier (Core.Maybe Core.NominalDiffTime)
eriOrderingTimestamp = Lens.field @"orderingTimestamp"
{-# INLINEABLE eriOrderingTimestamp #-}
{-# DEPRECATED orderingTimestamp "Use generic-lens or generic-optics with 'orderingTimestamp' instead"  #-}

instance Core.FromJSON EvaluationResultIdentifier where
        parseJSON
          = Core.withObject "EvaluationResultIdentifier" Core.$
              \ x ->
                EvaluationResultIdentifier' Core.<$>
                  (x Core..:? "EvaluationResultQualifier") Core.<*>
                    x Core..:? "OrderingTimestamp"
