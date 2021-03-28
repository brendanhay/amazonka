{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.HumanLoopActivationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.HumanLoopActivationOutput
  ( HumanLoopActivationOutput (..)
  -- * Smart constructor
  , mkHumanLoopActivationOutput
  -- * Lenses
  , hlaoHumanLoopActivationConditionsEvaluationResults
  , hlaoHumanLoopActivationReasons
  , hlaoHumanLoopArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.HumanLoopActivationConditionsEvaluationResults as Types
import qualified Network.AWS.Rekognition.Types.HumanLoopActivationReason as Types
import qualified Network.AWS.Rekognition.Types.HumanLoopArn as Types

-- | Shows the results of the human in the loop evaluation. If there is no HumanLoopArn, the input did not trigger human review.
--
-- /See:/ 'mkHumanLoopActivationOutput' smart constructor.
data HumanLoopActivationOutput = HumanLoopActivationOutput'
  { humanLoopActivationConditionsEvaluationResults :: Core.Maybe Types.HumanLoopActivationConditionsEvaluationResults
    -- ^ Shows the result of condition evaluations, including those conditions which activated a human review.
  , humanLoopActivationReasons :: Core.Maybe (Core.NonEmpty Types.HumanLoopActivationReason)
    -- ^ Shows if and why human review was needed.
  , humanLoopArn :: Core.Maybe Types.HumanLoopArn
    -- ^ The Amazon Resource Name (ARN) of the HumanLoop created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HumanLoopActivationOutput' value with any optional fields omitted.
mkHumanLoopActivationOutput
    :: HumanLoopActivationOutput
mkHumanLoopActivationOutput
  = HumanLoopActivationOutput'{humanLoopActivationConditionsEvaluationResults
                                 = Core.Nothing,
                               humanLoopActivationReasons = Core.Nothing,
                               humanLoopArn = Core.Nothing}

-- | Shows the result of condition evaluations, including those conditions which activated a human review.
--
-- /Note:/ Consider using 'humanLoopActivationConditionsEvaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlaoHumanLoopActivationConditionsEvaluationResults :: Lens.Lens' HumanLoopActivationOutput (Core.Maybe Types.HumanLoopActivationConditionsEvaluationResults)
hlaoHumanLoopActivationConditionsEvaluationResults = Lens.field @"humanLoopActivationConditionsEvaluationResults"
{-# INLINEABLE hlaoHumanLoopActivationConditionsEvaluationResults #-}
{-# DEPRECATED humanLoopActivationConditionsEvaluationResults "Use generic-lens or generic-optics with 'humanLoopActivationConditionsEvaluationResults' instead"  #-}

-- | Shows if and why human review was needed.
--
-- /Note:/ Consider using 'humanLoopActivationReasons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlaoHumanLoopActivationReasons :: Lens.Lens' HumanLoopActivationOutput (Core.Maybe (Core.NonEmpty Types.HumanLoopActivationReason))
hlaoHumanLoopActivationReasons = Lens.field @"humanLoopActivationReasons"
{-# INLINEABLE hlaoHumanLoopActivationReasons #-}
{-# DEPRECATED humanLoopActivationReasons "Use generic-lens or generic-optics with 'humanLoopActivationReasons' instead"  #-}

-- | The Amazon Resource Name (ARN) of the HumanLoop created.
--
-- /Note:/ Consider using 'humanLoopArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlaoHumanLoopArn :: Lens.Lens' HumanLoopActivationOutput (Core.Maybe Types.HumanLoopArn)
hlaoHumanLoopArn = Lens.field @"humanLoopArn"
{-# INLINEABLE hlaoHumanLoopArn #-}
{-# DEPRECATED humanLoopArn "Use generic-lens or generic-optics with 'humanLoopArn' instead"  #-}

instance Core.FromJSON HumanLoopActivationOutput where
        parseJSON
          = Core.withObject "HumanLoopActivationOutput" Core.$
              \ x ->
                HumanLoopActivationOutput' Core.<$>
                  (x Core..:? "HumanLoopActivationConditionsEvaluationResults")
                    Core.<*> x Core..:? "HumanLoopActivationReasons"
                    Core.<*> x Core..:? "HumanLoopArn"
