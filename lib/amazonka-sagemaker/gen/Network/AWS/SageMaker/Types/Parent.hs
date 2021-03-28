{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Parent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Parent
  ( Parent (..)
  -- * Smart constructor
  , mkParent
  -- * Lenses
  , pExperimentName
  , pTrialName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ExperimentName as Types
import qualified Network.AWS.SageMaker.Types.TrialName as Types

-- | The trial that a trial component is associated with and the experiment the trial is part of. A component might not be associated with a trial. A component can be associated with multiple trials.
--
-- /See:/ 'mkParent' smart constructor.
data Parent = Parent'
  { experimentName :: Core.Maybe Types.ExperimentName
    -- ^ The name of the experiment.
  , trialName :: Core.Maybe Types.TrialName
    -- ^ The name of the trial.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Parent' value with any optional fields omitted.
mkParent
    :: Parent
mkParent
  = Parent'{experimentName = Core.Nothing, trialName = Core.Nothing}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExperimentName :: Lens.Lens' Parent (Core.Maybe Types.ExperimentName)
pExperimentName = Lens.field @"experimentName"
{-# INLINEABLE pExperimentName #-}
{-# DEPRECATED experimentName "Use generic-lens or generic-optics with 'experimentName' instead"  #-}

-- | The name of the trial.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTrialName :: Lens.Lens' Parent (Core.Maybe Types.TrialName)
pTrialName = Lens.field @"trialName"
{-# INLINEABLE pTrialName #-}
{-# DEPRECATED trialName "Use generic-lens or generic-optics with 'trialName' instead"  #-}

instance Core.FromJSON Parent where
        parseJSON
          = Core.withObject "Parent" Core.$
              \ x ->
                Parent' Core.<$>
                  (x Core..:? "ExperimentName") Core.<*> x Core..:? "TrialName"
