{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
  ( ParentHyperParameterTuningJob (..)
  -- * Smart constructor
  , mkParentHyperParameterTuningJob
  -- * Lenses
  , phptjHyperParameterTuningJobName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobName as Types

-- | A previously completed or stopped hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
--
-- /See:/ 'mkParentHyperParameterTuningJob' smart constructor.
newtype ParentHyperParameterTuningJob = ParentHyperParameterTuningJob'
  { hyperParameterTuningJobName :: Core.Maybe Types.HyperParameterTuningJobName
    -- ^ The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ParentHyperParameterTuningJob' value with any optional fields omitted.
mkParentHyperParameterTuningJob
    :: ParentHyperParameterTuningJob
mkParentHyperParameterTuningJob
  = ParentHyperParameterTuningJob'{hyperParameterTuningJobName =
                                     Core.Nothing}

-- | The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phptjHyperParameterTuningJobName :: Lens.Lens' ParentHyperParameterTuningJob (Core.Maybe Types.HyperParameterTuningJobName)
phptjHyperParameterTuningJobName = Lens.field @"hyperParameterTuningJobName"
{-# INLINEABLE phptjHyperParameterTuningJobName #-}
{-# DEPRECATED hyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead"  #-}

instance Core.FromJSON ParentHyperParameterTuningJob where
        toJSON ParentHyperParameterTuningJob{..}
          = Core.object
              (Core.catMaybes
                 [("HyperParameterTuningJobName" Core..=) Core.<$>
                    hyperParameterTuningJobName])

instance Core.FromJSON ParentHyperParameterTuningJob where
        parseJSON
          = Core.withObject "ParentHyperParameterTuningJob" Core.$
              \ x ->
                ParentHyperParameterTuningJob' Core.<$>
                  (x Core..:? "HyperParameterTuningJobName")
