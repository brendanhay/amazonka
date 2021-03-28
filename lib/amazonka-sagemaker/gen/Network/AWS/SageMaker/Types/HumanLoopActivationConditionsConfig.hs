{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
  ( HumanLoopActivationConditionsConfig (..)
  -- * Smart constructor
  , mkHumanLoopActivationConditionsConfig
  -- * Lenses
  , hlaccHumanLoopActivationConditions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HumanLoopActivationConditions as Types

-- | Defines under what conditions SageMaker creates a human loop. Used within . See for the required format of activation conditions.
--
-- /See:/ 'mkHumanLoopActivationConditionsConfig' smart constructor.
newtype HumanLoopActivationConditionsConfig = HumanLoopActivationConditionsConfig'
  { humanLoopActivationConditions :: Types.HumanLoopActivationConditions
    -- ^ JSON expressing use-case specific conditions declaratively. If any condition is matched, atomic tasks are created against the configured work team. The set of conditions is different for Rekognition and Textract. For more information about how to structure the JSON, see <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI> in the /Amazon SageMaker Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HumanLoopActivationConditionsConfig' value with any optional fields omitted.
mkHumanLoopActivationConditionsConfig
    :: Types.HumanLoopActivationConditions -- ^ 'humanLoopActivationConditions'
    -> HumanLoopActivationConditionsConfig
mkHumanLoopActivationConditionsConfig humanLoopActivationConditions
  = HumanLoopActivationConditionsConfig'{humanLoopActivationConditions}

-- | JSON expressing use-case specific conditions declaratively. If any condition is matched, atomic tasks are created against the configured work team. The set of conditions is different for Rekognition and Textract. For more information about how to structure the JSON, see <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI> in the /Amazon SageMaker Developer Guide/ .
--
-- /Note:/ Consider using 'humanLoopActivationConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlaccHumanLoopActivationConditions :: Lens.Lens' HumanLoopActivationConditionsConfig Types.HumanLoopActivationConditions
hlaccHumanLoopActivationConditions = Lens.field @"humanLoopActivationConditions"
{-# INLINEABLE hlaccHumanLoopActivationConditions #-}
{-# DEPRECATED humanLoopActivationConditions "Use generic-lens or generic-optics with 'humanLoopActivationConditions' instead"  #-}

instance Core.FromJSON HumanLoopActivationConditionsConfig where
        toJSON HumanLoopActivationConditionsConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("HumanLoopActivationConditions" Core..=
                       humanLoopActivationConditions)])

instance Core.FromJSON HumanLoopActivationConditionsConfig where
        parseJSON
          = Core.withObject "HumanLoopActivationConditionsConfig" Core.$
              \ x ->
                HumanLoopActivationConditionsConfig' Core.<$>
                  (x Core..: "HumanLoopActivationConditions")
