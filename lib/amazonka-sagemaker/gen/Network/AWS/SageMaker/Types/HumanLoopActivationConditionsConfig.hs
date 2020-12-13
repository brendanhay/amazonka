{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
  ( HumanLoopActivationConditionsConfig (..),

    -- * Smart constructor
    mkHumanLoopActivationConditionsConfig,

    -- * Lenses
    hlaccHumanLoopActivationConditions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines under what conditions SageMaker creates a human loop. Used within . See for the required format of activation conditions.
--
-- /See:/ 'mkHumanLoopActivationConditionsConfig' smart constructor.
newtype HumanLoopActivationConditionsConfig = HumanLoopActivationConditionsConfig'
  { -- | JSON expressing use-case specific conditions declaratively. If any condition is matched, atomic tasks are created against the configured work team. The set of conditions is different for Rekognition and Textract. For more information about how to structure the JSON, see <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI> in the /Amazon SageMaker Developer Guide/ .
    humanLoopActivationConditions :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HumanLoopActivationConditionsConfig' with the minimum fields required to make a request.
--
-- * 'humanLoopActivationConditions' - JSON expressing use-case specific conditions declaratively. If any condition is matched, atomic tasks are created against the configured work team. The set of conditions is different for Rekognition and Textract. For more information about how to structure the JSON, see <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI> in the /Amazon SageMaker Developer Guide/ .
mkHumanLoopActivationConditionsConfig ::
  -- | 'humanLoopActivationConditions'
  Lude.Text ->
  HumanLoopActivationConditionsConfig
mkHumanLoopActivationConditionsConfig
  pHumanLoopActivationConditions_ =
    HumanLoopActivationConditionsConfig'
      { humanLoopActivationConditions =
          pHumanLoopActivationConditions_
      }

-- | JSON expressing use-case specific conditions declaratively. If any condition is matched, atomic tasks are created against the configured work team. The set of conditions is different for Rekognition and Textract. For more information about how to structure the JSON, see <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI> in the /Amazon SageMaker Developer Guide/ .
--
-- /Note:/ Consider using 'humanLoopActivationConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlaccHumanLoopActivationConditions :: Lens.Lens' HumanLoopActivationConditionsConfig Lude.Text
hlaccHumanLoopActivationConditions = Lens.lens (humanLoopActivationConditions :: HumanLoopActivationConditionsConfig -> Lude.Text) (\s a -> s {humanLoopActivationConditions = a} :: HumanLoopActivationConditionsConfig)
{-# DEPRECATED hlaccHumanLoopActivationConditions "Use generic-lens or generic-optics with 'humanLoopActivationConditions' instead." #-}

instance Lude.FromJSON HumanLoopActivationConditionsConfig where
  parseJSON =
    Lude.withObject
      "HumanLoopActivationConditionsConfig"
      ( \x ->
          HumanLoopActivationConditionsConfig'
            Lude.<$> (x Lude..: "HumanLoopActivationConditions")
      )

instance Lude.ToJSON HumanLoopActivationConditionsConfig where
  toJSON HumanLoopActivationConditionsConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "HumanLoopActivationConditions"
                  Lude..= humanLoopActivationConditions
              )
          ]
      )
