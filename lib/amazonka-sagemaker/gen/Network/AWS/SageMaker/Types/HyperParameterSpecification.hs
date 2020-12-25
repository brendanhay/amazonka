{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterSpecification
  ( HyperParameterSpecification (..),

    -- * Smart constructor
    mkHyperParameterSpecification,

    -- * Lenses
    hpsName,
    hpsType,
    hpsDefaultValue,
    hpsDescription,
    hpsIsRequired,
    hpsIsTunable,
    hpsRange,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EntityDescription as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterValue as Types
import qualified Network.AWS.SageMaker.Types.Name as Types
import qualified Network.AWS.SageMaker.Types.ParameterRange as Types
import qualified Network.AWS.SageMaker.Types.ParameterType as Types

-- | Defines a hyperparameter to be used by an algorithm.
--
-- /See:/ 'mkHyperParameterSpecification' smart constructor.
data HyperParameterSpecification = HyperParameterSpecification'
  { -- | The name of this hyperparameter. The name must be unique.
    name :: Types.Name,
    -- | The type of this hyperparameter. The valid types are @Integer@ , @Continuous@ , @Categorical@ , and @FreeText@ .
    type' :: Types.ParameterType,
    -- | The default value for this hyperparameter. If a default value is specified, a hyperparameter cannot be required.
    defaultValue :: Core.Maybe Types.HyperParameterValue,
    -- | A brief description of the hyperparameter.
    description :: Core.Maybe Types.EntityDescription,
    -- | Indicates whether this hyperparameter is required.
    isRequired :: Core.Maybe Core.Bool,
    -- | Indicates whether this hyperparameter is tunable in a hyperparameter tuning job.
    isTunable :: Core.Maybe Core.Bool,
    -- | The allowed range for this hyperparameter.
    range :: Core.Maybe Types.ParameterRange
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HyperParameterSpecification' value with any optional fields omitted.
mkHyperParameterSpecification ::
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.ParameterType ->
  HyperParameterSpecification
mkHyperParameterSpecification name type' =
  HyperParameterSpecification'
    { name,
      type',
      defaultValue = Core.Nothing,
      description = Core.Nothing,
      isRequired = Core.Nothing,
      isTunable = Core.Nothing,
      range = Core.Nothing
    }

-- | The name of this hyperparameter. The name must be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsName :: Lens.Lens' HyperParameterSpecification Types.Name
hpsName = Lens.field @"name"
{-# DEPRECATED hpsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of this hyperparameter. The valid types are @Integer@ , @Continuous@ , @Categorical@ , and @FreeText@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsType :: Lens.Lens' HyperParameterSpecification Types.ParameterType
hpsType = Lens.field @"type'"
{-# DEPRECATED hpsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The default value for this hyperparameter. If a default value is specified, a hyperparameter cannot be required.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsDefaultValue :: Lens.Lens' HyperParameterSpecification (Core.Maybe Types.HyperParameterValue)
hpsDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED hpsDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | A brief description of the hyperparameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsDescription :: Lens.Lens' HyperParameterSpecification (Core.Maybe Types.EntityDescription)
hpsDescription = Lens.field @"description"
{-# DEPRECATED hpsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether this hyperparameter is required.
--
-- /Note:/ Consider using 'isRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsIsRequired :: Lens.Lens' HyperParameterSpecification (Core.Maybe Core.Bool)
hpsIsRequired = Lens.field @"isRequired"
{-# DEPRECATED hpsIsRequired "Use generic-lens or generic-optics with 'isRequired' instead." #-}

-- | Indicates whether this hyperparameter is tunable in a hyperparameter tuning job.
--
-- /Note:/ Consider using 'isTunable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsIsTunable :: Lens.Lens' HyperParameterSpecification (Core.Maybe Core.Bool)
hpsIsTunable = Lens.field @"isTunable"
{-# DEPRECATED hpsIsTunable "Use generic-lens or generic-optics with 'isTunable' instead." #-}

-- | The allowed range for this hyperparameter.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsRange :: Lens.Lens' HyperParameterSpecification (Core.Maybe Types.ParameterRange)
hpsRange = Lens.field @"range"
{-# DEPRECATED hpsRange "Use generic-lens or generic-optics with 'range' instead." #-}

instance Core.FromJSON HyperParameterSpecification where
  toJSON HyperParameterSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type'),
            ("DefaultValue" Core..=) Core.<$> defaultValue,
            ("Description" Core..=) Core.<$> description,
            ("IsRequired" Core..=) Core.<$> isRequired,
            ("IsTunable" Core..=) Core.<$> isTunable,
            ("Range" Core..=) Core.<$> range
          ]
      )

instance Core.FromJSON HyperParameterSpecification where
  parseJSON =
    Core.withObject "HyperParameterSpecification" Core.$
      \x ->
        HyperParameterSpecification'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "Type")
          Core.<*> (x Core..:? "DefaultValue")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "IsRequired")
          Core.<*> (x Core..:? "IsTunable")
          Core.<*> (x Core..:? "Range")
