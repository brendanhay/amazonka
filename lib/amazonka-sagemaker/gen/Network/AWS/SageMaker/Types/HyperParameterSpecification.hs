{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterSpecification
  ( HyperParameterSpecification (..)
  -- * Smart constructor
  , mkHyperParameterSpecification
  -- * Lenses
  , hpsName
  , hpsType
  , hpsDefaultValue
  , hpsDescription
  , hpsIsRequired
  , hpsIsTunable
  , hpsRange
  ) where

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
  { name :: Types.Name
    -- ^ The name of this hyperparameter. The name must be unique.
  , type' :: Types.ParameterType
    -- ^ The type of this hyperparameter. The valid types are @Integer@ , @Continuous@ , @Categorical@ , and @FreeText@ .
  , defaultValue :: Core.Maybe Types.HyperParameterValue
    -- ^ The default value for this hyperparameter. If a default value is specified, a hyperparameter cannot be required.
  , description :: Core.Maybe Types.EntityDescription
    -- ^ A brief description of the hyperparameter.
  , isRequired :: Core.Maybe Core.Bool
    -- ^ Indicates whether this hyperparameter is required.
  , isTunable :: Core.Maybe Core.Bool
    -- ^ Indicates whether this hyperparameter is tunable in a hyperparameter tuning job.
  , range :: Core.Maybe Types.ParameterRange
    -- ^ The allowed range for this hyperparameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HyperParameterSpecification' value with any optional fields omitted.
mkHyperParameterSpecification
    :: Types.Name -- ^ 'name'
    -> Types.ParameterType -- ^ 'type\''
    -> HyperParameterSpecification
mkHyperParameterSpecification name type'
  = HyperParameterSpecification'{name, type',
                                 defaultValue = Core.Nothing, description = Core.Nothing,
                                 isRequired = Core.Nothing, isTunable = Core.Nothing,
                                 range = Core.Nothing}

-- | The name of this hyperparameter. The name must be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsName :: Lens.Lens' HyperParameterSpecification Types.Name
hpsName = Lens.field @"name"
{-# INLINEABLE hpsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of this hyperparameter. The valid types are @Integer@ , @Continuous@ , @Categorical@ , and @FreeText@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsType :: Lens.Lens' HyperParameterSpecification Types.ParameterType
hpsType = Lens.field @"type'"
{-# INLINEABLE hpsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The default value for this hyperparameter. If a default value is specified, a hyperparameter cannot be required.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsDefaultValue :: Lens.Lens' HyperParameterSpecification (Core.Maybe Types.HyperParameterValue)
hpsDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE hpsDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | A brief description of the hyperparameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsDescription :: Lens.Lens' HyperParameterSpecification (Core.Maybe Types.EntityDescription)
hpsDescription = Lens.field @"description"
{-# INLINEABLE hpsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether this hyperparameter is required.
--
-- /Note:/ Consider using 'isRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsIsRequired :: Lens.Lens' HyperParameterSpecification (Core.Maybe Core.Bool)
hpsIsRequired = Lens.field @"isRequired"
{-# INLINEABLE hpsIsRequired #-}
{-# DEPRECATED isRequired "Use generic-lens or generic-optics with 'isRequired' instead"  #-}

-- | Indicates whether this hyperparameter is tunable in a hyperparameter tuning job.
--
-- /Note:/ Consider using 'isTunable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsIsTunable :: Lens.Lens' HyperParameterSpecification (Core.Maybe Core.Bool)
hpsIsTunable = Lens.field @"isTunable"
{-# INLINEABLE hpsIsTunable #-}
{-# DEPRECATED isTunable "Use generic-lens or generic-optics with 'isTunable' instead"  #-}

-- | The allowed range for this hyperparameter.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsRange :: Lens.Lens' HyperParameterSpecification (Core.Maybe Types.ParameterRange)
hpsRange = Lens.field @"range"
{-# INLINEABLE hpsRange #-}
{-# DEPRECATED range "Use generic-lens or generic-optics with 'range' instead"  #-}

instance Core.FromJSON HyperParameterSpecification where
        toJSON HyperParameterSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name), Core.Just ("Type" Core..= type'),
                  ("DefaultValue" Core..=) Core.<$> defaultValue,
                  ("Description" Core..=) Core.<$> description,
                  ("IsRequired" Core..=) Core.<$> isRequired,
                  ("IsTunable" Core..=) Core.<$> isTunable,
                  ("Range" Core..=) Core.<$> range])

instance Core.FromJSON HyperParameterSpecification where
        parseJSON
          = Core.withObject "HyperParameterSpecification" Core.$
              \ x ->
                HyperParameterSpecification' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Type" Core.<*>
                    x Core..:? "DefaultValue"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "IsRequired"
                    Core.<*> x Core..:? "IsTunable"
                    Core.<*> x Core..:? "Range"
