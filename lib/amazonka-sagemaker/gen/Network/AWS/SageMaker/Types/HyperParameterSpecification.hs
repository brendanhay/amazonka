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
    hpsIsTunable,
    hpsRange,
    hpsDefaultValue,
    hpsIsRequired,
    hpsDescription,
    hpsName,
    hpsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ParameterRange
import Network.AWS.SageMaker.Types.ParameterType

-- | Defines a hyperparameter to be used by an algorithm.
--
-- /See:/ 'mkHyperParameterSpecification' smart constructor.
data HyperParameterSpecification = HyperParameterSpecification'
  { isTunable ::
      Lude.Maybe Lude.Bool,
    range :: Lude.Maybe ParameterRange,
    defaultValue ::
      Lude.Maybe Lude.Text,
    isRequired :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    type' :: ParameterType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HyperParameterSpecification' with the minimum fields required to make a request.
--
-- * 'defaultValue' - The default value for this hyperparameter. If a default value is specified, a hyperparameter cannot be required.
-- * 'description' - A brief description of the hyperparameter.
-- * 'isRequired' - Indicates whether this hyperparameter is required.
-- * 'isTunable' - Indicates whether this hyperparameter is tunable in a hyperparameter tuning job.
-- * 'name' - The name of this hyperparameter. The name must be unique.
-- * 'range' - The allowed range for this hyperparameter.
-- * 'type'' - The type of this hyperparameter. The valid types are @Integer@ , @Continuous@ , @Categorical@ , and @FreeText@ .
mkHyperParameterSpecification ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  ParameterType ->
  HyperParameterSpecification
mkHyperParameterSpecification pName_ pType_ =
  HyperParameterSpecification'
    { isTunable = Lude.Nothing,
      range = Lude.Nothing,
      defaultValue = Lude.Nothing,
      isRequired = Lude.Nothing,
      description = Lude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | Indicates whether this hyperparameter is tunable in a hyperparameter tuning job.
--
-- /Note:/ Consider using 'isTunable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsIsTunable :: Lens.Lens' HyperParameterSpecification (Lude.Maybe Lude.Bool)
hpsIsTunable = Lens.lens (isTunable :: HyperParameterSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {isTunable = a} :: HyperParameterSpecification)
{-# DEPRECATED hpsIsTunable "Use generic-lens or generic-optics with 'isTunable' instead." #-}

-- | The allowed range for this hyperparameter.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsRange :: Lens.Lens' HyperParameterSpecification (Lude.Maybe ParameterRange)
hpsRange = Lens.lens (range :: HyperParameterSpecification -> Lude.Maybe ParameterRange) (\s a -> s {range = a} :: HyperParameterSpecification)
{-# DEPRECATED hpsRange "Use generic-lens or generic-optics with 'range' instead." #-}

-- | The default value for this hyperparameter. If a default value is specified, a hyperparameter cannot be required.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsDefaultValue :: Lens.Lens' HyperParameterSpecification (Lude.Maybe Lude.Text)
hpsDefaultValue = Lens.lens (defaultValue :: HyperParameterSpecification -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: HyperParameterSpecification)
{-# DEPRECATED hpsDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Indicates whether this hyperparameter is required.
--
-- /Note:/ Consider using 'isRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsIsRequired :: Lens.Lens' HyperParameterSpecification (Lude.Maybe Lude.Bool)
hpsIsRequired = Lens.lens (isRequired :: HyperParameterSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {isRequired = a} :: HyperParameterSpecification)
{-# DEPRECATED hpsIsRequired "Use generic-lens or generic-optics with 'isRequired' instead." #-}

-- | A brief description of the hyperparameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsDescription :: Lens.Lens' HyperParameterSpecification (Lude.Maybe Lude.Text)
hpsDescription = Lens.lens (description :: HyperParameterSpecification -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: HyperParameterSpecification)
{-# DEPRECATED hpsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of this hyperparameter. The name must be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsName :: Lens.Lens' HyperParameterSpecification Lude.Text
hpsName = Lens.lens (name :: HyperParameterSpecification -> Lude.Text) (\s a -> s {name = a} :: HyperParameterSpecification)
{-# DEPRECATED hpsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of this hyperparameter. The valid types are @Integer@ , @Continuous@ , @Categorical@ , and @FreeText@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpsType :: Lens.Lens' HyperParameterSpecification ParameterType
hpsType = Lens.lens (type' :: HyperParameterSpecification -> ParameterType) (\s a -> s {type' = a} :: HyperParameterSpecification)
{-# DEPRECATED hpsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON HyperParameterSpecification where
  parseJSON =
    Lude.withObject
      "HyperParameterSpecification"
      ( \x ->
          HyperParameterSpecification'
            Lude.<$> (x Lude..:? "IsTunable")
            Lude.<*> (x Lude..:? "Range")
            Lude.<*> (x Lude..:? "DefaultValue")
            Lude.<*> (x Lude..:? "IsRequired")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON HyperParameterSpecification where
  toJSON HyperParameterSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IsTunable" Lude..=) Lude.<$> isTunable,
            ("Range" Lude..=) Lude.<$> range,
            ("DefaultValue" Lude..=) Lude.<$> defaultValue,
            ("IsRequired" Lude..=) Lude.<$> isRequired,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Type" Lude..= type')
          ]
      )
