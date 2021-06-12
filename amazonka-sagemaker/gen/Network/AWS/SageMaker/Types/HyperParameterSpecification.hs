{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ParameterRange
import Network.AWS.SageMaker.Types.ParameterType

-- | Defines a hyperparameter to be used by an algorithm.
--
-- /See:/ 'newHyperParameterSpecification' smart constructor.
data HyperParameterSpecification = HyperParameterSpecification'
  { -- | The allowed range for this hyperparameter.
    range :: Core.Maybe ParameterRange,
    -- | Indicates whether this hyperparameter is tunable in a hyperparameter
    -- tuning job.
    isTunable :: Core.Maybe Core.Bool,
    -- | A brief description of the hyperparameter.
    description :: Core.Maybe Core.Text,
    -- | Indicates whether this hyperparameter is required.
    isRequired :: Core.Maybe Core.Bool,
    -- | The default value for this hyperparameter. If a default value is
    -- specified, a hyperparameter cannot be required.
    defaultValue :: Core.Maybe Core.Text,
    -- | The name of this hyperparameter. The name must be unique.
    name :: Core.Text,
    -- | The type of this hyperparameter. The valid types are @Integer@,
    -- @Continuous@, @Categorical@, and @FreeText@.
    type' :: ParameterType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HyperParameterSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'range', 'hyperParameterSpecification_range' - The allowed range for this hyperparameter.
--
-- 'isTunable', 'hyperParameterSpecification_isTunable' - Indicates whether this hyperparameter is tunable in a hyperparameter
-- tuning job.
--
-- 'description', 'hyperParameterSpecification_description' - A brief description of the hyperparameter.
--
-- 'isRequired', 'hyperParameterSpecification_isRequired' - Indicates whether this hyperparameter is required.
--
-- 'defaultValue', 'hyperParameterSpecification_defaultValue' - The default value for this hyperparameter. If a default value is
-- specified, a hyperparameter cannot be required.
--
-- 'name', 'hyperParameterSpecification_name' - The name of this hyperparameter. The name must be unique.
--
-- 'type'', 'hyperParameterSpecification_type' - The type of this hyperparameter. The valid types are @Integer@,
-- @Continuous@, @Categorical@, and @FreeText@.
newHyperParameterSpecification ::
  -- | 'name'
  Core.Text ->
  -- | 'type''
  ParameterType ->
  HyperParameterSpecification
newHyperParameterSpecification pName_ pType_ =
  HyperParameterSpecification'
    { range = Core.Nothing,
      isTunable = Core.Nothing,
      description = Core.Nothing,
      isRequired = Core.Nothing,
      defaultValue = Core.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The allowed range for this hyperparameter.
hyperParameterSpecification_range :: Lens.Lens' HyperParameterSpecification (Core.Maybe ParameterRange)
hyperParameterSpecification_range = Lens.lens (\HyperParameterSpecification' {range} -> range) (\s@HyperParameterSpecification' {} a -> s {range = a} :: HyperParameterSpecification)

-- | Indicates whether this hyperparameter is tunable in a hyperparameter
-- tuning job.
hyperParameterSpecification_isTunable :: Lens.Lens' HyperParameterSpecification (Core.Maybe Core.Bool)
hyperParameterSpecification_isTunable = Lens.lens (\HyperParameterSpecification' {isTunable} -> isTunable) (\s@HyperParameterSpecification' {} a -> s {isTunable = a} :: HyperParameterSpecification)

-- | A brief description of the hyperparameter.
hyperParameterSpecification_description :: Lens.Lens' HyperParameterSpecification (Core.Maybe Core.Text)
hyperParameterSpecification_description = Lens.lens (\HyperParameterSpecification' {description} -> description) (\s@HyperParameterSpecification' {} a -> s {description = a} :: HyperParameterSpecification)

-- | Indicates whether this hyperparameter is required.
hyperParameterSpecification_isRequired :: Lens.Lens' HyperParameterSpecification (Core.Maybe Core.Bool)
hyperParameterSpecification_isRequired = Lens.lens (\HyperParameterSpecification' {isRequired} -> isRequired) (\s@HyperParameterSpecification' {} a -> s {isRequired = a} :: HyperParameterSpecification)

-- | The default value for this hyperparameter. If a default value is
-- specified, a hyperparameter cannot be required.
hyperParameterSpecification_defaultValue :: Lens.Lens' HyperParameterSpecification (Core.Maybe Core.Text)
hyperParameterSpecification_defaultValue = Lens.lens (\HyperParameterSpecification' {defaultValue} -> defaultValue) (\s@HyperParameterSpecification' {} a -> s {defaultValue = a} :: HyperParameterSpecification)

-- | The name of this hyperparameter. The name must be unique.
hyperParameterSpecification_name :: Lens.Lens' HyperParameterSpecification Core.Text
hyperParameterSpecification_name = Lens.lens (\HyperParameterSpecification' {name} -> name) (\s@HyperParameterSpecification' {} a -> s {name = a} :: HyperParameterSpecification)

-- | The type of this hyperparameter. The valid types are @Integer@,
-- @Continuous@, @Categorical@, and @FreeText@.
hyperParameterSpecification_type :: Lens.Lens' HyperParameterSpecification ParameterType
hyperParameterSpecification_type = Lens.lens (\HyperParameterSpecification' {type'} -> type') (\s@HyperParameterSpecification' {} a -> s {type' = a} :: HyperParameterSpecification)

instance Core.FromJSON HyperParameterSpecification where
  parseJSON =
    Core.withObject
      "HyperParameterSpecification"
      ( \x ->
          HyperParameterSpecification'
            Core.<$> (x Core..:? "Range")
            Core.<*> (x Core..:? "IsTunable")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "IsRequired")
            Core.<*> (x Core..:? "DefaultValue")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Type")
      )

instance Core.Hashable HyperParameterSpecification

instance Core.NFData HyperParameterSpecification

instance Core.ToJSON HyperParameterSpecification where
  toJSON HyperParameterSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Range" Core..=) Core.<$> range,
            ("IsTunable" Core..=) Core.<$> isTunable,
            ("Description" Core..=) Core.<$> description,
            ("IsRequired" Core..=) Core.<$> isRequired,
            ("DefaultValue" Core..=) Core.<$> defaultValue,
            Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type')
          ]
      )
