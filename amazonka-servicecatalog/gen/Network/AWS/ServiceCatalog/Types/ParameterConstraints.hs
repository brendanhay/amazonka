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
-- Module      : Network.AWS.ServiceCatalog.Types.ParameterConstraints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ParameterConstraints where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The constraints that the administrator has put on the parameter.
--
-- /See:/ 'newParameterConstraints' smart constructor.
data ParameterConstraints = ParameterConstraints'
  { -- | A numeric value that determines the largest numeric value you want to
    -- allow for @Number@ types.
    maxValue :: Core.Maybe Core.Text,
    -- | An integer value that determines the smallest number of characters you
    -- want to allow for @String@ types.
    minLength :: Core.Maybe Core.Text,
    -- | The values that the administrator has allowed for the parameter.
    allowedValues :: Core.Maybe [Core.Text],
    -- | A numeric value that determines the smallest numeric value you want to
    -- allow for @Number@ types.
    minValue :: Core.Maybe Core.Text,
    -- | A string that explains a constraint when the constraint is violated. For
    -- example, without a constraint description, a parameter that has an
    -- allowed pattern of @[A-Za-z0-9]+@ displays the following error message
    -- when the user specifies an invalid value:
    --
    -- @Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+@
    --
    -- By adding a constraint description, such as must only contain letters
    -- (uppercase and lowercase) and numbers, you can display the following
    -- customized error message:
    --
    -- @Malformed input-Parameter MyParameter must only contain uppercase and lowercase letters and numbers.@
    constraintDescription :: Core.Maybe Core.Text,
    -- | An integer value that determines the largest number of characters you
    -- want to allow for @String@ types.
    maxLength :: Core.Maybe Core.Text,
    -- | A regular expression that represents the patterns that allow for
    -- @String@ types. The pattern must match the entire parameter value
    -- provided.
    allowedPattern :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterConstraints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxValue', 'parameterConstraints_maxValue' - A numeric value that determines the largest numeric value you want to
-- allow for @Number@ types.
--
-- 'minLength', 'parameterConstraints_minLength' - An integer value that determines the smallest number of characters you
-- want to allow for @String@ types.
--
-- 'allowedValues', 'parameterConstraints_allowedValues' - The values that the administrator has allowed for the parameter.
--
-- 'minValue', 'parameterConstraints_minValue' - A numeric value that determines the smallest numeric value you want to
-- allow for @Number@ types.
--
-- 'constraintDescription', 'parameterConstraints_constraintDescription' - A string that explains a constraint when the constraint is violated. For
-- example, without a constraint description, a parameter that has an
-- allowed pattern of @[A-Za-z0-9]+@ displays the following error message
-- when the user specifies an invalid value:
--
-- @Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+@
--
-- By adding a constraint description, such as must only contain letters
-- (uppercase and lowercase) and numbers, you can display the following
-- customized error message:
--
-- @Malformed input-Parameter MyParameter must only contain uppercase and lowercase letters and numbers.@
--
-- 'maxLength', 'parameterConstraints_maxLength' - An integer value that determines the largest number of characters you
-- want to allow for @String@ types.
--
-- 'allowedPattern', 'parameterConstraints_allowedPattern' - A regular expression that represents the patterns that allow for
-- @String@ types. The pattern must match the entire parameter value
-- provided.
newParameterConstraints ::
  ParameterConstraints
newParameterConstraints =
  ParameterConstraints'
    { maxValue = Core.Nothing,
      minLength = Core.Nothing,
      allowedValues = Core.Nothing,
      minValue = Core.Nothing,
      constraintDescription = Core.Nothing,
      maxLength = Core.Nothing,
      allowedPattern = Core.Nothing
    }

-- | A numeric value that determines the largest numeric value you want to
-- allow for @Number@ types.
parameterConstraints_maxValue :: Lens.Lens' ParameterConstraints (Core.Maybe Core.Text)
parameterConstraints_maxValue = Lens.lens (\ParameterConstraints' {maxValue} -> maxValue) (\s@ParameterConstraints' {} a -> s {maxValue = a} :: ParameterConstraints)

-- | An integer value that determines the smallest number of characters you
-- want to allow for @String@ types.
parameterConstraints_minLength :: Lens.Lens' ParameterConstraints (Core.Maybe Core.Text)
parameterConstraints_minLength = Lens.lens (\ParameterConstraints' {minLength} -> minLength) (\s@ParameterConstraints' {} a -> s {minLength = a} :: ParameterConstraints)

-- | The values that the administrator has allowed for the parameter.
parameterConstraints_allowedValues :: Lens.Lens' ParameterConstraints (Core.Maybe [Core.Text])
parameterConstraints_allowedValues = Lens.lens (\ParameterConstraints' {allowedValues} -> allowedValues) (\s@ParameterConstraints' {} a -> s {allowedValues = a} :: ParameterConstraints) Core.. Lens.mapping Lens._Coerce

-- | A numeric value that determines the smallest numeric value you want to
-- allow for @Number@ types.
parameterConstraints_minValue :: Lens.Lens' ParameterConstraints (Core.Maybe Core.Text)
parameterConstraints_minValue = Lens.lens (\ParameterConstraints' {minValue} -> minValue) (\s@ParameterConstraints' {} a -> s {minValue = a} :: ParameterConstraints)

-- | A string that explains a constraint when the constraint is violated. For
-- example, without a constraint description, a parameter that has an
-- allowed pattern of @[A-Za-z0-9]+@ displays the following error message
-- when the user specifies an invalid value:
--
-- @Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+@
--
-- By adding a constraint description, such as must only contain letters
-- (uppercase and lowercase) and numbers, you can display the following
-- customized error message:
--
-- @Malformed input-Parameter MyParameter must only contain uppercase and lowercase letters and numbers.@
parameterConstraints_constraintDescription :: Lens.Lens' ParameterConstraints (Core.Maybe Core.Text)
parameterConstraints_constraintDescription = Lens.lens (\ParameterConstraints' {constraintDescription} -> constraintDescription) (\s@ParameterConstraints' {} a -> s {constraintDescription = a} :: ParameterConstraints)

-- | An integer value that determines the largest number of characters you
-- want to allow for @String@ types.
parameterConstraints_maxLength :: Lens.Lens' ParameterConstraints (Core.Maybe Core.Text)
parameterConstraints_maxLength = Lens.lens (\ParameterConstraints' {maxLength} -> maxLength) (\s@ParameterConstraints' {} a -> s {maxLength = a} :: ParameterConstraints)

-- | A regular expression that represents the patterns that allow for
-- @String@ types. The pattern must match the entire parameter value
-- provided.
parameterConstraints_allowedPattern :: Lens.Lens' ParameterConstraints (Core.Maybe Core.Text)
parameterConstraints_allowedPattern = Lens.lens (\ParameterConstraints' {allowedPattern} -> allowedPattern) (\s@ParameterConstraints' {} a -> s {allowedPattern = a} :: ParameterConstraints)

instance Core.FromJSON ParameterConstraints where
  parseJSON =
    Core.withObject
      "ParameterConstraints"
      ( \x ->
          ParameterConstraints'
            Core.<$> (x Core..:? "MaxValue")
            Core.<*> (x Core..:? "MinLength")
            Core.<*> (x Core..:? "AllowedValues" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MinValue")
            Core.<*> (x Core..:? "ConstraintDescription")
            Core.<*> (x Core..:? "MaxLength")
            Core.<*> (x Core..:? "AllowedPattern")
      )

instance Core.Hashable ParameterConstraints

instance Core.NFData ParameterConstraints
