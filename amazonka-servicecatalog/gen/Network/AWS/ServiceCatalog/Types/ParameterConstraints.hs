{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The constraints that the administrator has put on the parameter.
--
-- /See:/ 'newParameterConstraints' smart constructor.
data ParameterConstraints = ParameterConstraints'
  { -- | A numeric value that determines the largest numeric value you want to
    -- allow for @Number@ types.
    maxValue :: Prelude.Maybe Prelude.Text,
    -- | An integer value that determines the smallest number of characters you
    -- want to allow for @String@ types.
    minLength :: Prelude.Maybe Prelude.Text,
    -- | The values that the administrator has allowed for the parameter.
    allowedValues :: Prelude.Maybe [Prelude.Text],
    -- | A numeric value that determines the smallest numeric value you want to
    -- allow for @Number@ types.
    minValue :: Prelude.Maybe Prelude.Text,
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
    constraintDescription :: Prelude.Maybe Prelude.Text,
    -- | An integer value that determines the largest number of characters you
    -- want to allow for @String@ types.
    maxLength :: Prelude.Maybe Prelude.Text,
    -- | A regular expression that represents the patterns that allow for
    -- @String@ types. The pattern must match the entire parameter value
    -- provided.
    allowedPattern :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { maxValue = Prelude.Nothing,
      minLength = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      minValue = Prelude.Nothing,
      constraintDescription = Prelude.Nothing,
      maxLength = Prelude.Nothing,
      allowedPattern = Prelude.Nothing
    }

-- | A numeric value that determines the largest numeric value you want to
-- allow for @Number@ types.
parameterConstraints_maxValue :: Lens.Lens' ParameterConstraints (Prelude.Maybe Prelude.Text)
parameterConstraints_maxValue = Lens.lens (\ParameterConstraints' {maxValue} -> maxValue) (\s@ParameterConstraints' {} a -> s {maxValue = a} :: ParameterConstraints)

-- | An integer value that determines the smallest number of characters you
-- want to allow for @String@ types.
parameterConstraints_minLength :: Lens.Lens' ParameterConstraints (Prelude.Maybe Prelude.Text)
parameterConstraints_minLength = Lens.lens (\ParameterConstraints' {minLength} -> minLength) (\s@ParameterConstraints' {} a -> s {minLength = a} :: ParameterConstraints)

-- | The values that the administrator has allowed for the parameter.
parameterConstraints_allowedValues :: Lens.Lens' ParameterConstraints (Prelude.Maybe [Prelude.Text])
parameterConstraints_allowedValues = Lens.lens (\ParameterConstraints' {allowedValues} -> allowedValues) (\s@ParameterConstraints' {} a -> s {allowedValues = a} :: ParameterConstraints) Prelude.. Lens.mapping Prelude._Coerce

-- | A numeric value that determines the smallest numeric value you want to
-- allow for @Number@ types.
parameterConstraints_minValue :: Lens.Lens' ParameterConstraints (Prelude.Maybe Prelude.Text)
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
parameterConstraints_constraintDescription :: Lens.Lens' ParameterConstraints (Prelude.Maybe Prelude.Text)
parameterConstraints_constraintDescription = Lens.lens (\ParameterConstraints' {constraintDescription} -> constraintDescription) (\s@ParameterConstraints' {} a -> s {constraintDescription = a} :: ParameterConstraints)

-- | An integer value that determines the largest number of characters you
-- want to allow for @String@ types.
parameterConstraints_maxLength :: Lens.Lens' ParameterConstraints (Prelude.Maybe Prelude.Text)
parameterConstraints_maxLength = Lens.lens (\ParameterConstraints' {maxLength} -> maxLength) (\s@ParameterConstraints' {} a -> s {maxLength = a} :: ParameterConstraints)

-- | A regular expression that represents the patterns that allow for
-- @String@ types. The pattern must match the entire parameter value
-- provided.
parameterConstraints_allowedPattern :: Lens.Lens' ParameterConstraints (Prelude.Maybe Prelude.Text)
parameterConstraints_allowedPattern = Lens.lens (\ParameterConstraints' {allowedPattern} -> allowedPattern) (\s@ParameterConstraints' {} a -> s {allowedPattern = a} :: ParameterConstraints)

instance Prelude.FromJSON ParameterConstraints where
  parseJSON =
    Prelude.withObject
      "ParameterConstraints"
      ( \x ->
          ParameterConstraints'
            Prelude.<$> (x Prelude..:? "MaxValue")
            Prelude.<*> (x Prelude..:? "MinLength")
            Prelude.<*> ( x Prelude..:? "AllowedValues"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "MinValue")
            Prelude.<*> (x Prelude..:? "ConstraintDescription")
            Prelude.<*> (x Prelude..:? "MaxLength")
            Prelude.<*> (x Prelude..:? "AllowedPattern")
      )

instance Prelude.Hashable ParameterConstraints

instance Prelude.NFData ParameterConstraints
