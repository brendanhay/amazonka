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
-- Module      : Amazonka.ServerlessApplicationRepository.Types.ParameterDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServerlessApplicationRepository.Types.ParameterDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters supported by the application.
--
-- /See:/ 'newParameterDefinition' smart constructor.
data ParameterDefinition = ParameterDefinition'
  { -- | A regular expression that represents the patterns to allow for String
    -- types.
    allowedPattern :: Prelude.Maybe Prelude.Text,
    -- | An array containing the list of values allowed for the parameter.
    allowedValues :: Prelude.Maybe [Prelude.Text],
    -- | A string that explains a constraint when the constraint is violated. For
    -- example, without a constraint description, a parameter that has an
    -- allowed pattern of [A-Za-z0-9]+ displays the following error message
    -- when the user specifies an invalid value:
    --
    -- Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+
    --
    -- By adding a constraint description, such as \"must contain only
    -- uppercase and lowercase letters and numbers,\" you can display the
    -- following customized error message:
    --
    -- Malformed input-Parameter MyParameter must contain only uppercase and
    -- lowercase letters and numbers.
    constraintDescription :: Prelude.Maybe Prelude.Text,
    -- | A value of the appropriate type for the template to use if no value is
    -- specified when a stack is created. If you define constraints for the
    -- parameter, you must specify a value that adheres to those constraints.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | A string of up to 4,000 characters that describes the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | An integer value that determines the largest number of characters that
    -- you want to allow for String types.
    maxLength :: Prelude.Maybe Prelude.Int,
    -- | A numeric value that determines the largest numeric value that you want
    -- to allow for Number types.
    maxValue :: Prelude.Maybe Prelude.Int,
    -- | An integer value that determines the smallest number of characters that
    -- you want to allow for String types.
    minLength :: Prelude.Maybe Prelude.Int,
    -- | A numeric value that determines the smallest numeric value that you want
    -- to allow for Number types.
    minValue :: Prelude.Maybe Prelude.Int,
    -- | Whether to mask the parameter value whenever anyone makes a call that
    -- describes the stack. If you set the value to true, the parameter value
    -- is masked with asterisks (*****).
    noEcho :: Prelude.Maybe Prelude.Bool,
    -- | The type of the parameter.
    --
    -- Valid values: String | Number | List\<Number> | CommaDelimitedList
    --
    -- String: A literal string.
    --
    -- For example, users can specify \"MyUserName\".
    --
    -- Number: An integer or float. AWS CloudFormation validates the parameter
    -- value as a number. However, when you use the parameter elsewhere in your
    -- template (for example, by using the Ref intrinsic function), the
    -- parameter value becomes a string.
    --
    -- For example, users might specify \"8888\".
    --
    -- List\<Number>: An array of integers or floats that are separated by
    -- commas. AWS CloudFormation validates the parameter value as numbers.
    -- However, when you use the parameter elsewhere in your template (for
    -- example, by using the Ref intrinsic function), the parameter value
    -- becomes a list of strings.
    --
    -- For example, users might specify \"80,20\", and then Ref results in
    -- [\"80\",\"20\"].
    --
    -- CommaDelimitedList: An array of literal strings that are separated by
    -- commas. The total number of strings should be one more than the total
    -- number of commas. Also, each member string is space-trimmed.
    --
    -- For example, users might specify \"test,dev,prod\", and then Ref results
    -- in [\"test\",\"dev\",\"prod\"].
    type' :: Prelude.Maybe Prelude.Text,
    -- | A list of AWS SAM resources that use this parameter.
    referencedByResources :: [Prelude.Text],
    -- | The name of the parameter.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedPattern', 'parameterDefinition_allowedPattern' - A regular expression that represents the patterns to allow for String
-- types.
--
-- 'allowedValues', 'parameterDefinition_allowedValues' - An array containing the list of values allowed for the parameter.
--
-- 'constraintDescription', 'parameterDefinition_constraintDescription' - A string that explains a constraint when the constraint is violated. For
-- example, without a constraint description, a parameter that has an
-- allowed pattern of [A-Za-z0-9]+ displays the following error message
-- when the user specifies an invalid value:
--
-- Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+
--
-- By adding a constraint description, such as \"must contain only
-- uppercase and lowercase letters and numbers,\" you can display the
-- following customized error message:
--
-- Malformed input-Parameter MyParameter must contain only uppercase and
-- lowercase letters and numbers.
--
-- 'defaultValue', 'parameterDefinition_defaultValue' - A value of the appropriate type for the template to use if no value is
-- specified when a stack is created. If you define constraints for the
-- parameter, you must specify a value that adheres to those constraints.
--
-- 'description', 'parameterDefinition_description' - A string of up to 4,000 characters that describes the parameter.
--
-- 'maxLength', 'parameterDefinition_maxLength' - An integer value that determines the largest number of characters that
-- you want to allow for String types.
--
-- 'maxValue', 'parameterDefinition_maxValue' - A numeric value that determines the largest numeric value that you want
-- to allow for Number types.
--
-- 'minLength', 'parameterDefinition_minLength' - An integer value that determines the smallest number of characters that
-- you want to allow for String types.
--
-- 'minValue', 'parameterDefinition_minValue' - A numeric value that determines the smallest numeric value that you want
-- to allow for Number types.
--
-- 'noEcho', 'parameterDefinition_noEcho' - Whether to mask the parameter value whenever anyone makes a call that
-- describes the stack. If you set the value to true, the parameter value
-- is masked with asterisks (*****).
--
-- 'type'', 'parameterDefinition_type' - The type of the parameter.
--
-- Valid values: String | Number | List\<Number> | CommaDelimitedList
--
-- String: A literal string.
--
-- For example, users can specify \"MyUserName\".
--
-- Number: An integer or float. AWS CloudFormation validates the parameter
-- value as a number. However, when you use the parameter elsewhere in your
-- template (for example, by using the Ref intrinsic function), the
-- parameter value becomes a string.
--
-- For example, users might specify \"8888\".
--
-- List\<Number>: An array of integers or floats that are separated by
-- commas. AWS CloudFormation validates the parameter value as numbers.
-- However, when you use the parameter elsewhere in your template (for
-- example, by using the Ref intrinsic function), the parameter value
-- becomes a list of strings.
--
-- For example, users might specify \"80,20\", and then Ref results in
-- [\"80\",\"20\"].
--
-- CommaDelimitedList: An array of literal strings that are separated by
-- commas. The total number of strings should be one more than the total
-- number of commas. Also, each member string is space-trimmed.
--
-- For example, users might specify \"test,dev,prod\", and then Ref results
-- in [\"test\",\"dev\",\"prod\"].
--
-- 'referencedByResources', 'parameterDefinition_referencedByResources' - A list of AWS SAM resources that use this parameter.
--
-- 'name', 'parameterDefinition_name' - The name of the parameter.
newParameterDefinition ::
  -- | 'name'
  Prelude.Text ->
  ParameterDefinition
newParameterDefinition pName_ =
  ParameterDefinition'
    { allowedPattern =
        Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      constraintDescription = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      description = Prelude.Nothing,
      maxLength = Prelude.Nothing,
      maxValue = Prelude.Nothing,
      minLength = Prelude.Nothing,
      minValue = Prelude.Nothing,
      noEcho = Prelude.Nothing,
      type' = Prelude.Nothing,
      referencedByResources = Prelude.mempty,
      name = pName_
    }

-- | A regular expression that represents the patterns to allow for String
-- types.
parameterDefinition_allowedPattern :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Text)
parameterDefinition_allowedPattern = Lens.lens (\ParameterDefinition' {allowedPattern} -> allowedPattern) (\s@ParameterDefinition' {} a -> s {allowedPattern = a} :: ParameterDefinition)

-- | An array containing the list of values allowed for the parameter.
parameterDefinition_allowedValues :: Lens.Lens' ParameterDefinition (Prelude.Maybe [Prelude.Text])
parameterDefinition_allowedValues = Lens.lens (\ParameterDefinition' {allowedValues} -> allowedValues) (\s@ParameterDefinition' {} a -> s {allowedValues = a} :: ParameterDefinition) Prelude.. Lens.mapping Lens.coerced

-- | A string that explains a constraint when the constraint is violated. For
-- example, without a constraint description, a parameter that has an
-- allowed pattern of [A-Za-z0-9]+ displays the following error message
-- when the user specifies an invalid value:
--
-- Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+
--
-- By adding a constraint description, such as \"must contain only
-- uppercase and lowercase letters and numbers,\" you can display the
-- following customized error message:
--
-- Malformed input-Parameter MyParameter must contain only uppercase and
-- lowercase letters and numbers.
parameterDefinition_constraintDescription :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Text)
parameterDefinition_constraintDescription = Lens.lens (\ParameterDefinition' {constraintDescription} -> constraintDescription) (\s@ParameterDefinition' {} a -> s {constraintDescription = a} :: ParameterDefinition)

-- | A value of the appropriate type for the template to use if no value is
-- specified when a stack is created. If you define constraints for the
-- parameter, you must specify a value that adheres to those constraints.
parameterDefinition_defaultValue :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Text)
parameterDefinition_defaultValue = Lens.lens (\ParameterDefinition' {defaultValue} -> defaultValue) (\s@ParameterDefinition' {} a -> s {defaultValue = a} :: ParameterDefinition)

-- | A string of up to 4,000 characters that describes the parameter.
parameterDefinition_description :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Text)
parameterDefinition_description = Lens.lens (\ParameterDefinition' {description} -> description) (\s@ParameterDefinition' {} a -> s {description = a} :: ParameterDefinition)

-- | An integer value that determines the largest number of characters that
-- you want to allow for String types.
parameterDefinition_maxLength :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Int)
parameterDefinition_maxLength = Lens.lens (\ParameterDefinition' {maxLength} -> maxLength) (\s@ParameterDefinition' {} a -> s {maxLength = a} :: ParameterDefinition)

-- | A numeric value that determines the largest numeric value that you want
-- to allow for Number types.
parameterDefinition_maxValue :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Int)
parameterDefinition_maxValue = Lens.lens (\ParameterDefinition' {maxValue} -> maxValue) (\s@ParameterDefinition' {} a -> s {maxValue = a} :: ParameterDefinition)

-- | An integer value that determines the smallest number of characters that
-- you want to allow for String types.
parameterDefinition_minLength :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Int)
parameterDefinition_minLength = Lens.lens (\ParameterDefinition' {minLength} -> minLength) (\s@ParameterDefinition' {} a -> s {minLength = a} :: ParameterDefinition)

-- | A numeric value that determines the smallest numeric value that you want
-- to allow for Number types.
parameterDefinition_minValue :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Int)
parameterDefinition_minValue = Lens.lens (\ParameterDefinition' {minValue} -> minValue) (\s@ParameterDefinition' {} a -> s {minValue = a} :: ParameterDefinition)

-- | Whether to mask the parameter value whenever anyone makes a call that
-- describes the stack. If you set the value to true, the parameter value
-- is masked with asterisks (*****).
parameterDefinition_noEcho :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Bool)
parameterDefinition_noEcho = Lens.lens (\ParameterDefinition' {noEcho} -> noEcho) (\s@ParameterDefinition' {} a -> s {noEcho = a} :: ParameterDefinition)

-- | The type of the parameter.
--
-- Valid values: String | Number | List\<Number> | CommaDelimitedList
--
-- String: A literal string.
--
-- For example, users can specify \"MyUserName\".
--
-- Number: An integer or float. AWS CloudFormation validates the parameter
-- value as a number. However, when you use the parameter elsewhere in your
-- template (for example, by using the Ref intrinsic function), the
-- parameter value becomes a string.
--
-- For example, users might specify \"8888\".
--
-- List\<Number>: An array of integers or floats that are separated by
-- commas. AWS CloudFormation validates the parameter value as numbers.
-- However, when you use the parameter elsewhere in your template (for
-- example, by using the Ref intrinsic function), the parameter value
-- becomes a list of strings.
--
-- For example, users might specify \"80,20\", and then Ref results in
-- [\"80\",\"20\"].
--
-- CommaDelimitedList: An array of literal strings that are separated by
-- commas. The total number of strings should be one more than the total
-- number of commas. Also, each member string is space-trimmed.
--
-- For example, users might specify \"test,dev,prod\", and then Ref results
-- in [\"test\",\"dev\",\"prod\"].
parameterDefinition_type :: Lens.Lens' ParameterDefinition (Prelude.Maybe Prelude.Text)
parameterDefinition_type = Lens.lens (\ParameterDefinition' {type'} -> type') (\s@ParameterDefinition' {} a -> s {type' = a} :: ParameterDefinition)

-- | A list of AWS SAM resources that use this parameter.
parameterDefinition_referencedByResources :: Lens.Lens' ParameterDefinition [Prelude.Text]
parameterDefinition_referencedByResources = Lens.lens (\ParameterDefinition' {referencedByResources} -> referencedByResources) (\s@ParameterDefinition' {} a -> s {referencedByResources = a} :: ParameterDefinition) Prelude.. Lens.coerced

-- | The name of the parameter.
parameterDefinition_name :: Lens.Lens' ParameterDefinition Prelude.Text
parameterDefinition_name = Lens.lens (\ParameterDefinition' {name} -> name) (\s@ParameterDefinition' {} a -> s {name = a} :: ParameterDefinition)

instance Data.FromJSON ParameterDefinition where
  parseJSON =
    Data.withObject
      "ParameterDefinition"
      ( \x ->
          ParameterDefinition'
            Prelude.<$> (x Data..:? "allowedPattern")
            Prelude.<*> (x Data..:? "allowedValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "constraintDescription")
            Prelude.<*> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "maxLength")
            Prelude.<*> (x Data..:? "maxValue")
            Prelude.<*> (x Data..:? "minLength")
            Prelude.<*> (x Data..:? "minValue")
            Prelude.<*> (x Data..:? "noEcho")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> ( x
                            Data..:? "referencedByResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ParameterDefinition where
  hashWithSalt _salt ParameterDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` allowedPattern
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` constraintDescription
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxLength
      `Prelude.hashWithSalt` maxValue
      `Prelude.hashWithSalt` minLength
      `Prelude.hashWithSalt` minValue
      `Prelude.hashWithSalt` noEcho
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` referencedByResources
      `Prelude.hashWithSalt` name

instance Prelude.NFData ParameterDefinition where
  rnf ParameterDefinition' {..} =
    Prelude.rnf allowedPattern
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf constraintDescription
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxLength
      `Prelude.seq` Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf minLength
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf noEcho
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf referencedByResources
      `Prelude.seq` Prelude.rnf name
