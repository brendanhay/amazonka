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
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Parameters supported by the application.
--
-- /See:/ 'newParameterDefinition' smart constructor.
data ParameterDefinition = ParameterDefinition'
  { -- | A numeric value that determines the largest numeric value that you want
    -- to allow for Number types.
    maxValue :: Core.Maybe Core.Int,
    -- | An integer value that determines the smallest number of characters that
    -- you want to allow for String types.
    minLength :: Core.Maybe Core.Int,
    -- | An array containing the list of values allowed for the parameter.
    allowedValues :: Core.Maybe [Core.Text],
    -- | A numeric value that determines the smallest numeric value that you want
    -- to allow for Number types.
    minValue :: Core.Maybe Core.Int,
    -- | A string of up to 4,000 characters that describes the parameter.
    description :: Core.Maybe Core.Text,
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
    constraintDescription :: Core.Maybe Core.Text,
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
    type' :: Core.Maybe Core.Text,
    -- | Whether to mask the parameter value whenever anyone makes a call that
    -- describes the stack. If you set the value to true, the parameter value
    -- is masked with asterisks (*****).
    noEcho :: Core.Maybe Core.Bool,
    -- | An integer value that determines the largest number of characters that
    -- you want to allow for String types.
    maxLength :: Core.Maybe Core.Int,
    -- | A regular expression that represents the patterns to allow for String
    -- types.
    allowedPattern :: Core.Maybe Core.Text,
    -- | A value of the appropriate type for the template to use if no value is
    -- specified when a stack is created. If you define constraints for the
    -- parameter, you must specify a value that adheres to those constraints.
    defaultValue :: Core.Maybe Core.Text,
    -- | A list of AWS SAM resources that use this parameter.
    referencedByResources :: [Core.Text],
    -- | The name of the parameter.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxValue', 'parameterDefinition_maxValue' - A numeric value that determines the largest numeric value that you want
-- to allow for Number types.
--
-- 'minLength', 'parameterDefinition_minLength' - An integer value that determines the smallest number of characters that
-- you want to allow for String types.
--
-- 'allowedValues', 'parameterDefinition_allowedValues' - An array containing the list of values allowed for the parameter.
--
-- 'minValue', 'parameterDefinition_minValue' - A numeric value that determines the smallest numeric value that you want
-- to allow for Number types.
--
-- 'description', 'parameterDefinition_description' - A string of up to 4,000 characters that describes the parameter.
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
-- 'noEcho', 'parameterDefinition_noEcho' - Whether to mask the parameter value whenever anyone makes a call that
-- describes the stack. If you set the value to true, the parameter value
-- is masked with asterisks (*****).
--
-- 'maxLength', 'parameterDefinition_maxLength' - An integer value that determines the largest number of characters that
-- you want to allow for String types.
--
-- 'allowedPattern', 'parameterDefinition_allowedPattern' - A regular expression that represents the patterns to allow for String
-- types.
--
-- 'defaultValue', 'parameterDefinition_defaultValue' - A value of the appropriate type for the template to use if no value is
-- specified when a stack is created. If you define constraints for the
-- parameter, you must specify a value that adheres to those constraints.
--
-- 'referencedByResources', 'parameterDefinition_referencedByResources' - A list of AWS SAM resources that use this parameter.
--
-- 'name', 'parameterDefinition_name' - The name of the parameter.
newParameterDefinition ::
  -- | 'name'
  Core.Text ->
  ParameterDefinition
newParameterDefinition pName_ =
  ParameterDefinition'
    { maxValue = Core.Nothing,
      minLength = Core.Nothing,
      allowedValues = Core.Nothing,
      minValue = Core.Nothing,
      description = Core.Nothing,
      constraintDescription = Core.Nothing,
      type' = Core.Nothing,
      noEcho = Core.Nothing,
      maxLength = Core.Nothing,
      allowedPattern = Core.Nothing,
      defaultValue = Core.Nothing,
      referencedByResources = Core.mempty,
      name = pName_
    }

-- | A numeric value that determines the largest numeric value that you want
-- to allow for Number types.
parameterDefinition_maxValue :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Int)
parameterDefinition_maxValue = Lens.lens (\ParameterDefinition' {maxValue} -> maxValue) (\s@ParameterDefinition' {} a -> s {maxValue = a} :: ParameterDefinition)

-- | An integer value that determines the smallest number of characters that
-- you want to allow for String types.
parameterDefinition_minLength :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Int)
parameterDefinition_minLength = Lens.lens (\ParameterDefinition' {minLength} -> minLength) (\s@ParameterDefinition' {} a -> s {minLength = a} :: ParameterDefinition)

-- | An array containing the list of values allowed for the parameter.
parameterDefinition_allowedValues :: Lens.Lens' ParameterDefinition (Core.Maybe [Core.Text])
parameterDefinition_allowedValues = Lens.lens (\ParameterDefinition' {allowedValues} -> allowedValues) (\s@ParameterDefinition' {} a -> s {allowedValues = a} :: ParameterDefinition) Core.. Lens.mapping Lens._Coerce

-- | A numeric value that determines the smallest numeric value that you want
-- to allow for Number types.
parameterDefinition_minValue :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Int)
parameterDefinition_minValue = Lens.lens (\ParameterDefinition' {minValue} -> minValue) (\s@ParameterDefinition' {} a -> s {minValue = a} :: ParameterDefinition)

-- | A string of up to 4,000 characters that describes the parameter.
parameterDefinition_description :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
parameterDefinition_description = Lens.lens (\ParameterDefinition' {description} -> description) (\s@ParameterDefinition' {} a -> s {description = a} :: ParameterDefinition)

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
parameterDefinition_constraintDescription :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
parameterDefinition_constraintDescription = Lens.lens (\ParameterDefinition' {constraintDescription} -> constraintDescription) (\s@ParameterDefinition' {} a -> s {constraintDescription = a} :: ParameterDefinition)

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
parameterDefinition_type :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
parameterDefinition_type = Lens.lens (\ParameterDefinition' {type'} -> type') (\s@ParameterDefinition' {} a -> s {type' = a} :: ParameterDefinition)

-- | Whether to mask the parameter value whenever anyone makes a call that
-- describes the stack. If you set the value to true, the parameter value
-- is masked with asterisks (*****).
parameterDefinition_noEcho :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Bool)
parameterDefinition_noEcho = Lens.lens (\ParameterDefinition' {noEcho} -> noEcho) (\s@ParameterDefinition' {} a -> s {noEcho = a} :: ParameterDefinition)

-- | An integer value that determines the largest number of characters that
-- you want to allow for String types.
parameterDefinition_maxLength :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Int)
parameterDefinition_maxLength = Lens.lens (\ParameterDefinition' {maxLength} -> maxLength) (\s@ParameterDefinition' {} a -> s {maxLength = a} :: ParameterDefinition)

-- | A regular expression that represents the patterns to allow for String
-- types.
parameterDefinition_allowedPattern :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
parameterDefinition_allowedPattern = Lens.lens (\ParameterDefinition' {allowedPattern} -> allowedPattern) (\s@ParameterDefinition' {} a -> s {allowedPattern = a} :: ParameterDefinition)

-- | A value of the appropriate type for the template to use if no value is
-- specified when a stack is created. If you define constraints for the
-- parameter, you must specify a value that adheres to those constraints.
parameterDefinition_defaultValue :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
parameterDefinition_defaultValue = Lens.lens (\ParameterDefinition' {defaultValue} -> defaultValue) (\s@ParameterDefinition' {} a -> s {defaultValue = a} :: ParameterDefinition)

-- | A list of AWS SAM resources that use this parameter.
parameterDefinition_referencedByResources :: Lens.Lens' ParameterDefinition [Core.Text]
parameterDefinition_referencedByResources = Lens.lens (\ParameterDefinition' {referencedByResources} -> referencedByResources) (\s@ParameterDefinition' {} a -> s {referencedByResources = a} :: ParameterDefinition) Core.. Lens._Coerce

-- | The name of the parameter.
parameterDefinition_name :: Lens.Lens' ParameterDefinition Core.Text
parameterDefinition_name = Lens.lens (\ParameterDefinition' {name} -> name) (\s@ParameterDefinition' {} a -> s {name = a} :: ParameterDefinition)

instance Core.FromJSON ParameterDefinition where
  parseJSON =
    Core.withObject
      "ParameterDefinition"
      ( \x ->
          ParameterDefinition'
            Core.<$> (x Core..:? "maxValue")
            Core.<*> (x Core..:? "minLength")
            Core.<*> (x Core..:? "allowedValues" Core..!= Core.mempty)
            Core.<*> (x Core..:? "minValue")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "constraintDescription")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "noEcho")
            Core.<*> (x Core..:? "maxLength")
            Core.<*> (x Core..:? "allowedPattern")
            Core.<*> (x Core..:? "defaultValue")
            Core.<*> ( x Core..:? "referencedByResources"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..: "name")
      )

instance Core.Hashable ParameterDefinition

instance Core.NFData ParameterDefinition
