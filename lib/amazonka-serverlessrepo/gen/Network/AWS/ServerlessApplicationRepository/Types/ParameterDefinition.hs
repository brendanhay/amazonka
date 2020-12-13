{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
  ( ParameterDefinition (..),

    -- * Smart constructor
    mkParameterDefinition,

    -- * Lenses
    pdMaxValue,
    pdReferencedByResources,
    pdMaxLength,
    pdConstraintDescription,
    pdMinLength,
    pdName,
    pdDefaultValue,
    pdAllowedPattern,
    pdNoEcho,
    pdType,
    pdAllowedValues,
    pdDescription,
    pdMinValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Parameters supported by the application.
--
-- /See:/ 'mkParameterDefinition' smart constructor.
data ParameterDefinition = ParameterDefinition'
  { -- | A numeric value that determines the largest numeric value that you want to allow for Number types.
    maxValue :: Lude.Maybe Lude.Int,
    -- | A list of AWS SAM resources that use this parameter.
    referencedByResources :: [Lude.Text],
    -- | An integer value that determines the largest number of characters that you want to allow for String types.
    maxLength :: Lude.Maybe Lude.Int,
    -- | A string that explains a constraint when the constraint is violated. For example, without a constraint description,
    --
    --  a parameter that has an allowed pattern of [A-Za-z0-9]+ displays the following error message when the user
    --  specifies an invalid value:
    --
    --  Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+
    --
    -- By adding a constraint description, such as "must contain only uppercase and lowercase letters and numbers," you can display
    --  the following customized error message:
    --
    --  Malformed input-Parameter MyParameter must contain only uppercase and lowercase letters and numbers.
    constraintDescription :: Lude.Maybe Lude.Text,
    -- | An integer value that determines the smallest number of characters that you want to allow for String types.
    minLength :: Lude.Maybe Lude.Int,
    -- | The name of the parameter.
    name :: Lude.Text,
    -- | A value of the appropriate type for the template to use if no value is specified when a stack is created.
    --
    --  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
    defaultValue :: Lude.Maybe Lude.Text,
    -- | A regular expression that represents the patterns to allow for String types.
    allowedPattern :: Lude.Maybe Lude.Text,
    -- | Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the
    --
    --  value to true, the parameter value is masked with asterisks (*****).
    noEcho :: Lude.Maybe Lude.Bool,
    -- | The type of the parameter.
    --
    -- Valid values: String | Number | List<Number> | CommaDelimitedList
    --
    --
    --  String: A literal string.
    -- For example, users can specify "MyUserName".
    --
    --  Number: An integer or float. AWS CloudFormation validates the parameter value as a number. However, when you use the
    --  parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a string.
    -- For example, users might specify "8888".
    --
    --  List<Number>: An array of integers or floats that are separated by commas. AWS CloudFormation validates the parameter value as numbers. However, when
    --  you use the parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a list of strings.
    -- For example, users might specify "80,20", and then Ref results in ["80","20"].
    --
    --  CommaDelimitedList: An array of literal strings that are separated by commas. The total number of strings should be one more than the total number of commas.
    --  Also, each member string is space-trimmed.
    -- For example, users might specify "test,dev,prod", and then Ref results in ["test","dev","prod"].
    type' :: Lude.Maybe Lude.Text,
    -- | An array containing the list of values allowed for the parameter.
    allowedValues :: Lude.Maybe [Lude.Text],
    -- | A string of up to 4,000 characters that describes the parameter.
    description :: Lude.Maybe Lude.Text,
    -- | A numeric value that determines the smallest numeric value that you want to allow for Number types.
    minValue :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterDefinition' with the minimum fields required to make a request.
--
-- * 'maxValue' - A numeric value that determines the largest numeric value that you want to allow for Number types.
-- * 'referencedByResources' - A list of AWS SAM resources that use this parameter.
-- * 'maxLength' - An integer value that determines the largest number of characters that you want to allow for String types.
-- * 'constraintDescription' - A string that explains a constraint when the constraint is violated. For example, without a constraint description,
--
--  a parameter that has an allowed pattern of [A-Za-z0-9]+ displays the following error message when the user
--  specifies an invalid value:
--
--  Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+
--
-- By adding a constraint description, such as "must contain only uppercase and lowercase letters and numbers," you can display
--  the following customized error message:
--
--  Malformed input-Parameter MyParameter must contain only uppercase and lowercase letters and numbers.
--
-- * 'minLength' - An integer value that determines the smallest number of characters that you want to allow for String types.
-- * 'name' - The name of the parameter.
-- * 'defaultValue' - A value of the appropriate type for the template to use if no value is specified when a stack is created.
--
--  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
-- * 'allowedPattern' - A regular expression that represents the patterns to allow for String types.
-- * 'noEcho' - Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the
--
--  value to true, the parameter value is masked with asterisks (*****).
-- * 'type'' - The type of the parameter.
--
-- Valid values: String | Number | List<Number> | CommaDelimitedList
--
--
--  String: A literal string.
-- For example, users can specify "MyUserName".
--
--  Number: An integer or float. AWS CloudFormation validates the parameter value as a number. However, when you use the
--  parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a string.
-- For example, users might specify "8888".
--
--  List<Number>: An array of integers or floats that are separated by commas. AWS CloudFormation validates the parameter value as numbers. However, when
--  you use the parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a list of strings.
-- For example, users might specify "80,20", and then Ref results in ["80","20"].
--
--  CommaDelimitedList: An array of literal strings that are separated by commas. The total number of strings should be one more than the total number of commas.
--  Also, each member string is space-trimmed.
-- For example, users might specify "test,dev,prod", and then Ref results in ["test","dev","prod"].
-- * 'allowedValues' - An array containing the list of values allowed for the parameter.
-- * 'description' - A string of up to 4,000 characters that describes the parameter.
-- * 'minValue' - A numeric value that determines the smallest numeric value that you want to allow for Number types.
mkParameterDefinition ::
  -- | 'name'
  Lude.Text ->
  ParameterDefinition
mkParameterDefinition pName_ =
  ParameterDefinition'
    { maxValue = Lude.Nothing,
      referencedByResources = Lude.mempty,
      maxLength = Lude.Nothing,
      constraintDescription = Lude.Nothing,
      minLength = Lude.Nothing,
      name = pName_,
      defaultValue = Lude.Nothing,
      allowedPattern = Lude.Nothing,
      noEcho = Lude.Nothing,
      type' = Lude.Nothing,
      allowedValues = Lude.Nothing,
      description = Lude.Nothing,
      minValue = Lude.Nothing
    }

-- | A numeric value that determines the largest numeric value that you want to allow for Number types.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMaxValue :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Int)
pdMaxValue = Lens.lens (maxValue :: ParameterDefinition -> Lude.Maybe Lude.Int) (\s a -> s {maxValue = a} :: ParameterDefinition)
{-# DEPRECATED pdMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

-- | A list of AWS SAM resources that use this parameter.
--
-- /Note:/ Consider using 'referencedByResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdReferencedByResources :: Lens.Lens' ParameterDefinition [Lude.Text]
pdReferencedByResources = Lens.lens (referencedByResources :: ParameterDefinition -> [Lude.Text]) (\s a -> s {referencedByResources = a} :: ParameterDefinition)
{-# DEPRECATED pdReferencedByResources "Use generic-lens or generic-optics with 'referencedByResources' instead." #-}

-- | An integer value that determines the largest number of characters that you want to allow for String types.
--
-- /Note:/ Consider using 'maxLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMaxLength :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Int)
pdMaxLength = Lens.lens (maxLength :: ParameterDefinition -> Lude.Maybe Lude.Int) (\s a -> s {maxLength = a} :: ParameterDefinition)
{-# DEPRECATED pdMaxLength "Use generic-lens or generic-optics with 'maxLength' instead." #-}

-- | A string that explains a constraint when the constraint is violated. For example, without a constraint description,
--
--  a parameter that has an allowed pattern of [A-Za-z0-9]+ displays the following error message when the user
--  specifies an invalid value:
--
--  Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+
--
-- By adding a constraint description, such as "must contain only uppercase and lowercase letters and numbers," you can display
--  the following customized error message:
--
--  Malformed input-Parameter MyParameter must contain only uppercase and lowercase letters and numbers.
--
--
-- /Note:/ Consider using 'constraintDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdConstraintDescription :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Text)
pdConstraintDescription = Lens.lens (constraintDescription :: ParameterDefinition -> Lude.Maybe Lude.Text) (\s a -> s {constraintDescription = a} :: ParameterDefinition)
{-# DEPRECATED pdConstraintDescription "Use generic-lens or generic-optics with 'constraintDescription' instead." #-}

-- | An integer value that determines the smallest number of characters that you want to allow for String types.
--
-- /Note:/ Consider using 'minLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMinLength :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Int)
pdMinLength = Lens.lens (minLength :: ParameterDefinition -> Lude.Maybe Lude.Int) (\s a -> s {minLength = a} :: ParameterDefinition)
{-# DEPRECATED pdMinLength "Use generic-lens or generic-optics with 'minLength' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' ParameterDefinition Lude.Text
pdName = Lens.lens (name :: ParameterDefinition -> Lude.Text) (\s a -> s {name = a} :: ParameterDefinition)
{-# DEPRECATED pdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A value of the appropriate type for the template to use if no value is specified when a stack is created.
--
--  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDefaultValue :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Text)
pdDefaultValue = Lens.lens (defaultValue :: ParameterDefinition -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: ParameterDefinition)
{-# DEPRECATED pdDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | A regular expression that represents the patterns to allow for String types.
--
-- /Note:/ Consider using 'allowedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdAllowedPattern :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Text)
pdAllowedPattern = Lens.lens (allowedPattern :: ParameterDefinition -> Lude.Maybe Lude.Text) (\s a -> s {allowedPattern = a} :: ParameterDefinition)
{-# DEPRECATED pdAllowedPattern "Use generic-lens or generic-optics with 'allowedPattern' instead." #-}

-- | Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the
--
--  value to true, the parameter value is masked with asterisks (*****).
--
-- /Note:/ Consider using 'noEcho' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdNoEcho :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Bool)
pdNoEcho = Lens.lens (noEcho :: ParameterDefinition -> Lude.Maybe Lude.Bool) (\s a -> s {noEcho = a} :: ParameterDefinition)
{-# DEPRECATED pdNoEcho "Use generic-lens or generic-optics with 'noEcho' instead." #-}

-- | The type of the parameter.
--
-- Valid values: String | Number | List<Number> | CommaDelimitedList
--
--
--  String: A literal string.
-- For example, users can specify "MyUserName".
--
--  Number: An integer or float. AWS CloudFormation validates the parameter value as a number. However, when you use the
--  parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a string.
-- For example, users might specify "8888".
--
--  List<Number>: An array of integers or floats that are separated by commas. AWS CloudFormation validates the parameter value as numbers. However, when
--  you use the parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a list of strings.
-- For example, users might specify "80,20", and then Ref results in ["80","20"].
--
--  CommaDelimitedList: An array of literal strings that are separated by commas. The total number of strings should be one more than the total number of commas.
--  Also, each member string is space-trimmed.
-- For example, users might specify "test,dev,prod", and then Ref results in ["test","dev","prod"].
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdType :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Text)
pdType = Lens.lens (type' :: ParameterDefinition -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ParameterDefinition)
{-# DEPRECATED pdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An array containing the list of values allowed for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdAllowedValues :: Lens.Lens' ParameterDefinition (Lude.Maybe [Lude.Text])
pdAllowedValues = Lens.lens (allowedValues :: ParameterDefinition -> Lude.Maybe [Lude.Text]) (\s a -> s {allowedValues = a} :: ParameterDefinition)
{-# DEPRECATED pdAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | A string of up to 4,000 characters that describes the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Text)
pdDescription = Lens.lens (description :: ParameterDefinition -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ParameterDefinition)
{-# DEPRECATED pdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A numeric value that determines the smallest numeric value that you want to allow for Number types.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMinValue :: Lens.Lens' ParameterDefinition (Lude.Maybe Lude.Int)
pdMinValue = Lens.lens (minValue :: ParameterDefinition -> Lude.Maybe Lude.Int) (\s a -> s {minValue = a} :: ParameterDefinition)
{-# DEPRECATED pdMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

instance Lude.FromJSON ParameterDefinition where
  parseJSON =
    Lude.withObject
      "ParameterDefinition"
      ( \x ->
          ParameterDefinition'
            Lude.<$> (x Lude..:? "maxValue")
            Lude.<*> (x Lude..:? "referencedByResources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "maxLength")
            Lude.<*> (x Lude..:? "constraintDescription")
            Lude.<*> (x Lude..:? "minLength")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "defaultValue")
            Lude.<*> (x Lude..:? "allowedPattern")
            Lude.<*> (x Lude..:? "noEcho")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "allowedValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "minValue")
      )
