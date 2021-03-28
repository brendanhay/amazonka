{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
  ( ParameterDefinition (..)
  -- * Smart constructor
  , mkParameterDefinition
  -- * Lenses
  , pdReferencedByResources
  , pdName
  , pdAllowedPattern
  , pdAllowedValues
  , pdConstraintDescription
  , pdDefaultValue
  , pdDescription
  , pdMaxLength
  , pdMaxValue
  , pdMinLength
  , pdMinValue
  , pdNoEcho
  , pdType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameters supported by the application.
--
-- /See:/ 'mkParameterDefinition' smart constructor.
data ParameterDefinition = ParameterDefinition'
  { referencedByResources :: [Core.Text]
    -- ^ A list of AWS SAM resources that use this parameter.
  , name :: Core.Text
    -- ^ The name of the parameter.
  , allowedPattern :: Core.Maybe Core.Text
    -- ^ A regular expression that represents the patterns to allow for String types.
  , allowedValues :: Core.Maybe [Core.Text]
    -- ^ An array containing the list of values allowed for the parameter.
  , constraintDescription :: Core.Maybe Core.Text
    -- ^ A string that explains a constraint when the constraint is violated. For example, without a constraint description,
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
  , defaultValue :: Core.Maybe Core.Text
    -- ^ A value of the appropriate type for the template to use if no value is specified when a stack is created.
--
--  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
  , description :: Core.Maybe Core.Text
    -- ^ A string of up to 4,000 characters that describes the parameter.
  , maxLength :: Core.Maybe Core.Int
    -- ^ An integer value that determines the largest number of characters that you want to allow for String types.
  , maxValue :: Core.Maybe Core.Int
    -- ^ A numeric value that determines the largest numeric value that you want to allow for Number types.
  , minLength :: Core.Maybe Core.Int
    -- ^ An integer value that determines the smallest number of characters that you want to allow for String types.
  , minValue :: Core.Maybe Core.Int
    -- ^ A numeric value that determines the smallest numeric value that you want to allow for Number types.
  , noEcho :: Core.Maybe Core.Bool
    -- ^ Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the
--
--  value to true, the parameter value is masked with asterisks (*****).
  , type' :: Core.Maybe Core.Text
    -- ^ The type of the parameter.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterDefinition' value with any optional fields omitted.
mkParameterDefinition
    :: Core.Text -- ^ 'name'
    -> ParameterDefinition
mkParameterDefinition name
  = ParameterDefinition'{referencedByResources = Core.mempty, name,
                         allowedPattern = Core.Nothing, allowedValues = Core.Nothing,
                         constraintDescription = Core.Nothing, defaultValue = Core.Nothing,
                         description = Core.Nothing, maxLength = Core.Nothing,
                         maxValue = Core.Nothing, minLength = Core.Nothing,
                         minValue = Core.Nothing, noEcho = Core.Nothing,
                         type' = Core.Nothing}

-- | A list of AWS SAM resources that use this parameter.
--
-- /Note:/ Consider using 'referencedByResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdReferencedByResources :: Lens.Lens' ParameterDefinition [Core.Text]
pdReferencedByResources = Lens.field @"referencedByResources"
{-# INLINEABLE pdReferencedByResources #-}
{-# DEPRECATED referencedByResources "Use generic-lens or generic-optics with 'referencedByResources' instead"  #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' ParameterDefinition Core.Text
pdName = Lens.field @"name"
{-# INLINEABLE pdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A regular expression that represents the patterns to allow for String types.
--
-- /Note:/ Consider using 'allowedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdAllowedPattern :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
pdAllowedPattern = Lens.field @"allowedPattern"
{-# INLINEABLE pdAllowedPattern #-}
{-# DEPRECATED allowedPattern "Use generic-lens or generic-optics with 'allowedPattern' instead"  #-}

-- | An array containing the list of values allowed for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdAllowedValues :: Lens.Lens' ParameterDefinition (Core.Maybe [Core.Text])
pdAllowedValues = Lens.field @"allowedValues"
{-# INLINEABLE pdAllowedValues #-}
{-# DEPRECATED allowedValues "Use generic-lens or generic-optics with 'allowedValues' instead"  #-}

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
pdConstraintDescription :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
pdConstraintDescription = Lens.field @"constraintDescription"
{-# INLINEABLE pdConstraintDescription #-}
{-# DEPRECATED constraintDescription "Use generic-lens or generic-optics with 'constraintDescription' instead"  #-}

-- | A value of the appropriate type for the template to use if no value is specified when a stack is created.
--
--  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDefaultValue :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
pdDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE pdDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | A string of up to 4,000 characters that describes the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
pdDescription = Lens.field @"description"
{-# INLINEABLE pdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | An integer value that determines the largest number of characters that you want to allow for String types.
--
-- /Note:/ Consider using 'maxLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMaxLength :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Int)
pdMaxLength = Lens.field @"maxLength"
{-# INLINEABLE pdMaxLength #-}
{-# DEPRECATED maxLength "Use generic-lens or generic-optics with 'maxLength' instead"  #-}

-- | A numeric value that determines the largest numeric value that you want to allow for Number types.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMaxValue :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Int)
pdMaxValue = Lens.field @"maxValue"
{-# INLINEABLE pdMaxValue #-}
{-# DEPRECATED maxValue "Use generic-lens or generic-optics with 'maxValue' instead"  #-}

-- | An integer value that determines the smallest number of characters that you want to allow for String types.
--
-- /Note:/ Consider using 'minLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMinLength :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Int)
pdMinLength = Lens.field @"minLength"
{-# INLINEABLE pdMinLength #-}
{-# DEPRECATED minLength "Use generic-lens or generic-optics with 'minLength' instead"  #-}

-- | A numeric value that determines the smallest numeric value that you want to allow for Number types.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMinValue :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Int)
pdMinValue = Lens.field @"minValue"
{-# INLINEABLE pdMinValue #-}
{-# DEPRECATED minValue "Use generic-lens or generic-optics with 'minValue' instead"  #-}

-- | Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the
--
--  value to true, the parameter value is masked with asterisks (*****).
--
-- /Note:/ Consider using 'noEcho' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdNoEcho :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Bool)
pdNoEcho = Lens.field @"noEcho"
{-# INLINEABLE pdNoEcho #-}
{-# DEPRECATED noEcho "Use generic-lens or generic-optics with 'noEcho' instead"  #-}

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
pdType :: Lens.Lens' ParameterDefinition (Core.Maybe Core.Text)
pdType = Lens.field @"type'"
{-# INLINEABLE pdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ParameterDefinition where
        parseJSON
          = Core.withObject "ParameterDefinition" Core.$
              \ x ->
                ParameterDefinition' Core.<$>
                  (x Core..:? "referencedByResources" Core..!= Core.mempty) Core.<*>
                    x Core..: "name"
                    Core.<*> x Core..:? "allowedPattern"
                    Core.<*> x Core..:? "allowedValues"
                    Core.<*> x Core..:? "constraintDescription"
                    Core.<*> x Core..:? "defaultValue"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "maxLength"
                    Core.<*> x Core..:? "maxValue"
                    Core.<*> x Core..:? "minLength"
                    Core.<*> x Core..:? "minValue"
                    Core.<*> x Core..:? "noEcho"
                    Core.<*> x Core..:? "type"
