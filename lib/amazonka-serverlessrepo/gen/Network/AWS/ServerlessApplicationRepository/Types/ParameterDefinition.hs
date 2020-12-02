{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameters supported by the application.
--
--
--
-- /See:/ 'parameterDefinition' smart constructor.
data ParameterDefinition = ParameterDefinition'
  { _pdMaxValue ::
      !(Maybe Int),
    _pdMaxLength :: !(Maybe Int),
    _pdConstraintDescription :: !(Maybe Text),
    _pdMinLength :: !(Maybe Int),
    _pdDefaultValue :: !(Maybe Text),
    _pdAllowedPattern :: !(Maybe Text),
    _pdNoEcho :: !(Maybe Bool),
    _pdType :: !(Maybe Text),
    _pdAllowedValues :: !(Maybe [Text]),
    _pdDescription :: !(Maybe Text),
    _pdMinValue :: !(Maybe Int),
    _pdReferencedByResources :: ![Text],
    _pdName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdMaxValue' - A numeric value that determines the largest numeric value that you want to allow for Number types.
--
-- * 'pdMaxLength' - An integer value that determines the largest number of characters that you want to allow for String types.
--
-- * 'pdConstraintDescription' - A string that explains a constraint when the constraint is violated. For example, without a constraint description,  a parameter that has an allowed pattern of [A-Za-z0-9]+ displays the following error message when the user  specifies an invalid value:  Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+   By adding a constraint description, such as "must contain only uppercase and lowercase letters and numbers," you can display  the following customized error message:  Malformed input-Parameter MyParameter must contain only uppercase and lowercase letters and numbers.
--
-- * 'pdMinLength' - An integer value that determines the smallest number of characters that you want to allow for String types.
--
-- * 'pdDefaultValue' - A value of the appropriate type for the template to use if no value is specified when a stack is created.  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
--
-- * 'pdAllowedPattern' - A regular expression that represents the patterns to allow for String types.
--
-- * 'pdNoEcho' - Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the  value to true, the parameter value is masked with asterisks (*****).
--
-- * 'pdType' - The type of the parameter. Valid values: String | Number | List<Number> | CommaDelimitedList    String: A literal string. For example, users can specify "MyUserName".  Number: An integer or float. AWS CloudFormation validates the parameter value as a number. However, when you use the  parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a string. For example, users might specify "8888".  List<Number>: An array of integers or floats that are separated by commas. AWS CloudFormation validates the parameter value as numbers. However, when  you use the parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a list of strings. For example, users might specify "80,20", and then Ref results in ["80","20"].  CommaDelimitedList: An array of literal strings that are separated by commas. The total number of strings should be one more than the total number of commas.  Also, each member string is space-trimmed. For example, users might specify "test,dev,prod", and then Ref results in ["test","dev","prod"].
--
-- * 'pdAllowedValues' - An array containing the list of values allowed for the parameter.
--
-- * 'pdDescription' - A string of up to 4,000 characters that describes the parameter.
--
-- * 'pdMinValue' - A numeric value that determines the smallest numeric value that you want to allow for Number types.
--
-- * 'pdReferencedByResources' - A list of AWS SAM resources that use this parameter.
--
-- * 'pdName' - The name of the parameter.
parameterDefinition ::
  -- | 'pdName'
  Text ->
  ParameterDefinition
parameterDefinition pName_ =
  ParameterDefinition'
    { _pdMaxValue = Nothing,
      _pdMaxLength = Nothing,
      _pdConstraintDescription = Nothing,
      _pdMinLength = Nothing,
      _pdDefaultValue = Nothing,
      _pdAllowedPattern = Nothing,
      _pdNoEcho = Nothing,
      _pdType = Nothing,
      _pdAllowedValues = Nothing,
      _pdDescription = Nothing,
      _pdMinValue = Nothing,
      _pdReferencedByResources = mempty,
      _pdName = pName_
    }

-- | A numeric value that determines the largest numeric value that you want to allow for Number types.
pdMaxValue :: Lens' ParameterDefinition (Maybe Int)
pdMaxValue = lens _pdMaxValue (\s a -> s {_pdMaxValue = a})

-- | An integer value that determines the largest number of characters that you want to allow for String types.
pdMaxLength :: Lens' ParameterDefinition (Maybe Int)
pdMaxLength = lens _pdMaxLength (\s a -> s {_pdMaxLength = a})

-- | A string that explains a constraint when the constraint is violated. For example, without a constraint description,  a parameter that has an allowed pattern of [A-Za-z0-9]+ displays the following error message when the user  specifies an invalid value:  Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+   By adding a constraint description, such as "must contain only uppercase and lowercase letters and numbers," you can display  the following customized error message:  Malformed input-Parameter MyParameter must contain only uppercase and lowercase letters and numbers.
pdConstraintDescription :: Lens' ParameterDefinition (Maybe Text)
pdConstraintDescription = lens _pdConstraintDescription (\s a -> s {_pdConstraintDescription = a})

-- | An integer value that determines the smallest number of characters that you want to allow for String types.
pdMinLength :: Lens' ParameterDefinition (Maybe Int)
pdMinLength = lens _pdMinLength (\s a -> s {_pdMinLength = a})

-- | A value of the appropriate type for the template to use if no value is specified when a stack is created.  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
pdDefaultValue :: Lens' ParameterDefinition (Maybe Text)
pdDefaultValue = lens _pdDefaultValue (\s a -> s {_pdDefaultValue = a})

-- | A regular expression that represents the patterns to allow for String types.
pdAllowedPattern :: Lens' ParameterDefinition (Maybe Text)
pdAllowedPattern = lens _pdAllowedPattern (\s a -> s {_pdAllowedPattern = a})

-- | Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the  value to true, the parameter value is masked with asterisks (*****).
pdNoEcho :: Lens' ParameterDefinition (Maybe Bool)
pdNoEcho = lens _pdNoEcho (\s a -> s {_pdNoEcho = a})

-- | The type of the parameter. Valid values: String | Number | List<Number> | CommaDelimitedList    String: A literal string. For example, users can specify "MyUserName".  Number: An integer or float. AWS CloudFormation validates the parameter value as a number. However, when you use the  parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a string. For example, users might specify "8888".  List<Number>: An array of integers or floats that are separated by commas. AWS CloudFormation validates the parameter value as numbers. However, when  you use the parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a list of strings. For example, users might specify "80,20", and then Ref results in ["80","20"].  CommaDelimitedList: An array of literal strings that are separated by commas. The total number of strings should be one more than the total number of commas.  Also, each member string is space-trimmed. For example, users might specify "test,dev,prod", and then Ref results in ["test","dev","prod"].
pdType :: Lens' ParameterDefinition (Maybe Text)
pdType = lens _pdType (\s a -> s {_pdType = a})

-- | An array containing the list of values allowed for the parameter.
pdAllowedValues :: Lens' ParameterDefinition [Text]
pdAllowedValues = lens _pdAllowedValues (\s a -> s {_pdAllowedValues = a}) . _Default . _Coerce

-- | A string of up to 4,000 characters that describes the parameter.
pdDescription :: Lens' ParameterDefinition (Maybe Text)
pdDescription = lens _pdDescription (\s a -> s {_pdDescription = a})

-- | A numeric value that determines the smallest numeric value that you want to allow for Number types.
pdMinValue :: Lens' ParameterDefinition (Maybe Int)
pdMinValue = lens _pdMinValue (\s a -> s {_pdMinValue = a})

-- | A list of AWS SAM resources that use this parameter.
pdReferencedByResources :: Lens' ParameterDefinition [Text]
pdReferencedByResources = lens _pdReferencedByResources (\s a -> s {_pdReferencedByResources = a}) . _Coerce

-- | The name of the parameter.
pdName :: Lens' ParameterDefinition Text
pdName = lens _pdName (\s a -> s {_pdName = a})

instance FromJSON ParameterDefinition where
  parseJSON =
    withObject
      "ParameterDefinition"
      ( \x ->
          ParameterDefinition'
            <$> (x .:? "maxValue")
            <*> (x .:? "maxLength")
            <*> (x .:? "constraintDescription")
            <*> (x .:? "minLength")
            <*> (x .:? "defaultValue")
            <*> (x .:? "allowedPattern")
            <*> (x .:? "noEcho")
            <*> (x .:? "type")
            <*> (x .:? "allowedValues" .!= mempty)
            <*> (x .:? "description")
            <*> (x .:? "minValue")
            <*> (x .:? "referencedByResources" .!= mempty)
            <*> (x .: "name")
      )

instance Hashable ParameterDefinition

instance NFData ParameterDefinition
