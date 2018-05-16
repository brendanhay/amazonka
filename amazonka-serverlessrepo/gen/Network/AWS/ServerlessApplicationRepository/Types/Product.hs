{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServerlessApplicationRepository.Types.Sum

-- | Policy statement applied to the application.
--
--
--
-- /See:/ 'applicationPolicyStatement' smart constructor.
data ApplicationPolicyStatement = ApplicationPolicyStatement'
  { _apsStatementId :: !(Maybe Text)
  , _apsPrincipals  :: ![Text]
  , _apsActions     :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationPolicyStatement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsStatementId' - A unique ID for the statement.
--
-- * 'apsPrincipals' - An AWS account ID, or * to make the application public.
--
-- * 'apsActions' - A list of supported actions:  GetApplication    CreateCloudFormationChangeSet    ListApplicationVersions    SearchApplications    Deploy (Note: This action enables all other actions above.)
applicationPolicyStatement
    :: ApplicationPolicyStatement
applicationPolicyStatement =
  ApplicationPolicyStatement'
    {_apsStatementId = Nothing, _apsPrincipals = mempty, _apsActions = mempty}


-- | A unique ID for the statement.
apsStatementId :: Lens' ApplicationPolicyStatement (Maybe Text)
apsStatementId = lens _apsStatementId (\ s a -> s{_apsStatementId = a})

-- | An AWS account ID, or * to make the application public.
apsPrincipals :: Lens' ApplicationPolicyStatement [Text]
apsPrincipals = lens _apsPrincipals (\ s a -> s{_apsPrincipals = a}) . _Coerce

-- | A list of supported actions:  GetApplication    CreateCloudFormationChangeSet    ListApplicationVersions    SearchApplications    Deploy (Note: This action enables all other actions above.)
apsActions :: Lens' ApplicationPolicyStatement [Text]
apsActions = lens _apsActions (\ s a -> s{_apsActions = a}) . _Coerce

instance FromJSON ApplicationPolicyStatement where
        parseJSON
          = withObject "ApplicationPolicyStatement"
              (\ x ->
                 ApplicationPolicyStatement' <$>
                   (x .:? "statementId") <*>
                     (x .:? "principals" .!= mempty)
                     <*> (x .:? "actions" .!= mempty))

instance Hashable ApplicationPolicyStatement where

instance NFData ApplicationPolicyStatement where

instance ToJSON ApplicationPolicyStatement where
        toJSON ApplicationPolicyStatement'{..}
          = object
              (catMaybes
                 [("statementId" .=) <$> _apsStatementId,
                  Just ("principals" .= _apsPrincipals),
                  Just ("actions" .= _apsActions)])

-- | Summary of details about the application.
--
--
--
-- /See:/ 'applicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { _asCreationTime  :: !(Maybe Text)
  , _asHomePageURL   :: !(Maybe Text)
  , _asLabels        :: !(Maybe [Text])
  , _asSpdxLicenseId :: !(Maybe Text)
  , _asDescription   :: !Text
  , _asAuthor        :: !Text
  , _asApplicationId :: !Text
  , _asName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asCreationTime' - The date/time this resource was created.
--
-- * 'asHomePageURL' - A URL with more information about the application, for example  the location of your GitHub repository for the application.
--
-- * 'asLabels' - Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'asSpdxLicenseId' - A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
--
-- * 'asDescription' - The description of the application. Min Length=1. Max Length=256
--
-- * 'asAuthor' - The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'asApplicationId' - The application ARN.
--
-- * 'asName' - The name of the application. Min Length=1. Max Length=140 Pattern: "[a-zA-Z0-9\\-]+";
applicationSummary
    :: Text -- ^ 'asDescription'
    -> Text -- ^ 'asAuthor'
    -> Text -- ^ 'asApplicationId'
    -> Text -- ^ 'asName'
    -> ApplicationSummary
applicationSummary pDescription_ pAuthor_ pApplicationId_ pName_ =
  ApplicationSummary'
    { _asCreationTime = Nothing
    , _asHomePageURL = Nothing
    , _asLabels = Nothing
    , _asSpdxLicenseId = Nothing
    , _asDescription = pDescription_
    , _asAuthor = pAuthor_
    , _asApplicationId = pApplicationId_
    , _asName = pName_
    }


-- | The date/time this resource was created.
asCreationTime :: Lens' ApplicationSummary (Maybe Text)
asCreationTime = lens _asCreationTime (\ s a -> s{_asCreationTime = a})

-- | A URL with more information about the application, for example  the location of your GitHub repository for the application.
asHomePageURL :: Lens' ApplicationSummary (Maybe Text)
asHomePageURL = lens _asHomePageURL (\ s a -> s{_asHomePageURL = a})

-- | Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
asLabels :: Lens' ApplicationSummary [Text]
asLabels = lens _asLabels (\ s a -> s{_asLabels = a}) . _Default . _Coerce

-- | A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
asSpdxLicenseId :: Lens' ApplicationSummary (Maybe Text)
asSpdxLicenseId = lens _asSpdxLicenseId (\ s a -> s{_asSpdxLicenseId = a})

-- | The description of the application. Min Length=1. Max Length=256
asDescription :: Lens' ApplicationSummary Text
asDescription = lens _asDescription (\ s a -> s{_asDescription = a})

-- | The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
asAuthor :: Lens' ApplicationSummary Text
asAuthor = lens _asAuthor (\ s a -> s{_asAuthor = a})

-- | The application ARN.
asApplicationId :: Lens' ApplicationSummary Text
asApplicationId = lens _asApplicationId (\ s a -> s{_asApplicationId = a})

-- | The name of the application. Min Length=1. Max Length=140 Pattern: "[a-zA-Z0-9\\-]+";
asName :: Lens' ApplicationSummary Text
asName = lens _asName (\ s a -> s{_asName = a})

instance FromJSON ApplicationSummary where
        parseJSON
          = withObject "ApplicationSummary"
              (\ x ->
                 ApplicationSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "homePageUrl") <*>
                     (x .:? "labels" .!= mempty)
                     <*> (x .:? "spdxLicenseId")
                     <*> (x .: "description")
                     <*> (x .: "author")
                     <*> (x .: "applicationId")
                     <*> (x .: "name"))

instance Hashable ApplicationSummary where

instance NFData ApplicationSummary where

-- | Parameters supported by the application.
--
--
--
-- /See:/ 'parameterDefinition' smart constructor.
data ParameterDefinition = ParameterDefinition'
  { _pdMaxValue              :: !(Maybe Int)
  , _pdMaxLength             :: !(Maybe Int)
  , _pdConstraintDescription :: !(Maybe Text)
  , _pdMinLength             :: !(Maybe Int)
  , _pdDefaultValue          :: !(Maybe Text)
  , _pdAllowedPattern        :: !(Maybe Text)
  , _pdNoEcho                :: !(Maybe Bool)
  , _pdType                  :: !(Maybe Text)
  , _pdAllowedValues         :: !(Maybe [Text])
  , _pdDescription           :: !(Maybe Text)
  , _pdMinValue              :: !(Maybe Int)
  , _pdReferencedByResources :: ![Text]
  , _pdName                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdMaxValue' - A numeric value that determines the largest numeric value you want to allow for Number types.
--
-- * 'pdMaxLength' - An integer value that determines the largest number of characters you want to allow for String types.
--
-- * 'pdConstraintDescription' - A string that explains a constraint when the constraint is violated. For example, without a constraint description,  a parameter that has an allowed pattern of [A-Za-z0-9]+ displays the following error message when the user  specifies an invalid value:  Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+   By adding a constraint description, such as "must contain only uppercase and lowercase letters, and numbers," you can display  the following customized error message:  Malformed input-Parameter MyParameter must contain only uppercase and lowercase letters and numbers.
--
-- * 'pdMinLength' - An integer value that determines the smallest number of characters you want to allow for String types.
--
-- * 'pdDefaultValue' - A value of the appropriate type for the template to use if no value is specified when a stack is created.  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
--
-- * 'pdAllowedPattern' - A regular expression that represents the patterns to allow for String types.
--
-- * 'pdNoEcho' - Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the  value to true, the parameter value is masked with asterisks (*****).
--
-- * 'pdType' - The type of the parameter. Valid values: String | Number | List<Number> | CommaDelimitedList    String: A literal string. For example, users could specify "MyUserName".  Number: An integer or float. AWS CloudFormation validates the parameter value as a number; however, when you use the  parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a string. For example, users could specify "8888".  List<Number>: An array of integers or floats that are separated by commas. AWS CloudFormation validates the parameter value as numbers; however, when  you use the parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a list of strings. For example, users could specify "80,20", and a Ref results in ["80","20"].  CommaDelimitedList: An array of literal strings that are separated by commas. The total number of strings should be one more than the total number of commas.  Also, each member string is space-trimmed. For example, users could specify "test,dev,prod", and a Ref results in ["test","dev","prod"].
--
-- * 'pdAllowedValues' - Array containing the list of values allowed for the parameter.
--
-- * 'pdDescription' - A string of up to 4,000 characters that describes the parameter.
--
-- * 'pdMinValue' - A numeric value that determines the smallest numeric value you want to allow for Number types.
--
-- * 'pdReferencedByResources' - A list of AWS SAM resources that use this parameter.
--
-- * 'pdName' - The name of the parameter.
parameterDefinition
    :: Text -- ^ 'pdName'
    -> ParameterDefinition
parameterDefinition pName_ =
  ParameterDefinition'
    { _pdMaxValue = Nothing
    , _pdMaxLength = Nothing
    , _pdConstraintDescription = Nothing
    , _pdMinLength = Nothing
    , _pdDefaultValue = Nothing
    , _pdAllowedPattern = Nothing
    , _pdNoEcho = Nothing
    , _pdType = Nothing
    , _pdAllowedValues = Nothing
    , _pdDescription = Nothing
    , _pdMinValue = Nothing
    , _pdReferencedByResources = mempty
    , _pdName = pName_
    }


-- | A numeric value that determines the largest numeric value you want to allow for Number types.
pdMaxValue :: Lens' ParameterDefinition (Maybe Int)
pdMaxValue = lens _pdMaxValue (\ s a -> s{_pdMaxValue = a})

-- | An integer value that determines the largest number of characters you want to allow for String types.
pdMaxLength :: Lens' ParameterDefinition (Maybe Int)
pdMaxLength = lens _pdMaxLength (\ s a -> s{_pdMaxLength = a})

-- | A string that explains a constraint when the constraint is violated. For example, without a constraint description,  a parameter that has an allowed pattern of [A-Za-z0-9]+ displays the following error message when the user  specifies an invalid value:  Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+   By adding a constraint description, such as "must contain only uppercase and lowercase letters, and numbers," you can display  the following customized error message:  Malformed input-Parameter MyParameter must contain only uppercase and lowercase letters and numbers.
pdConstraintDescription :: Lens' ParameterDefinition (Maybe Text)
pdConstraintDescription = lens _pdConstraintDescription (\ s a -> s{_pdConstraintDescription = a})

-- | An integer value that determines the smallest number of characters you want to allow for String types.
pdMinLength :: Lens' ParameterDefinition (Maybe Int)
pdMinLength = lens _pdMinLength (\ s a -> s{_pdMinLength = a})

-- | A value of the appropriate type for the template to use if no value is specified when a stack is created.  If you define constraints for the parameter, you must specify a value that adheres to those constraints.
pdDefaultValue :: Lens' ParameterDefinition (Maybe Text)
pdDefaultValue = lens _pdDefaultValue (\ s a -> s{_pdDefaultValue = a})

-- | A regular expression that represents the patterns to allow for String types.
pdAllowedPattern :: Lens' ParameterDefinition (Maybe Text)
pdAllowedPattern = lens _pdAllowedPattern (\ s a -> s{_pdAllowedPattern = a})

-- | Whether to mask the parameter value whenever anyone makes a call that describes the stack. If you set the  value to true, the parameter value is masked with asterisks (*****).
pdNoEcho :: Lens' ParameterDefinition (Maybe Bool)
pdNoEcho = lens _pdNoEcho (\ s a -> s{_pdNoEcho = a})

-- | The type of the parameter. Valid values: String | Number | List<Number> | CommaDelimitedList    String: A literal string. For example, users could specify "MyUserName".  Number: An integer or float. AWS CloudFormation validates the parameter value as a number; however, when you use the  parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a string. For example, users could specify "8888".  List<Number>: An array of integers or floats that are separated by commas. AWS CloudFormation validates the parameter value as numbers; however, when  you use the parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a list of strings. For example, users could specify "80,20", and a Ref results in ["80","20"].  CommaDelimitedList: An array of literal strings that are separated by commas. The total number of strings should be one more than the total number of commas.  Also, each member string is space-trimmed. For example, users could specify "test,dev,prod", and a Ref results in ["test","dev","prod"].
pdType :: Lens' ParameterDefinition (Maybe Text)
pdType = lens _pdType (\ s a -> s{_pdType = a})

-- | Array containing the list of values allowed for the parameter.
pdAllowedValues :: Lens' ParameterDefinition [Text]
pdAllowedValues = lens _pdAllowedValues (\ s a -> s{_pdAllowedValues = a}) . _Default . _Coerce

-- | A string of up to 4,000 characters that describes the parameter.
pdDescription :: Lens' ParameterDefinition (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a})

-- | A numeric value that determines the smallest numeric value you want to allow for Number types.
pdMinValue :: Lens' ParameterDefinition (Maybe Int)
pdMinValue = lens _pdMinValue (\ s a -> s{_pdMinValue = a})

-- | A list of AWS SAM resources that use this parameter.
pdReferencedByResources :: Lens' ParameterDefinition [Text]
pdReferencedByResources = lens _pdReferencedByResources (\ s a -> s{_pdReferencedByResources = a}) . _Coerce

-- | The name of the parameter.
pdName :: Lens' ParameterDefinition Text
pdName = lens _pdName (\ s a -> s{_pdName = a})

instance FromJSON ParameterDefinition where
        parseJSON
          = withObject "ParameterDefinition"
              (\ x ->
                 ParameterDefinition' <$>
                   (x .:? "maxValue") <*> (x .:? "maxLength") <*>
                     (x .:? "constraintDescription")
                     <*> (x .:? "minLength")
                     <*> (x .:? "defaultValue")
                     <*> (x .:? "allowedPattern")
                     <*> (x .:? "noEcho")
                     <*> (x .:? "type")
                     <*> (x .:? "allowedValues" .!= mempty)
                     <*> (x .:? "description")
                     <*> (x .:? "minValue")
                     <*> (x .:? "referencedByResources" .!= mempty)
                     <*> (x .: "name"))

instance Hashable ParameterDefinition where

instance NFData ParameterDefinition where

-- | Parameter value of the application.
--
--
--
-- /See:/ 'parameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { _pvValue :: !Text
  , _pvName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvValue' - The input value associated with the parameter.
--
-- * 'pvName' - The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation  uses the default value that is specified in your template.
parameterValue
    :: Text -- ^ 'pvValue'
    -> Text -- ^ 'pvName'
    -> ParameterValue
parameterValue pValue_ pName_ =
  ParameterValue' {_pvValue = pValue_, _pvName = pName_}


-- | The input value associated with the parameter.
pvValue :: Lens' ParameterValue Text
pvValue = lens _pvValue (\ s a -> s{_pvValue = a})

-- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation  uses the default value that is specified in your template.
pvName :: Lens' ParameterValue Text
pvName = lens _pvName (\ s a -> s{_pvName = a})

instance Hashable ParameterValue where

instance NFData ParameterValue where

instance ToJSON ParameterValue where
        toJSON ParameterValue'{..}
          = object
              (catMaybes
                 [Just ("value" .= _pvValue),
                  Just ("name" .= _pvName)])

-- | Application version details.
--
--
--
-- /See:/ 'version' smart constructor.
data Version = Version'
  { _vSourceCodeURL        :: !(Maybe Text)
  , _vTemplateURL          :: !Text
  , _vParameterDefinitions :: ![ParameterDefinition]
  , _vCreationTime         :: !Text
  , _vApplicationId        :: !Text
  , _vSemanticVersion      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Version' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vSourceCodeURL' - A link to a public repository for the source code of your application.
--
-- * 'vTemplateURL' - A link to the packaged AWS SAM template of your application.
--
-- * 'vParameterDefinitions' - Array of parameter types supported by the application.
--
-- * 'vCreationTime' - The date/time this resource was created.
--
-- * 'vApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'vSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
version
    :: Text -- ^ 'vTemplateURL'
    -> Text -- ^ 'vCreationTime'
    -> Text -- ^ 'vApplicationId'
    -> Text -- ^ 'vSemanticVersion'
    -> Version
version pTemplateURL_ pCreationTime_ pApplicationId_ pSemanticVersion_ =
  Version'
    { _vSourceCodeURL = Nothing
    , _vTemplateURL = pTemplateURL_
    , _vParameterDefinitions = mempty
    , _vCreationTime = pCreationTime_
    , _vApplicationId = pApplicationId_
    , _vSemanticVersion = pSemanticVersion_
    }


-- | A link to a public repository for the source code of your application.
vSourceCodeURL :: Lens' Version (Maybe Text)
vSourceCodeURL = lens _vSourceCodeURL (\ s a -> s{_vSourceCodeURL = a})

-- | A link to the packaged AWS SAM template of your application.
vTemplateURL :: Lens' Version Text
vTemplateURL = lens _vTemplateURL (\ s a -> s{_vTemplateURL = a})

-- | Array of parameter types supported by the application.
vParameterDefinitions :: Lens' Version [ParameterDefinition]
vParameterDefinitions = lens _vParameterDefinitions (\ s a -> s{_vParameterDefinitions = a}) . _Coerce

-- | The date/time this resource was created.
vCreationTime :: Lens' Version Text
vCreationTime = lens _vCreationTime (\ s a -> s{_vCreationTime = a})

-- | The application Amazon Resource Name (ARN).
vApplicationId :: Lens' Version Text
vApplicationId = lens _vApplicationId (\ s a -> s{_vApplicationId = a})

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
vSemanticVersion :: Lens' Version Text
vSemanticVersion = lens _vSemanticVersion (\ s a -> s{_vSemanticVersion = a})

instance FromJSON Version where
        parseJSON
          = withObject "Version"
              (\ x ->
                 Version' <$>
                   (x .:? "sourceCodeUrl") <*> (x .: "templateUrl") <*>
                     (x .:? "parameterDefinitions" .!= mempty)
                     <*> (x .: "creationTime")
                     <*> (x .: "applicationId")
                     <*> (x .: "semanticVersion"))

instance Hashable Version where

instance NFData Version where

-- | Application version summary.
--
--
--
-- /See:/ 'versionSummary' smart constructor.
data VersionSummary = VersionSummary'
  { _vsSourceCodeURL   :: !(Maybe Text)
  , _vsCreationTime    :: !Text
  , _vsApplicationId   :: !Text
  , _vsSemanticVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VersionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsSourceCodeURL' - A link to a public repository for the source code of your application.
--
-- * 'vsCreationTime' - The date/time this resource was created.
--
-- * 'vsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'vsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
versionSummary
    :: Text -- ^ 'vsCreationTime'
    -> Text -- ^ 'vsApplicationId'
    -> Text -- ^ 'vsSemanticVersion'
    -> VersionSummary
versionSummary pCreationTime_ pApplicationId_ pSemanticVersion_ =
  VersionSummary'
    { _vsSourceCodeURL = Nothing
    , _vsCreationTime = pCreationTime_
    , _vsApplicationId = pApplicationId_
    , _vsSemanticVersion = pSemanticVersion_
    }


-- | A link to a public repository for the source code of your application.
vsSourceCodeURL :: Lens' VersionSummary (Maybe Text)
vsSourceCodeURL = lens _vsSourceCodeURL (\ s a -> s{_vsSourceCodeURL = a})

-- | The date/time this resource was created.
vsCreationTime :: Lens' VersionSummary Text
vsCreationTime = lens _vsCreationTime (\ s a -> s{_vsCreationTime = a})

-- | The application Amazon Resource Name (ARN).
vsApplicationId :: Lens' VersionSummary Text
vsApplicationId = lens _vsApplicationId (\ s a -> s{_vsApplicationId = a})

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
vsSemanticVersion :: Lens' VersionSummary Text
vsSemanticVersion = lens _vsSemanticVersion (\ s a -> s{_vsSemanticVersion = a})

instance FromJSON VersionSummary where
        parseJSON
          = withObject "VersionSummary"
              (\ x ->
                 VersionSummary' <$>
                   (x .:? "sourceCodeUrl") <*> (x .: "creationTime") <*>
                     (x .: "applicationId")
                     <*> (x .: "semanticVersion"))

instance Hashable VersionSummary where

instance NFData VersionSummary where
