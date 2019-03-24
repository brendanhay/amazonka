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

-- | A nested application summary.
--
--
--
-- /See:/ 'applicationDependencySummary' smart constructor.
data ApplicationDependencySummary = ApplicationDependencySummary'
  { _adsApplicationId   :: !Text
  , _adsSemanticVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationDependencySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adsApplicationId' - The Amazon Resource Name (ARN) of the nested application.
--
-- * 'adsSemanticVersion' - The semantic version of the nested application.
applicationDependencySummary
    :: Text -- ^ 'adsApplicationId'
    -> Text -- ^ 'adsSemanticVersion'
    -> ApplicationDependencySummary
applicationDependencySummary pApplicationId_ pSemanticVersion_ =
  ApplicationDependencySummary'
    { _adsApplicationId = pApplicationId_
    , _adsSemanticVersion = pSemanticVersion_
    }


-- | The Amazon Resource Name (ARN) of the nested application.
adsApplicationId :: Lens' ApplicationDependencySummary Text
adsApplicationId = lens _adsApplicationId (\ s a -> s{_adsApplicationId = a})

-- | The semantic version of the nested application.
adsSemanticVersion :: Lens' ApplicationDependencySummary Text
adsSemanticVersion = lens _adsSemanticVersion (\ s a -> s{_adsSemanticVersion = a})

instance FromJSON ApplicationDependencySummary where
        parseJSON
          = withObject "ApplicationDependencySummary"
              (\ x ->
                 ApplicationDependencySummary' <$>
                   (x .: "applicationId") <*> (x .: "semanticVersion"))

instance Hashable ApplicationDependencySummary where

instance NFData ApplicationDependencySummary where

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
-- * 'apsPrincipals' - An array of AWS account IDs, or * to make the application public.
--
-- * 'apsActions' - For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application   Permissions> .
applicationPolicyStatement
    :: ApplicationPolicyStatement
applicationPolicyStatement =
  ApplicationPolicyStatement'
    {_apsStatementId = Nothing, _apsPrincipals = mempty, _apsActions = mempty}


-- | A unique ID for the statement.
apsStatementId :: Lens' ApplicationPolicyStatement (Maybe Text)
apsStatementId = lens _apsStatementId (\ s a -> s{_apsStatementId = a})

-- | An array of AWS account IDs, or * to make the application public.
apsPrincipals :: Lens' ApplicationPolicyStatement [Text]
apsPrincipals = lens _apsPrincipals (\ s a -> s{_apsPrincipals = a}) . _Coerce

-- | For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application   Permissions> .
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
-- * 'asCreationTime' - The date and time this resource was created.
--
-- * 'asHomePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- * 'asLabels' - Labels to improve discovery of apps in search results. Minimum length=1. Maximum length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'asSpdxLicenseId' - A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
--
-- * 'asDescription' - The description of the application. Minimum length=1. Maximum length=256
--
-- * 'asAuthor' - The name of the author publishing the app. Minimum length=1. Maximum length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'asApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'asName' - The name of the application. Minimum length=1. Maximum length=140 Pattern: "[a-zA-Z0-9\\-]+";
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


-- | The date and time this resource was created.
asCreationTime :: Lens' ApplicationSummary (Maybe Text)
asCreationTime = lens _asCreationTime (\ s a -> s{_asCreationTime = a})

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
asHomePageURL :: Lens' ApplicationSummary (Maybe Text)
asHomePageURL = lens _asHomePageURL (\ s a -> s{_asHomePageURL = a})

-- | Labels to improve discovery of apps in search results. Minimum length=1. Maximum length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
asLabels :: Lens' ApplicationSummary [Text]
asLabels = lens _asLabels (\ s a -> s{_asLabels = a}) . _Default . _Coerce

-- | A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
asSpdxLicenseId :: Lens' ApplicationSummary (Maybe Text)
asSpdxLicenseId = lens _asSpdxLicenseId (\ s a -> s{_asSpdxLicenseId = a})

-- | The description of the application. Minimum length=1. Maximum length=256
asDescription :: Lens' ApplicationSummary Text
asDescription = lens _asDescription (\ s a -> s{_asDescription = a})

-- | The name of the author publishing the app. Minimum length=1. Maximum length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
asAuthor :: Lens' ApplicationSummary Text
asAuthor = lens _asAuthor (\ s a -> s{_asAuthor = a})

-- | The application Amazon Resource Name (ARN).
asApplicationId :: Lens' ApplicationSummary Text
asApplicationId = lens _asApplicationId (\ s a -> s{_asApplicationId = a})

-- | The name of the application. Minimum length=1. Maximum length=140 Pattern: "[a-zA-Z0-9\\-]+";
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


-- | A numeric value that determines the largest numeric value that you want to allow for Number types.
pdMaxValue :: Lens' ParameterDefinition (Maybe Int)
pdMaxValue = lens _pdMaxValue (\ s a -> s{_pdMaxValue = a})

-- | An integer value that determines the largest number of characters that you want to allow for String types.
pdMaxLength :: Lens' ParameterDefinition (Maybe Int)
pdMaxLength = lens _pdMaxLength (\ s a -> s{_pdMaxLength = a})

-- | A string that explains a constraint when the constraint is violated. For example, without a constraint description,  a parameter that has an allowed pattern of [A-Za-z0-9]+ displays the following error message when the user  specifies an invalid value:  Malformed input-Parameter MyParameter must match pattern [A-Za-z0-9]+   By adding a constraint description, such as "must contain only uppercase and lowercase letters and numbers," you can display  the following customized error message:  Malformed input-Parameter MyParameter must contain only uppercase and lowercase letters and numbers.
pdConstraintDescription :: Lens' ParameterDefinition (Maybe Text)
pdConstraintDescription = lens _pdConstraintDescription (\ s a -> s{_pdConstraintDescription = a})

-- | An integer value that determines the smallest number of characters that you want to allow for String types.
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

-- | The type of the parameter. Valid values: String | Number | List<Number> | CommaDelimitedList    String: A literal string. For example, users can specify "MyUserName".  Number: An integer or float. AWS CloudFormation validates the parameter value as a number. However, when you use the  parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a string. For example, users might specify "8888".  List<Number>: An array of integers or floats that are separated by commas. AWS CloudFormation validates the parameter value as numbers. However, when  you use the parameter elsewhere in your template (for example, by using the Ref intrinsic function), the parameter value becomes a list of strings. For example, users might specify "80,20", and then Ref results in ["80","20"].  CommaDelimitedList: An array of literal strings that are separated by commas. The total number of strings should be one more than the total number of commas.  Also, each member string is space-trimmed. For example, users might specify "test,dev,prod", and then Ref results in ["test","dev","prod"].
pdType :: Lens' ParameterDefinition (Maybe Text)
pdType = lens _pdType (\ s a -> s{_pdType = a})

-- | An array containing the list of values allowed for the parameter.
pdAllowedValues :: Lens' ParameterDefinition [Text]
pdAllowedValues = lens _pdAllowedValues (\ s a -> s{_pdAllowedValues = a}) . _Default . _Coerce

-- | A string of up to 4,000 characters that describes the parameter.
pdDescription :: Lens' ParameterDefinition (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a})

-- | A numeric value that determines the smallest numeric value that you want to allow for Number types.
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

-- | This property corresponds to the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
--
--
--
-- /See:/ 'rollbackConfiguration' smart constructor.
data RollbackConfiguration = RollbackConfiguration'
  { _rcRollbackTriggers        :: !(Maybe [RollbackTrigger])
  , _rcMonitoringTimeInMinutes :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RollbackConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRollbackTriggers' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
--
-- * 'rcMonitoringTimeInMinutes' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
rollbackConfiguration
    :: RollbackConfiguration
rollbackConfiguration =
  RollbackConfiguration'
    {_rcRollbackTriggers = Nothing, _rcMonitoringTimeInMinutes = Nothing}


-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
rcRollbackTriggers :: Lens' RollbackConfiguration [RollbackTrigger]
rcRollbackTriggers = lens _rcRollbackTriggers (\ s a -> s{_rcRollbackTriggers = a}) . _Default . _Coerce

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
rcMonitoringTimeInMinutes :: Lens' RollbackConfiguration (Maybe Int)
rcMonitoringTimeInMinutes = lens _rcMonitoringTimeInMinutes (\ s a -> s{_rcMonitoringTimeInMinutes = a})

instance Hashable RollbackConfiguration where

instance NFData RollbackConfiguration where

instance ToJSON RollbackConfiguration where
        toJSON RollbackConfiguration'{..}
          = object
              (catMaybes
                 [("rollbackTriggers" .=) <$> _rcRollbackTriggers,
                  ("monitoringTimeInMinutes" .=) <$>
                    _rcMonitoringTimeInMinutes])

-- | This property corresponds to the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
--
--
-- /See:/ 'rollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { _rtType :: !Text
  , _rtARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RollbackTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtType' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
-- * 'rtARN' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
rollbackTrigger
    :: Text -- ^ 'rtType'
    -> Text -- ^ 'rtARN'
    -> RollbackTrigger
rollbackTrigger pType_ pARN_ =
  RollbackTrigger' {_rtType = pType_, _rtARN = pARN_}


-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
rtType :: Lens' RollbackTrigger Text
rtType = lens _rtType (\ s a -> s{_rtType = a})

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
rtARN :: Lens' RollbackTrigger Text
rtARN = lens _rtARN (\ s a -> s{_rtARN = a})

instance Hashable RollbackTrigger where

instance NFData RollbackTrigger where

instance ToJSON RollbackTrigger where
        toJSON RollbackTrigger'{..}
          = object
              (catMaybes
                 [Just ("type" .= _rtType), Just ("arn" .= _rtARN)])

-- | This property corresponds to the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/Tag Tag> / Data Type.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !Text
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/Tag   Tag> /   Data Type.
--
-- * 'tagKey' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/Tag Tag> / Data Type.
tag
    :: Text -- ^ 'tagValue'
    -> Text -- ^ 'tagKey'
    -> Tag
tag pValue_ pKey_ = Tag' {_tagValue = pValue_, _tagKey = pKey_}


-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/Tag   Tag> /   Data Type.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/Tag Tag> / Data Type.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("value" .= _tagValue),
                  Just ("key" .= _tagKey)])

-- | Application version details.
--
--
--
-- /See:/ 'version' smart constructor.
data Version = Version'
  { _vSourceCodeURL        :: !(Maybe Text)
  , _vSourceCodeArchiveURL :: !(Maybe Text)
  , _vTemplateURL          :: !Text
  , _vParameterDefinitions :: ![ParameterDefinition]
  , _vResourcesSupported   :: !Bool
  , _vCreationTime         :: !Text
  , _vRequiredCapabilities :: ![Capability]
  , _vApplicationId        :: !Text
  , _vSemanticVersion      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Version' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vSourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- * 'vSourceCodeArchiveURL' - A link to the S3 object that contains the ZIP archive of the source code for this version of your application. Maximum size 50 MB
--
-- * 'vTemplateURL' - A link to the packaged AWS SAM template of your application.
--
-- * 'vParameterDefinitions' - An array of parameter types supported by the application.
--
-- * 'vResourcesSupported' - Whether all of the AWS resources contained in this application are supported in the region  in which it is being retrieved.
--
-- * 'vCreationTime' - The date and time this resource was created.
--
-- * 'vRequiredCapabilities' - A list of values that you must specify before you can deploy certain applications.  Some applications might include resources that can affect permissions in your AWS  account, for example, by creating new AWS Identity and Access Management (IAM) users.  For those applications, you must explicitly acknowledge their capabilities by  specifying this parameter. The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND. The following resources require you to specify CAPABILITY_IAM or  CAPABILITY_NAMED_IAM:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .  If the application contains IAM resources, you can specify either CAPABILITY_IAM  or CAPABILITY_NAMED_IAM. If the application contains IAM resources  with custom names, you must specify CAPABILITY_NAMED_IAM. The following resources require you to specify CAPABILITY_RESOURCE_POLICY:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy> . Applications that contain one or more nested applications require you to specify  CAPABILITY_AUTO_EXPAND. If your application template contains any of the above resources, we recommend that you review  all permissions associated with the application before deploying. If you don't specify  this parameter for an application that requires capabilities, the call will fail.
--
-- * 'vApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'vSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
version
    :: Text -- ^ 'vTemplateURL'
    -> Bool -- ^ 'vResourcesSupported'
    -> Text -- ^ 'vCreationTime'
    -> Text -- ^ 'vApplicationId'
    -> Text -- ^ 'vSemanticVersion'
    -> Version
version pTemplateURL_ pResourcesSupported_ pCreationTime_ pApplicationId_ pSemanticVersion_ =
  Version'
    { _vSourceCodeURL = Nothing
    , _vSourceCodeArchiveURL = Nothing
    , _vTemplateURL = pTemplateURL_
    , _vParameterDefinitions = mempty
    , _vResourcesSupported = pResourcesSupported_
    , _vCreationTime = pCreationTime_
    , _vRequiredCapabilities = mempty
    , _vApplicationId = pApplicationId_
    , _vSemanticVersion = pSemanticVersion_
    }


-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
vSourceCodeURL :: Lens' Version (Maybe Text)
vSourceCodeURL = lens _vSourceCodeURL (\ s a -> s{_vSourceCodeURL = a})

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application. Maximum size 50 MB
vSourceCodeArchiveURL :: Lens' Version (Maybe Text)
vSourceCodeArchiveURL = lens _vSourceCodeArchiveURL (\ s a -> s{_vSourceCodeArchiveURL = a})

-- | A link to the packaged AWS SAM template of your application.
vTemplateURL :: Lens' Version Text
vTemplateURL = lens _vTemplateURL (\ s a -> s{_vTemplateURL = a})

-- | An array of parameter types supported by the application.
vParameterDefinitions :: Lens' Version [ParameterDefinition]
vParameterDefinitions = lens _vParameterDefinitions (\ s a -> s{_vParameterDefinitions = a}) . _Coerce

-- | Whether all of the AWS resources contained in this application are supported in the region  in which it is being retrieved.
vResourcesSupported :: Lens' Version Bool
vResourcesSupported = lens _vResourcesSupported (\ s a -> s{_vResourcesSupported = a})

-- | The date and time this resource was created.
vCreationTime :: Lens' Version Text
vCreationTime = lens _vCreationTime (\ s a -> s{_vCreationTime = a})

-- | A list of values that you must specify before you can deploy certain applications.  Some applications might include resources that can affect permissions in your AWS  account, for example, by creating new AWS Identity and Access Management (IAM) users.  For those applications, you must explicitly acknowledge their capabilities by  specifying this parameter. The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND. The following resources require you to specify CAPABILITY_IAM or  CAPABILITY_NAMED_IAM:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .  If the application contains IAM resources, you can specify either CAPABILITY_IAM  or CAPABILITY_NAMED_IAM. If the application contains IAM resources  with custom names, you must specify CAPABILITY_NAMED_IAM. The following resources require you to specify CAPABILITY_RESOURCE_POLICY:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy> . Applications that contain one or more nested applications require you to specify  CAPABILITY_AUTO_EXPAND. If your application template contains any of the above resources, we recommend that you review  all permissions associated with the application before deploying. If you don't specify  this parameter for an application that requires capabilities, the call will fail.
vRequiredCapabilities :: Lens' Version [Capability]
vRequiredCapabilities = lens _vRequiredCapabilities (\ s a -> s{_vRequiredCapabilities = a}) . _Coerce

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
                   (x .:? "sourceCodeUrl") <*>
                     (x .:? "sourceCodeArchiveUrl")
                     <*> (x .: "templateUrl")
                     <*> (x .:? "parameterDefinitions" .!= mempty)
                     <*> (x .: "resourcesSupported")
                     <*> (x .: "creationTime")
                     <*> (x .:? "requiredCapabilities" .!= mempty)
                     <*> (x .: "applicationId")
                     <*> (x .: "semanticVersion"))

instance Hashable Version where

instance NFData Version where

-- | An application version summary.
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
-- * 'vsSourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- * 'vsCreationTime' - The date and time this resource was created.
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


-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
vsSourceCodeURL :: Lens' VersionSummary (Maybe Text)
vsSourceCodeURL = lens _vsSourceCodeURL (\ s a -> s{_vsSourceCodeURL = a})

-- | The date and time this resource was created.
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
