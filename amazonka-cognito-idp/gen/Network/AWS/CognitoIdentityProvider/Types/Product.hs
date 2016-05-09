{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.Product where

import           Network.AWS.CognitoIdentityProvider.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Specifies whether the attribute is standard or custom.
--
-- /See:/ 'attributeType' smart constructor.
data AttributeType = AttributeType'
    { _atValue :: !(Maybe (Sensitive Text))
    , _atName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttributeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atValue'
--
-- * 'atName'
attributeType
    :: Text -- ^ 'atName'
    -> AttributeType
attributeType pName_ =
    AttributeType'
    { _atValue = Nothing
    , _atName = pName_
    }

-- | The value of the attribute.
atValue :: Lens' AttributeType (Maybe Text)
atValue = lens _atValue (\ s a -> s{_atValue = a}) . mapping _Sensitive;

-- | The name of the attribute.
atName :: Lens' AttributeType Text
atName = lens _atName (\ s a -> s{_atName = a});

instance FromJSON AttributeType where
        parseJSON
          = withObject "AttributeType"
              (\ x ->
                 AttributeType' <$> (x .:? "Value") <*> (x .: "Name"))

instance Hashable AttributeType

instance NFData AttributeType

instance ToJSON AttributeType where
        toJSON AttributeType'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _atValue,
                  Just ("Name" .= _atName)])

-- | The type of code delivery details being returned from the server.
--
-- /See:/ 'codeDeliveryDetailsType' smart constructor.
data CodeDeliveryDetailsType = CodeDeliveryDetailsType'
    { _cddtDestination    :: !(Maybe Text)
    , _cddtDeliveryMedium :: !(Maybe DeliveryMediumType)
    , _cddtAttributeName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CodeDeliveryDetailsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cddtDestination'
--
-- * 'cddtDeliveryMedium'
--
-- * 'cddtAttributeName'
codeDeliveryDetailsType
    :: CodeDeliveryDetailsType
codeDeliveryDetailsType =
    CodeDeliveryDetailsType'
    { _cddtDestination = Nothing
    , _cddtDeliveryMedium = Nothing
    , _cddtAttributeName = Nothing
    }

-- | The destination for the code delivery details.
cddtDestination :: Lens' CodeDeliveryDetailsType (Maybe Text)
cddtDestination = lens _cddtDestination (\ s a -> s{_cddtDestination = a});

-- | The delivery medium (email message or phone number).
cddtDeliveryMedium :: Lens' CodeDeliveryDetailsType (Maybe DeliveryMediumType)
cddtDeliveryMedium = lens _cddtDeliveryMedium (\ s a -> s{_cddtDeliveryMedium = a});

-- | The name of the attribute in the code delivery details type.
cddtAttributeName :: Lens' CodeDeliveryDetailsType (Maybe Text)
cddtAttributeName = lens _cddtAttributeName (\ s a -> s{_cddtAttributeName = a});

instance FromJSON CodeDeliveryDetailsType where
        parseJSON
          = withObject "CodeDeliveryDetailsType"
              (\ x ->
                 CodeDeliveryDetailsType' <$>
                   (x .:? "Destination") <*> (x .:? "DeliveryMedium")
                     <*> (x .:? "AttributeName"))

instance Hashable CodeDeliveryDetailsType

instance NFData CodeDeliveryDetailsType

-- | Specifies the type of configuration for AWS Lambda triggers.
--
-- /See:/ 'lambdaConfigType' smart constructor.
data LambdaConfigType = LambdaConfigType'
    { _lctPreAuthentication  :: !(Maybe Text)
    , _lctPostAuthentication :: !(Maybe Text)
    , _lctCustomMessage      :: !(Maybe Text)
    , _lctPostConfirmation   :: !(Maybe Text)
    , _lctPreSignUp          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LambdaConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lctPreAuthentication'
--
-- * 'lctPostAuthentication'
--
-- * 'lctCustomMessage'
--
-- * 'lctPostConfirmation'
--
-- * 'lctPreSignUp'
lambdaConfigType
    :: LambdaConfigType
lambdaConfigType =
    LambdaConfigType'
    { _lctPreAuthentication = Nothing
    , _lctPostAuthentication = Nothing
    , _lctCustomMessage = Nothing
    , _lctPostConfirmation = Nothing
    , _lctPreSignUp = Nothing
    }

-- | A pre-authentication AWS Lambda trigger.
lctPreAuthentication :: Lens' LambdaConfigType (Maybe Text)
lctPreAuthentication = lens _lctPreAuthentication (\ s a -> s{_lctPreAuthentication = a});

-- | A post-authentication AWS Lambda trigger.
lctPostAuthentication :: Lens' LambdaConfigType (Maybe Text)
lctPostAuthentication = lens _lctPostAuthentication (\ s a -> s{_lctPostAuthentication = a});

-- | A custom Message AWS Lambda trigger.
lctCustomMessage :: Lens' LambdaConfigType (Maybe Text)
lctCustomMessage = lens _lctCustomMessage (\ s a -> s{_lctCustomMessage = a});

-- | A post-confirmation AWS Lambda trigger.
lctPostConfirmation :: Lens' LambdaConfigType (Maybe Text)
lctPostConfirmation = lens _lctPostConfirmation (\ s a -> s{_lctPostConfirmation = a});

-- | A pre-registration AWS Lambda trigger.
lctPreSignUp :: Lens' LambdaConfigType (Maybe Text)
lctPreSignUp = lens _lctPreSignUp (\ s a -> s{_lctPreSignUp = a});

instance FromJSON LambdaConfigType where
        parseJSON
          = withObject "LambdaConfigType"
              (\ x ->
                 LambdaConfigType' <$>
                   (x .:? "PreAuthentication") <*>
                     (x .:? "PostAuthentication")
                     <*> (x .:? "CustomMessage")
                     <*> (x .:? "PostConfirmation")
                     <*> (x .:? "PreSignUp"))

instance Hashable LambdaConfigType

instance NFData LambdaConfigType

instance ToJSON LambdaConfigType where
        toJSON LambdaConfigType'{..}
          = object
              (catMaybes
                 [("PreAuthentication" .=) <$> _lctPreAuthentication,
                  ("PostAuthentication" .=) <$> _lctPostAuthentication,
                  ("CustomMessage" .=) <$> _lctCustomMessage,
                  ("PostConfirmation" .=) <$> _lctPostConfirmation,
                  ("PreSignUp" .=) <$> _lctPreSignUp])

-- | Specifies the different settings for multi-factor authentication (MFA).
--
-- /See:/ 'mfaOptionType' smart constructor.
data MFAOptionType = MFAOptionType'
    { _motDeliveryMedium :: !(Maybe DeliveryMediumType)
    , _motAttributeName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MFAOptionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'motDeliveryMedium'
--
-- * 'motAttributeName'
mfaOptionType
    :: MFAOptionType
mfaOptionType =
    MFAOptionType'
    { _motDeliveryMedium = Nothing
    , _motAttributeName = Nothing
    }

-- | The delivery medium (email message or SMS message) to send the MFA code.
motDeliveryMedium :: Lens' MFAOptionType (Maybe DeliveryMediumType)
motDeliveryMedium = lens _motDeliveryMedium (\ s a -> s{_motDeliveryMedium = a});

-- | The attribute name of the MFA option type.
motAttributeName :: Lens' MFAOptionType (Maybe Text)
motAttributeName = lens _motAttributeName (\ s a -> s{_motAttributeName = a});

instance FromJSON MFAOptionType where
        parseJSON
          = withObject "MFAOptionType"
              (\ x ->
                 MFAOptionType' <$>
                   (x .:? "DeliveryMedium") <*> (x .:? "AttributeName"))

instance Hashable MFAOptionType

instance NFData MFAOptionType

instance ToJSON MFAOptionType where
        toJSON MFAOptionType'{..}
          = object
              (catMaybes
                 [("DeliveryMedium" .=) <$> _motDeliveryMedium,
                  ("AttributeName" .=) <$> _motAttributeName])

-- | The minimum and maximum value of an attribute that is of the number data
-- type.
--
-- /See:/ 'numberAttributeConstraintsType' smart constructor.
data NumberAttributeConstraintsType = NumberAttributeConstraintsType'
    { _nactMaxValue :: !(Maybe Text)
    , _nactMinValue :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NumberAttributeConstraintsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nactMaxValue'
--
-- * 'nactMinValue'
numberAttributeConstraintsType
    :: NumberAttributeConstraintsType
numberAttributeConstraintsType =
    NumberAttributeConstraintsType'
    { _nactMaxValue = Nothing
    , _nactMinValue = Nothing
    }

-- | The maximum value of an attribute that is of the number data type.
nactMaxValue :: Lens' NumberAttributeConstraintsType (Maybe Text)
nactMaxValue = lens _nactMaxValue (\ s a -> s{_nactMaxValue = a});

-- | The minimum value of an attribute that is of the number data type.
nactMinValue :: Lens' NumberAttributeConstraintsType (Maybe Text)
nactMinValue = lens _nactMinValue (\ s a -> s{_nactMinValue = a});

instance FromJSON NumberAttributeConstraintsType
         where
        parseJSON
          = withObject "NumberAttributeConstraintsType"
              (\ x ->
                 NumberAttributeConstraintsType' <$>
                   (x .:? "MaxValue") <*> (x .:? "MinValue"))

instance Hashable NumberAttributeConstraintsType

instance NFData NumberAttributeConstraintsType

instance ToJSON NumberAttributeConstraintsType where
        toJSON NumberAttributeConstraintsType'{..}
          = object
              (catMaybes
                 [("MaxValue" .=) <$> _nactMaxValue,
                  ("MinValue" .=) <$> _nactMinValue])

-- | The password policy type.
--
-- /See:/ 'passwordPolicyType' smart constructor.
data PasswordPolicyType = PasswordPolicyType'
    { _pptRequireNumbers   :: !(Maybe Bool)
    , _pptRequireUppercase :: !(Maybe Bool)
    , _pptRequireLowercase :: !(Maybe Bool)
    , _pptMinimumLength    :: !(Maybe Nat)
    , _pptRequireSymbols   :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PasswordPolicyType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pptRequireNumbers'
--
-- * 'pptRequireUppercase'
--
-- * 'pptRequireLowercase'
--
-- * 'pptMinimumLength'
--
-- * 'pptRequireSymbols'
passwordPolicyType
    :: PasswordPolicyType
passwordPolicyType =
    PasswordPolicyType'
    { _pptRequireNumbers = Nothing
    , _pptRequireUppercase = Nothing
    , _pptRequireLowercase = Nothing
    , _pptMinimumLength = Nothing
    , _pptRequireSymbols = Nothing
    }

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one number in their password.
pptRequireNumbers :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireNumbers = lens _pptRequireNumbers (\ s a -> s{_pptRequireNumbers = a});

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one uppercase letter in their password.
pptRequireUppercase :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireUppercase = lens _pptRequireUppercase (\ s a -> s{_pptRequireUppercase = a});

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one lowercase letter in their password.
pptRequireLowercase :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireLowercase = lens _pptRequireLowercase (\ s a -> s{_pptRequireLowercase = a});

-- | The minimum length of the password policy that you have set. Cannot be
-- less than 6.
pptMinimumLength :: Lens' PasswordPolicyType (Maybe Natural)
pptMinimumLength = lens _pptMinimumLength (\ s a -> s{_pptMinimumLength = a}) . mapping _Nat;

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one symbol in their password.
pptRequireSymbols :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireSymbols = lens _pptRequireSymbols (\ s a -> s{_pptRequireSymbols = a});

instance FromJSON PasswordPolicyType where
        parseJSON
          = withObject "PasswordPolicyType"
              (\ x ->
                 PasswordPolicyType' <$>
                   (x .:? "RequireNumbers") <*>
                     (x .:? "RequireUppercase")
                     <*> (x .:? "RequireLowercase")
                     <*> (x .:? "MinimumLength")
                     <*> (x .:? "RequireSymbols"))

instance Hashable PasswordPolicyType

instance NFData PasswordPolicyType

instance ToJSON PasswordPolicyType where
        toJSON PasswordPolicyType'{..}
          = object
              (catMaybes
                 [("RequireNumbers" .=) <$> _pptRequireNumbers,
                  ("RequireUppercase" .=) <$> _pptRequireUppercase,
                  ("RequireLowercase" .=) <$> _pptRequireLowercase,
                  ("MinimumLength" .=) <$> _pptMinimumLength,
                  ("RequireSymbols" .=) <$> _pptRequireSymbols])

-- | Contains information about the schema attribute.
--
-- /See:/ 'schemaAttributeType' smart constructor.
data SchemaAttributeType = SchemaAttributeType'
    { _satNumberAttributeConstraints :: !(Maybe NumberAttributeConstraintsType)
    , _satRequired                   :: !(Maybe Bool)
    , _satAttributeDataType          :: !(Maybe AttributeDataType)
    , _satStringAttributeConstraints :: !(Maybe StringAttributeConstraintsType)
    , _satName                       :: !(Maybe Text)
    , _satDeveloperOnlyAttribute     :: !(Maybe Bool)
    , _satMutable                    :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SchemaAttributeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'satNumberAttributeConstraints'
--
-- * 'satRequired'
--
-- * 'satAttributeDataType'
--
-- * 'satStringAttributeConstraints'
--
-- * 'satName'
--
-- * 'satDeveloperOnlyAttribute'
--
-- * 'satMutable'
schemaAttributeType
    :: SchemaAttributeType
schemaAttributeType =
    SchemaAttributeType'
    { _satNumberAttributeConstraints = Nothing
    , _satRequired = Nothing
    , _satAttributeDataType = Nothing
    , _satStringAttributeConstraints = Nothing
    , _satName = Nothing
    , _satDeveloperOnlyAttribute = Nothing
    , _satMutable = Nothing
    }

-- | Specifies the constraints for an attribute of the number type.
satNumberAttributeConstraints :: Lens' SchemaAttributeType (Maybe NumberAttributeConstraintsType)
satNumberAttributeConstraints = lens _satNumberAttributeConstraints (\ s a -> s{_satNumberAttributeConstraints = a});

-- | Specifies whether a user pool attribute is required. If the attribute is
-- required and the user does not provide a value, registration or sign-in
-- will fail.
satRequired :: Lens' SchemaAttributeType (Maybe Bool)
satRequired = lens _satRequired (\ s a -> s{_satRequired = a});

-- | The attribute data type.
satAttributeDataType :: Lens' SchemaAttributeType (Maybe AttributeDataType)
satAttributeDataType = lens _satAttributeDataType (\ s a -> s{_satAttributeDataType = a});

-- | Specifies the constraints for an attribute of the string type.
satStringAttributeConstraints :: Lens' SchemaAttributeType (Maybe StringAttributeConstraintsType)
satStringAttributeConstraints = lens _satStringAttributeConstraints (\ s a -> s{_satStringAttributeConstraints = a});

-- | A schema attribute of the name type.
satName :: Lens' SchemaAttributeType (Maybe Text)
satName = lens _satName (\ s a -> s{_satName = a});

-- | Specifies whether the attribute type is developer only.
satDeveloperOnlyAttribute :: Lens' SchemaAttributeType (Maybe Bool)
satDeveloperOnlyAttribute = lens _satDeveloperOnlyAttribute (\ s a -> s{_satDeveloperOnlyAttribute = a});

-- | Specifies whether the attribute can be changed once it has been created.
satMutable :: Lens' SchemaAttributeType (Maybe Bool)
satMutable = lens _satMutable (\ s a -> s{_satMutable = a});

instance FromJSON SchemaAttributeType where
        parseJSON
          = withObject "SchemaAttributeType"
              (\ x ->
                 SchemaAttributeType' <$>
                   (x .:? "NumberAttributeConstraints") <*>
                     (x .:? "Required")
                     <*> (x .:? "AttributeDataType")
                     <*> (x .:? "StringAttributeConstraints")
                     <*> (x .:? "Name")
                     <*> (x .:? "DeveloperOnlyAttribute")
                     <*> (x .:? "Mutable"))

instance Hashable SchemaAttributeType

instance NFData SchemaAttributeType

instance ToJSON SchemaAttributeType where
        toJSON SchemaAttributeType'{..}
          = object
              (catMaybes
                 [("NumberAttributeConstraints" .=) <$>
                    _satNumberAttributeConstraints,
                  ("Required" .=) <$> _satRequired,
                  ("AttributeDataType" .=) <$> _satAttributeDataType,
                  ("StringAttributeConstraints" .=) <$>
                    _satStringAttributeConstraints,
                  ("Name" .=) <$> _satName,
                  ("DeveloperOnlyAttribute" .=) <$>
                    _satDeveloperOnlyAttribute,
                  ("Mutable" .=) <$> _satMutable])

-- | The type of constraints associated with an attribute of the string type.
--
-- /See:/ 'stringAttributeConstraintsType' smart constructor.
data StringAttributeConstraintsType = StringAttributeConstraintsType'
    { _sactMaxLength :: !(Maybe Text)
    , _sactMinLength :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StringAttributeConstraintsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sactMaxLength'
--
-- * 'sactMinLength'
stringAttributeConstraintsType
    :: StringAttributeConstraintsType
stringAttributeConstraintsType =
    StringAttributeConstraintsType'
    { _sactMaxLength = Nothing
    , _sactMinLength = Nothing
    }

-- | The maximum length of an attribute value of the string type.
sactMaxLength :: Lens' StringAttributeConstraintsType (Maybe Text)
sactMaxLength = lens _sactMaxLength (\ s a -> s{_sactMaxLength = a});

-- | The minimum length of an attribute value of the string type.
sactMinLength :: Lens' StringAttributeConstraintsType (Maybe Text)
sactMinLength = lens _sactMinLength (\ s a -> s{_sactMinLength = a});

instance FromJSON StringAttributeConstraintsType
         where
        parseJSON
          = withObject "StringAttributeConstraintsType"
              (\ x ->
                 StringAttributeConstraintsType' <$>
                   (x .:? "MaxLength") <*> (x .:? "MinLength"))

instance Hashable StringAttributeConstraintsType

instance NFData StringAttributeConstraintsType

instance ToJSON StringAttributeConstraintsType where
        toJSON StringAttributeConstraintsType'{..}
          = object
              (catMaybes
                 [("MaxLength" .=) <$> _sactMaxLength,
                  ("MinLength" .=) <$> _sactMinLength])

-- | The description of the user poool client.
--
-- /See:/ 'userPoolClientDescription' smart constructor.
data UserPoolClientDescription = UserPoolClientDescription'
    { _upcdClientId   :: !(Maybe (Sensitive Text))
    , _upcdUserPoolId :: !(Maybe Text)
    , _upcdClientName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserPoolClientDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcdClientId'
--
-- * 'upcdUserPoolId'
--
-- * 'upcdClientName'
userPoolClientDescription
    :: UserPoolClientDescription
userPoolClientDescription =
    UserPoolClientDescription'
    { _upcdClientId = Nothing
    , _upcdUserPoolId = Nothing
    , _upcdClientName = Nothing
    }

-- | The ID of the client associated with the user pool.
upcdClientId :: Lens' UserPoolClientDescription (Maybe Text)
upcdClientId = lens _upcdClientId (\ s a -> s{_upcdClientId = a}) . mapping _Sensitive;

-- | The user pool ID for the user pool where you want to describe the user
-- pool client.
upcdUserPoolId :: Lens' UserPoolClientDescription (Maybe Text)
upcdUserPoolId = lens _upcdUserPoolId (\ s a -> s{_upcdUserPoolId = a});

-- | The client name from the user pool client description.
upcdClientName :: Lens' UserPoolClientDescription (Maybe Text)
upcdClientName = lens _upcdClientName (\ s a -> s{_upcdClientName = a});

instance FromJSON UserPoolClientDescription where
        parseJSON
          = withObject "UserPoolClientDescription"
              (\ x ->
                 UserPoolClientDescription' <$>
                   (x .:? "ClientId") <*> (x .:? "UserPoolId") <*>
                     (x .:? "ClientName"))

instance Hashable UserPoolClientDescription

instance NFData UserPoolClientDescription

-- | A user pool of the client type.
--
-- /See:/ 'userPoolClientType' smart constructor.
data UserPoolClientType = UserPoolClientType'
    { _upctClientId         :: !(Maybe (Sensitive Text))
    , _upctClientSecret     :: !(Maybe (Sensitive Text))
    , _upctLastModifiedDate :: !(Maybe POSIX)
    , _upctUserPoolId       :: !(Maybe Text)
    , _upctCreationDate     :: !(Maybe POSIX)
    , _upctClientName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserPoolClientType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upctClientId'
--
-- * 'upctClientSecret'
--
-- * 'upctLastModifiedDate'
--
-- * 'upctUserPoolId'
--
-- * 'upctCreationDate'
--
-- * 'upctClientName'
userPoolClientType
    :: UserPoolClientType
userPoolClientType =
    UserPoolClientType'
    { _upctClientId = Nothing
    , _upctClientSecret = Nothing
    , _upctLastModifiedDate = Nothing
    , _upctUserPoolId = Nothing
    , _upctCreationDate = Nothing
    , _upctClientName = Nothing
    }

-- | The ID of the client associated with the user pool.
upctClientId :: Lens' UserPoolClientType (Maybe Text)
upctClientId = lens _upctClientId (\ s a -> s{_upctClientId = a}) . mapping _Sensitive;

-- | The client secret from the user pool request of the client type.
upctClientSecret :: Lens' UserPoolClientType (Maybe Text)
upctClientSecret = lens _upctClientSecret (\ s a -> s{_upctClientSecret = a}) . mapping _Sensitive;

-- | The last modified date from the user pool request of the client type.
upctLastModifiedDate :: Lens' UserPoolClientType (Maybe UTCTime)
upctLastModifiedDate = lens _upctLastModifiedDate (\ s a -> s{_upctLastModifiedDate = a}) . mapping _Time;

-- | The user pool ID for the user pool client.
upctUserPoolId :: Lens' UserPoolClientType (Maybe Text)
upctUserPoolId = lens _upctUserPoolId (\ s a -> s{_upctUserPoolId = a});

-- | The creation date from the user pool request of the client type.
upctCreationDate :: Lens' UserPoolClientType (Maybe UTCTime)
upctCreationDate = lens _upctCreationDate (\ s a -> s{_upctCreationDate = a}) . mapping _Time;

-- | The client name from the user pool request of the client type.
upctClientName :: Lens' UserPoolClientType (Maybe Text)
upctClientName = lens _upctClientName (\ s a -> s{_upctClientName = a});

instance FromJSON UserPoolClientType where
        parseJSON
          = withObject "UserPoolClientType"
              (\ x ->
                 UserPoolClientType' <$>
                   (x .:? "ClientId") <*> (x .:? "ClientSecret") <*>
                     (x .:? "LastModifiedDate")
                     <*> (x .:? "UserPoolId")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "ClientName"))

instance Hashable UserPoolClientType

instance NFData UserPoolClientType

-- | A user pool description.
--
-- /See:/ 'userPoolDescriptionType' smart constructor.
data UserPoolDescriptionType = UserPoolDescriptionType'
    { _updtStatus           :: !(Maybe StatusType)
    , _updtLastModifiedDate :: !(Maybe POSIX)
    , _updtName             :: !(Maybe Text)
    , _updtId               :: !(Maybe Text)
    , _updtCreationDate     :: !(Maybe POSIX)
    , _updtLambdaConfig     :: !(Maybe LambdaConfigType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserPoolDescriptionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updtStatus'
--
-- * 'updtLastModifiedDate'
--
-- * 'updtName'
--
-- * 'updtId'
--
-- * 'updtCreationDate'
--
-- * 'updtLambdaConfig'
userPoolDescriptionType
    :: UserPoolDescriptionType
userPoolDescriptionType =
    UserPoolDescriptionType'
    { _updtStatus = Nothing
    , _updtLastModifiedDate = Nothing
    , _updtName = Nothing
    , _updtId = Nothing
    , _updtCreationDate = Nothing
    , _updtLambdaConfig = Nothing
    }

-- | The user pool status in a user pool description.
updtStatus :: Lens' UserPoolDescriptionType (Maybe StatusType)
updtStatus = lens _updtStatus (\ s a -> s{_updtStatus = a});

-- | The last modified date in a user pool description.
updtLastModifiedDate :: Lens' UserPoolDescriptionType (Maybe UTCTime)
updtLastModifiedDate = lens _updtLastModifiedDate (\ s a -> s{_updtLastModifiedDate = a}) . mapping _Time;

-- | The name in a user pool description.
updtName :: Lens' UserPoolDescriptionType (Maybe Text)
updtName = lens _updtName (\ s a -> s{_updtName = a});

-- | The ID in a user pool description.
updtId :: Lens' UserPoolDescriptionType (Maybe Text)
updtId = lens _updtId (\ s a -> s{_updtId = a});

-- | The creation date in a user pool description.
updtCreationDate :: Lens' UserPoolDescriptionType (Maybe UTCTime)
updtCreationDate = lens _updtCreationDate (\ s a -> s{_updtCreationDate = a}) . mapping _Time;

-- | The AWS Lambda configuration information in a user pool description.
updtLambdaConfig :: Lens' UserPoolDescriptionType (Maybe LambdaConfigType)
updtLambdaConfig = lens _updtLambdaConfig (\ s a -> s{_updtLambdaConfig = a});

instance FromJSON UserPoolDescriptionType where
        parseJSON
          = withObject "UserPoolDescriptionType"
              (\ x ->
                 UserPoolDescriptionType' <$>
                   (x .:? "Status") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LambdaConfig"))

instance Hashable UserPoolDescriptionType

instance NFData UserPoolDescriptionType

-- | The type of policy in a user pool.
--
-- /See:/ 'userPoolPolicyType' smart constructor.
newtype UserPoolPolicyType = UserPoolPolicyType'
    { _upptPasswordPolicy :: Maybe PasswordPolicyType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserPoolPolicyType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upptPasswordPolicy'
userPoolPolicyType
    :: UserPoolPolicyType
userPoolPolicyType =
    UserPoolPolicyType'
    { _upptPasswordPolicy = Nothing
    }

-- | A container with information about the user pool password policy.
upptPasswordPolicy :: Lens' UserPoolPolicyType (Maybe PasswordPolicyType)
upptPasswordPolicy = lens _upptPasswordPolicy (\ s a -> s{_upptPasswordPolicy = a});

instance FromJSON UserPoolPolicyType where
        parseJSON
          = withObject "UserPoolPolicyType"
              (\ x ->
                 UserPoolPolicyType' <$> (x .:? "PasswordPolicy"))

instance Hashable UserPoolPolicyType

instance NFData UserPoolPolicyType

instance ToJSON UserPoolPolicyType where
        toJSON UserPoolPolicyType'{..}
          = object
              (catMaybes
                 [("PasswordPolicy" .=) <$> _upptPasswordPolicy])

-- | A container with information about the user pool type.
--
-- /See:/ 'userPoolType' smart constructor.
data UserPoolType = UserPoolType'
    { _uptStatus                   :: !(Maybe StatusType)
    , _uptLastModifiedDate         :: !(Maybe POSIX)
    , _uptEstimatedNumberOfUsers   :: !(Maybe Int)
    , _uptEmailVerificationMessage :: !(Maybe Text)
    , _uptSmsAuthenticationMessage :: !(Maybe Text)
    , _uptSchemaAttributes         :: !(Maybe (List1 SchemaAttributeType))
    , _uptEmailVerificationSubject :: !(Maybe Text)
    , _uptAliasAttributes          :: !(Maybe [AliasAttributeType])
    , _uptSmsVerificationMessage   :: !(Maybe Text)
    , _uptName                     :: !(Maybe Text)
    , _uptMFAConfiguration         :: !(Maybe UserPoolMFAType)
    , _uptId                       :: !(Maybe Text)
    , _uptCreationDate             :: !(Maybe POSIX)
    , _uptLambdaConfig             :: !(Maybe LambdaConfigType)
    , _uptAutoVerifiedAttributes   :: !(Maybe [VerifiedAttributeType])
    , _uptPolicies                 :: !(Maybe UserPoolPolicyType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserPoolType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uptStatus'
--
-- * 'uptLastModifiedDate'
--
-- * 'uptEstimatedNumberOfUsers'
--
-- * 'uptEmailVerificationMessage'
--
-- * 'uptSmsAuthenticationMessage'
--
-- * 'uptSchemaAttributes'
--
-- * 'uptEmailVerificationSubject'
--
-- * 'uptAliasAttributes'
--
-- * 'uptSmsVerificationMessage'
--
-- * 'uptName'
--
-- * 'uptMFAConfiguration'
--
-- * 'uptId'
--
-- * 'uptCreationDate'
--
-- * 'uptLambdaConfig'
--
-- * 'uptAutoVerifiedAttributes'
--
-- * 'uptPolicies'
userPoolType
    :: UserPoolType
userPoolType =
    UserPoolType'
    { _uptStatus = Nothing
    , _uptLastModifiedDate = Nothing
    , _uptEstimatedNumberOfUsers = Nothing
    , _uptEmailVerificationMessage = Nothing
    , _uptSmsAuthenticationMessage = Nothing
    , _uptSchemaAttributes = Nothing
    , _uptEmailVerificationSubject = Nothing
    , _uptAliasAttributes = Nothing
    , _uptSmsVerificationMessage = Nothing
    , _uptName = Nothing
    , _uptMFAConfiguration = Nothing
    , _uptId = Nothing
    , _uptCreationDate = Nothing
    , _uptLambdaConfig = Nothing
    , _uptAutoVerifiedAttributes = Nothing
    , _uptPolicies = Nothing
    }

-- | The status of a user pool.
uptStatus :: Lens' UserPoolType (Maybe StatusType)
uptStatus = lens _uptStatus (\ s a -> s{_uptStatus = a});

-- | The last modified date of a user pool.
uptLastModifiedDate :: Lens' UserPoolType (Maybe UTCTime)
uptLastModifiedDate = lens _uptLastModifiedDate (\ s a -> s{_uptLastModifiedDate = a}) . mapping _Time;

-- | A number estimating the size of the user pool.
uptEstimatedNumberOfUsers :: Lens' UserPoolType (Maybe Int)
uptEstimatedNumberOfUsers = lens _uptEstimatedNumberOfUsers (\ s a -> s{_uptEstimatedNumberOfUsers = a});

-- | The contents of the email verification message.
uptEmailVerificationMessage :: Lens' UserPoolType (Maybe Text)
uptEmailVerificationMessage = lens _uptEmailVerificationMessage (\ s a -> s{_uptEmailVerificationMessage = a});

-- | The contents of the SMS authentication message.
uptSmsAuthenticationMessage :: Lens' UserPoolType (Maybe Text)
uptSmsAuthenticationMessage = lens _uptSmsAuthenticationMessage (\ s a -> s{_uptSmsAuthenticationMessage = a});

-- | A container with the schema attributes of a user pool.
uptSchemaAttributes :: Lens' UserPoolType (Maybe (NonEmpty SchemaAttributeType))
uptSchemaAttributes = lens _uptSchemaAttributes (\ s a -> s{_uptSchemaAttributes = a}) . mapping _List1;

-- | The subject of the email verification message.
uptEmailVerificationSubject :: Lens' UserPoolType (Maybe Text)
uptEmailVerificationSubject = lens _uptEmailVerificationSubject (\ s a -> s{_uptEmailVerificationSubject = a});

-- | Specifies the attributes that are aliased in a user pool.
uptAliasAttributes :: Lens' UserPoolType [AliasAttributeType]
uptAliasAttributes = lens _uptAliasAttributes (\ s a -> s{_uptAliasAttributes = a}) . _Default . _Coerce;

-- | The contents of the SMS verification message.
uptSmsVerificationMessage :: Lens' UserPoolType (Maybe Text)
uptSmsVerificationMessage = lens _uptSmsVerificationMessage (\ s a -> s{_uptSmsVerificationMessage = a});

-- | The name of the user pool.
uptName :: Lens' UserPoolType (Maybe Text)
uptName = lens _uptName (\ s a -> s{_uptName = a});

-- | Can be one of the following values:
--
-- -   'OFF' - MFA tokens are not required and cannot be specified during
--     user registration.
-- -   'ON' - MFA tokens are required for all user registrations. You can
--     only specify required when you are initially creating a user pool.
-- -   'OPTIONAL' - Users have the option when registering to create an MFA
--     token.
uptMFAConfiguration :: Lens' UserPoolType (Maybe UserPoolMFAType)
uptMFAConfiguration = lens _uptMFAConfiguration (\ s a -> s{_uptMFAConfiguration = a});

-- | The ID of the user pool.
uptId :: Lens' UserPoolType (Maybe Text)
uptId = lens _uptId (\ s a -> s{_uptId = a});

-- | The creation date of a user pool.
uptCreationDate :: Lens' UserPoolType (Maybe UTCTime)
uptCreationDate = lens _uptCreationDate (\ s a -> s{_uptCreationDate = a}) . mapping _Time;

-- | A container describing the AWS Lambda triggers associated with a user
-- pool.
uptLambdaConfig :: Lens' UserPoolType (Maybe LambdaConfigType)
uptLambdaConfig = lens _uptLambdaConfig (\ s a -> s{_uptLambdaConfig = a});

-- | Specifies the attributes that are auto-verified in a user pool.
uptAutoVerifiedAttributes :: Lens' UserPoolType [VerifiedAttributeType]
uptAutoVerifiedAttributes = lens _uptAutoVerifiedAttributes (\ s a -> s{_uptAutoVerifiedAttributes = a}) . _Default . _Coerce;

-- | A container describing the policies associated with a user pool.
uptPolicies :: Lens' UserPoolType (Maybe UserPoolPolicyType)
uptPolicies = lens _uptPolicies (\ s a -> s{_uptPolicies = a});

instance FromJSON UserPoolType where
        parseJSON
          = withObject "UserPoolType"
              (\ x ->
                 UserPoolType' <$>
                   (x .:? "Status") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "EstimatedNumberOfUsers")
                     <*> (x .:? "EmailVerificationMessage")
                     <*> (x .:? "SmsAuthenticationMessage")
                     <*> (x .:? "SchemaAttributes")
                     <*> (x .:? "EmailVerificationSubject")
                     <*> (x .:? "AliasAttributes" .!= mempty)
                     <*> (x .:? "SmsVerificationMessage")
                     <*> (x .:? "Name")
                     <*> (x .:? "MfaConfiguration")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LambdaConfig")
                     <*> (x .:? "AutoVerifiedAttributes" .!= mempty)
                     <*> (x .:? "Policies"))

instance Hashable UserPoolType

instance NFData UserPoolType

-- | The user type.
--
-- /See:/ 'userType' smart constructor.
data UserType = UserType'
    { _utEnabled              :: !(Maybe Bool)
    , _utUserStatus           :: !(Maybe UserStatusType)
    , _utUsername             :: !(Maybe (Sensitive Text))
    , _utUserCreateDate       :: !(Maybe POSIX)
    , _utAttributes           :: !(Maybe [AttributeType])
    , _utUserLastModifiedDate :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utEnabled'
--
-- * 'utUserStatus'
--
-- * 'utUsername'
--
-- * 'utUserCreateDate'
--
-- * 'utAttributes'
--
-- * 'utUserLastModifiedDate'
userType
    :: UserType
userType =
    UserType'
    { _utEnabled = Nothing
    , _utUserStatus = Nothing
    , _utUsername = Nothing
    , _utUserCreateDate = Nothing
    , _utAttributes = Nothing
    , _utUserLastModifiedDate = Nothing
    }

-- | Specifies whether the user is enabled.
utEnabled :: Lens' UserType (Maybe Bool)
utEnabled = lens _utEnabled (\ s a -> s{_utEnabled = a});

-- | The user status. Can be one of the following:
--
-- -   UNCONFIRMED - User has been created but not confirmed.
-- -   CONFIRMED - User has been confirmed.
-- -   ARCHIVED - User is no longer active.
-- -   COMPROMISED - User is disabled due to a potential security threat.
-- -   UNKNOWN - User status is not known.
utUserStatus :: Lens' UserType (Maybe UserStatusType)
utUserStatus = lens _utUserStatus (\ s a -> s{_utUserStatus = a});

-- | The user name of the user you wish to describe.
utUsername :: Lens' UserType (Maybe Text)
utUsername = lens _utUsername (\ s a -> s{_utUsername = a}) . mapping _Sensitive;

-- | The creation date of the user.
utUserCreateDate :: Lens' UserType (Maybe UTCTime)
utUserCreateDate = lens _utUserCreateDate (\ s a -> s{_utUserCreateDate = a}) . mapping _Time;

-- | A container with information about the user type attributes.
utAttributes :: Lens' UserType [AttributeType]
utAttributes = lens _utAttributes (\ s a -> s{_utAttributes = a}) . _Default . _Coerce;

-- | The last modified date of the user.
utUserLastModifiedDate :: Lens' UserType (Maybe UTCTime)
utUserLastModifiedDate = lens _utUserLastModifiedDate (\ s a -> s{_utUserLastModifiedDate = a}) . mapping _Time;

instance FromJSON UserType where
        parseJSON
          = withObject "UserType"
              (\ x ->
                 UserType' <$>
                   (x .:? "Enabled") <*> (x .:? "UserStatus") <*>
                     (x .:? "Username")
                     <*> (x .:? "UserCreateDate")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "UserLastModifiedDate"))

instance Hashable UserType

instance NFData UserType
