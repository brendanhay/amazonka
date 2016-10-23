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

-- | The result type of the authentication result.
--
-- /See:/ 'authenticationResultType' smart constructor.
data AuthenticationResultType = AuthenticationResultType'
    { _artAccessToken       :: !(Maybe (Sensitive Text))
    , _artRefreshToken      :: !(Maybe (Sensitive Text))
    , _artNewDeviceMetadata :: !(Maybe NewDeviceMetadataType)
    , _artExpiresIn         :: !(Maybe Int)
    , _artTokenType         :: !(Maybe Text)
    , _artIdToken           :: !(Maybe (Sensitive Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuthenticationResultType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artAccessToken'
--
-- * 'artRefreshToken'
--
-- * 'artNewDeviceMetadata'
--
-- * 'artExpiresIn'
--
-- * 'artTokenType'
--
-- * 'artIdToken'
authenticationResultType
    :: AuthenticationResultType
authenticationResultType =
    AuthenticationResultType'
    { _artAccessToken = Nothing
    , _artRefreshToken = Nothing
    , _artNewDeviceMetadata = Nothing
    , _artExpiresIn = Nothing
    , _artTokenType = Nothing
    , _artIdToken = Nothing
    }

-- | The access token of the authentication result.
artAccessToken :: Lens' AuthenticationResultType (Maybe Text)
artAccessToken = lens _artAccessToken (\ s a -> s{_artAccessToken = a}) . mapping _Sensitive;

-- | The refresh token of the authentication result.
artRefreshToken :: Lens' AuthenticationResultType (Maybe Text)
artRefreshToken = lens _artRefreshToken (\ s a -> s{_artRefreshToken = a}) . mapping _Sensitive;

-- | The new device metadata from an authentication result.
artNewDeviceMetadata :: Lens' AuthenticationResultType (Maybe NewDeviceMetadataType)
artNewDeviceMetadata = lens _artNewDeviceMetadata (\ s a -> s{_artNewDeviceMetadata = a});

-- | The expiration period of the authentication result.
artExpiresIn :: Lens' AuthenticationResultType (Maybe Int)
artExpiresIn = lens _artExpiresIn (\ s a -> s{_artExpiresIn = a});

-- | The token type of the authentication result.
artTokenType :: Lens' AuthenticationResultType (Maybe Text)
artTokenType = lens _artTokenType (\ s a -> s{_artTokenType = a});

-- | The ID token of the authentication result.
artIdToken :: Lens' AuthenticationResultType (Maybe Text)
artIdToken = lens _artIdToken (\ s a -> s{_artIdToken = a}) . mapping _Sensitive;

instance FromJSON AuthenticationResultType where
        parseJSON
          = withObject "AuthenticationResultType"
              (\ x ->
                 AuthenticationResultType' <$>
                   (x .:? "AccessToken") <*> (x .:? "RefreshToken") <*>
                     (x .:? "NewDeviceMetadata")
                     <*> (x .:? "ExpiresIn")
                     <*> (x .:? "TokenType")
                     <*> (x .:? "IdToken"))

instance Hashable AuthenticationResultType

instance NFData AuthenticationResultType

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

-- | The type of configuration for the user pool\'s device tracking.
--
-- /See:/ 'deviceConfigurationType' smart constructor.
data DeviceConfigurationType = DeviceConfigurationType'
    { _dctChallengeRequiredOnNewDevice     :: !(Maybe Bool)
    , _dctDeviceOnlyRememberedOnUserPrompt :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeviceConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dctChallengeRequiredOnNewDevice'
--
-- * 'dctDeviceOnlyRememberedOnUserPrompt'
deviceConfigurationType
    :: DeviceConfigurationType
deviceConfigurationType =
    DeviceConfigurationType'
    { _dctChallengeRequiredOnNewDevice = Nothing
    , _dctDeviceOnlyRememberedOnUserPrompt = Nothing
    }

-- | Indicates whether a challenge is required on a new device. Only applicable to a new device.
dctChallengeRequiredOnNewDevice :: Lens' DeviceConfigurationType (Maybe Bool)
dctChallengeRequiredOnNewDevice = lens _dctChallengeRequiredOnNewDevice (\ s a -> s{_dctChallengeRequiredOnNewDevice = a});

-- | If true, a device is only remembered on user prompt.
dctDeviceOnlyRememberedOnUserPrompt :: Lens' DeviceConfigurationType (Maybe Bool)
dctDeviceOnlyRememberedOnUserPrompt = lens _dctDeviceOnlyRememberedOnUserPrompt (\ s a -> s{_dctDeviceOnlyRememberedOnUserPrompt = a});

instance FromJSON DeviceConfigurationType where
        parseJSON
          = withObject "DeviceConfigurationType"
              (\ x ->
                 DeviceConfigurationType' <$>
                   (x .:? "ChallengeRequiredOnNewDevice") <*>
                     (x .:? "DeviceOnlyRememberedOnUserPrompt"))

instance Hashable DeviceConfigurationType

instance NFData DeviceConfigurationType

instance ToJSON DeviceConfigurationType where
        toJSON DeviceConfigurationType'{..}
          = object
              (catMaybes
                 [("ChallengeRequiredOnNewDevice" .=) <$>
                    _dctChallengeRequiredOnNewDevice,
                  ("DeviceOnlyRememberedOnUserPrompt" .=) <$>
                    _dctDeviceOnlyRememberedOnUserPrompt])

-- | The device verifier against which it will be authenticated.
--
-- /See:/ 'deviceSecretVerifierConfigType' smart constructor.
data DeviceSecretVerifierConfigType = DeviceSecretVerifierConfigType'
    { _dsvctPasswordVerifier :: !(Maybe Text)
    , _dsvctSalt             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeviceSecretVerifierConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsvctPasswordVerifier'
--
-- * 'dsvctSalt'
deviceSecretVerifierConfigType
    :: DeviceSecretVerifierConfigType
deviceSecretVerifierConfigType =
    DeviceSecretVerifierConfigType'
    { _dsvctPasswordVerifier = Nothing
    , _dsvctSalt = Nothing
    }

-- | The password verifier.
dsvctPasswordVerifier :: Lens' DeviceSecretVerifierConfigType (Maybe Text)
dsvctPasswordVerifier = lens _dsvctPasswordVerifier (\ s a -> s{_dsvctPasswordVerifier = a});

-- | The salt.
dsvctSalt :: Lens' DeviceSecretVerifierConfigType (Maybe Text)
dsvctSalt = lens _dsvctSalt (\ s a -> s{_dsvctSalt = a});

instance Hashable DeviceSecretVerifierConfigType

instance NFData DeviceSecretVerifierConfigType

instance ToJSON DeviceSecretVerifierConfigType where
        toJSON DeviceSecretVerifierConfigType'{..}
          = object
              (catMaybes
                 [("PasswordVerifier" .=) <$> _dsvctPasswordVerifier,
                  ("Salt" .=) <$> _dsvctSalt])

-- | The device type.
--
-- /See:/ 'deviceType' smart constructor.
data DeviceType = DeviceType'
    { _dtDeviceLastModifiedDate      :: !(Maybe POSIX)
    , _dtDeviceCreateDate            :: !(Maybe POSIX)
    , _dtDeviceAttributes            :: !(Maybe [AttributeType])
    , _dtDeviceKey                   :: !(Maybe Text)
    , _dtDeviceLastAuthenticatedDate :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeviceType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtDeviceLastModifiedDate'
--
-- * 'dtDeviceCreateDate'
--
-- * 'dtDeviceAttributes'
--
-- * 'dtDeviceKey'
--
-- * 'dtDeviceLastAuthenticatedDate'
deviceType
    :: DeviceType
deviceType =
    DeviceType'
    { _dtDeviceLastModifiedDate = Nothing
    , _dtDeviceCreateDate = Nothing
    , _dtDeviceAttributes = Nothing
    , _dtDeviceKey = Nothing
    , _dtDeviceLastAuthenticatedDate = Nothing
    }

-- | The last modified date of the device.
dtDeviceLastModifiedDate :: Lens' DeviceType (Maybe UTCTime)
dtDeviceLastModifiedDate = lens _dtDeviceLastModifiedDate (\ s a -> s{_dtDeviceLastModifiedDate = a}) . mapping _Time;

-- | The creation date of the device.
dtDeviceCreateDate :: Lens' DeviceType (Maybe UTCTime)
dtDeviceCreateDate = lens _dtDeviceCreateDate (\ s a -> s{_dtDeviceCreateDate = a}) . mapping _Time;

-- | The device attributes.
dtDeviceAttributes :: Lens' DeviceType [AttributeType]
dtDeviceAttributes = lens _dtDeviceAttributes (\ s a -> s{_dtDeviceAttributes = a}) . _Default . _Coerce;

-- | The device key.
dtDeviceKey :: Lens' DeviceType (Maybe Text)
dtDeviceKey = lens _dtDeviceKey (\ s a -> s{_dtDeviceKey = a});

-- | The date in which the device was last authenticated.
dtDeviceLastAuthenticatedDate :: Lens' DeviceType (Maybe UTCTime)
dtDeviceLastAuthenticatedDate = lens _dtDeviceLastAuthenticatedDate (\ s a -> s{_dtDeviceLastAuthenticatedDate = a}) . mapping _Time;

instance FromJSON DeviceType where
        parseJSON
          = withObject "DeviceType"
              (\ x ->
                 DeviceType' <$>
                   (x .:? "DeviceLastModifiedDate") <*>
                     (x .:? "DeviceCreateDate")
                     <*> (x .:? "DeviceAttributes" .!= mempty)
                     <*> (x .:? "DeviceKey")
                     <*> (x .:? "DeviceLastAuthenticatedDate"))

instance Hashable DeviceType

instance NFData DeviceType

-- | The email configuration type.
--
-- /See:/ 'emailConfigurationType' smart constructor.
data EmailConfigurationType = EmailConfigurationType'
    { _ectSourceARN           :: !(Maybe Text)
    , _ectReplyToEmailAddress :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EmailConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ectSourceARN'
--
-- * 'ectReplyToEmailAddress'
emailConfigurationType
    :: EmailConfigurationType
emailConfigurationType =
    EmailConfigurationType'
    { _ectSourceARN = Nothing
    , _ectReplyToEmailAddress = Nothing
    }

-- | The Amazon Resource Name (ARN) of the email source.
ectSourceARN :: Lens' EmailConfigurationType (Maybe Text)
ectSourceARN = lens _ectSourceARN (\ s a -> s{_ectSourceARN = a});

-- | The REPLY-TO email address.
ectReplyToEmailAddress :: Lens' EmailConfigurationType (Maybe Text)
ectReplyToEmailAddress = lens _ectReplyToEmailAddress (\ s a -> s{_ectReplyToEmailAddress = a});

instance FromJSON EmailConfigurationType where
        parseJSON
          = withObject "EmailConfigurationType"
              (\ x ->
                 EmailConfigurationType' <$>
                   (x .:? "SourceArn") <*>
                     (x .:? "ReplyToEmailAddress"))

instance Hashable EmailConfigurationType

instance NFData EmailConfigurationType

instance ToJSON EmailConfigurationType where
        toJSON EmailConfigurationType'{..}
          = object
              (catMaybes
                 [("SourceArn" .=) <$> _ectSourceARN,
                  ("ReplyToEmailAddress" .=) <$>
                    _ectReplyToEmailAddress])

-- | Specifies the type of configuration for AWS Lambda triggers.
--
-- /See:/ 'lambdaConfigType' smart constructor.
data LambdaConfigType = LambdaConfigType'
    { _lctPreAuthentication           :: !(Maybe Text)
    , _lctCreateAuthChallenge         :: !(Maybe Text)
    , _lctVerifyAuthChallengeResponse :: !(Maybe Text)
    , _lctPostAuthentication          :: !(Maybe Text)
    , _lctCustomMessage               :: !(Maybe Text)
    , _lctDefineAuthChallenge         :: !(Maybe Text)
    , _lctPostConfirmation            :: !(Maybe Text)
    , _lctPreSignUp                   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LambdaConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lctPreAuthentication'
--
-- * 'lctCreateAuthChallenge'
--
-- * 'lctVerifyAuthChallengeResponse'
--
-- * 'lctPostAuthentication'
--
-- * 'lctCustomMessage'
--
-- * 'lctDefineAuthChallenge'
--
-- * 'lctPostConfirmation'
--
-- * 'lctPreSignUp'
lambdaConfigType
    :: LambdaConfigType
lambdaConfigType =
    LambdaConfigType'
    { _lctPreAuthentication = Nothing
    , _lctCreateAuthChallenge = Nothing
    , _lctVerifyAuthChallengeResponse = Nothing
    , _lctPostAuthentication = Nothing
    , _lctCustomMessage = Nothing
    , _lctDefineAuthChallenge = Nothing
    , _lctPostConfirmation = Nothing
    , _lctPreSignUp = Nothing
    }

-- | A pre-authentication AWS Lambda trigger.
lctPreAuthentication :: Lens' LambdaConfigType (Maybe Text)
lctPreAuthentication = lens _lctPreAuthentication (\ s a -> s{_lctPreAuthentication = a});

-- | Creates an authentication challenge.
lctCreateAuthChallenge :: Lens' LambdaConfigType (Maybe Text)
lctCreateAuthChallenge = lens _lctCreateAuthChallenge (\ s a -> s{_lctCreateAuthChallenge = a});

-- | Verifies the authentication challenge response.
lctVerifyAuthChallengeResponse :: Lens' LambdaConfigType (Maybe Text)
lctVerifyAuthChallengeResponse = lens _lctVerifyAuthChallengeResponse (\ s a -> s{_lctVerifyAuthChallengeResponse = a});

-- | A post-authentication AWS Lambda trigger.
lctPostAuthentication :: Lens' LambdaConfigType (Maybe Text)
lctPostAuthentication = lens _lctPostAuthentication (\ s a -> s{_lctPostAuthentication = a});

-- | A custom Message AWS Lambda trigger.
lctCustomMessage :: Lens' LambdaConfigType (Maybe Text)
lctCustomMessage = lens _lctCustomMessage (\ s a -> s{_lctCustomMessage = a});

-- | Defines the authentication challenge.
lctDefineAuthChallenge :: Lens' LambdaConfigType (Maybe Text)
lctDefineAuthChallenge = lens _lctDefineAuthChallenge (\ s a -> s{_lctDefineAuthChallenge = a});

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
                     (x .:? "CreateAuthChallenge")
                     <*> (x .:? "VerifyAuthChallengeResponse")
                     <*> (x .:? "PostAuthentication")
                     <*> (x .:? "CustomMessage")
                     <*> (x .:? "DefineAuthChallenge")
                     <*> (x .:? "PostConfirmation")
                     <*> (x .:? "PreSignUp"))

instance Hashable LambdaConfigType

instance NFData LambdaConfigType

instance ToJSON LambdaConfigType where
        toJSON LambdaConfigType'{..}
          = object
              (catMaybes
                 [("PreAuthentication" .=) <$> _lctPreAuthentication,
                  ("CreateAuthChallenge" .=) <$>
                    _lctCreateAuthChallenge,
                  ("VerifyAuthChallengeResponse" .=) <$>
                    _lctVerifyAuthChallengeResponse,
                  ("PostAuthentication" .=) <$> _lctPostAuthentication,
                  ("CustomMessage" .=) <$> _lctCustomMessage,
                  ("DefineAuthChallenge" .=) <$>
                    _lctDefineAuthChallenge,
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

-- | The new device metadata type.
--
-- /See:/ 'newDeviceMetadataType' smart constructor.
data NewDeviceMetadataType = NewDeviceMetadataType'
    { _ndmtDeviceGroupKey :: !(Maybe Text)
    , _ndmtDeviceKey      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NewDeviceMetadataType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ndmtDeviceGroupKey'
--
-- * 'ndmtDeviceKey'
newDeviceMetadataType
    :: NewDeviceMetadataType
newDeviceMetadataType =
    NewDeviceMetadataType'
    { _ndmtDeviceGroupKey = Nothing
    , _ndmtDeviceKey = Nothing
    }

-- | The device group key.
ndmtDeviceGroupKey :: Lens' NewDeviceMetadataType (Maybe Text)
ndmtDeviceGroupKey = lens _ndmtDeviceGroupKey (\ s a -> s{_ndmtDeviceGroupKey = a});

-- | The device key.
ndmtDeviceKey :: Lens' NewDeviceMetadataType (Maybe Text)
ndmtDeviceKey = lens _ndmtDeviceKey (\ s a -> s{_ndmtDeviceKey = a});

instance FromJSON NewDeviceMetadataType where
        parseJSON
          = withObject "NewDeviceMetadataType"
              (\ x ->
                 NewDeviceMetadataType' <$>
                   (x .:? "DeviceGroupKey") <*> (x .:? "DeviceKey"))

instance Hashable NewDeviceMetadataType

instance NFData NewDeviceMetadataType

-- | The minimum and maximum value of an attribute that is of the number data type.
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

-- | In the password policy that you have set, refers to whether you have required users to use at least one number in their password.
pptRequireNumbers :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireNumbers = lens _pptRequireNumbers (\ s a -> s{_pptRequireNumbers = a});

-- | In the password policy that you have set, refers to whether you have required users to use at least one uppercase letter in their password.
pptRequireUppercase :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireUppercase = lens _pptRequireUppercase (\ s a -> s{_pptRequireUppercase = a});

-- | In the password policy that you have set, refers to whether you have required users to use at least one lowercase letter in their password.
pptRequireLowercase :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireLowercase = lens _pptRequireLowercase (\ s a -> s{_pptRequireLowercase = a});

-- | The minimum length of the password policy that you have set. Cannot be less than 6.
pptMinimumLength :: Lens' PasswordPolicyType (Maybe Natural)
pptMinimumLength = lens _pptMinimumLength (\ s a -> s{_pptMinimumLength = a}) . mapping _Nat;

-- | In the password policy that you have set, refers to whether you have required users to use at least one symbol in their password.
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

-- | Specifies whether a user pool attribute is required. If the attribute is required and the user does not provide a value, registration or sign-in will fail.
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

-- | The SMS configuratoin type.
--
-- /See:/ 'smsConfigurationType' smart constructor.
data SmsConfigurationType = SmsConfigurationType'
    { _sctSNSCallerARN :: !(Maybe Text)
    , _sctExternalId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SmsConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sctSNSCallerARN'
--
-- * 'sctExternalId'
smsConfigurationType
    :: SmsConfigurationType
smsConfigurationType =
    SmsConfigurationType'
    { _sctSNSCallerARN = Nothing
    , _sctExternalId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) caller.
sctSNSCallerARN :: Lens' SmsConfigurationType (Maybe Text)
sctSNSCallerARN = lens _sctSNSCallerARN (\ s a -> s{_sctSNSCallerARN = a});

-- | The external ID.
sctExternalId :: Lens' SmsConfigurationType (Maybe Text)
sctExternalId = lens _sctExternalId (\ s a -> s{_sctExternalId = a});

instance FromJSON SmsConfigurationType where
        parseJSON
          = withObject "SmsConfigurationType"
              (\ x ->
                 SmsConfigurationType' <$>
                   (x .:? "SnsCallerArn") <*> (x .:? "ExternalId"))

instance Hashable SmsConfigurationType

instance NFData SmsConfigurationType

instance ToJSON SmsConfigurationType where
        toJSON SmsConfigurationType'{..}
          = object
              (catMaybes
                 [("SnsCallerArn" .=) <$> _sctSNSCallerARN,
                  ("ExternalId" .=) <$> _sctExternalId])

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

-- | The user import job type.
--
-- /See:/ 'userImportJobType' smart constructor.
data UserImportJobType = UserImportJobType'
    { _uijtStatus                :: !(Maybe UserImportJobStatusType)
    , _uijtSkippedUsers          :: !(Maybe Integer)
    , _uijtJobId                 :: !(Maybe Text)
    , _uijtUserPoolId            :: !(Maybe Text)
    , _uijtJobName               :: !(Maybe Text)
    , _uijtPreSignedURL          :: !(Maybe Text)
    , _uijtFailedUsers           :: !(Maybe Integer)
    , _uijtStartDate             :: !(Maybe POSIX)
    , _uijtCompletionMessage     :: !(Maybe Text)
    , _uijtCreationDate          :: !(Maybe POSIX)
    , _uijtCompletionDate        :: !(Maybe POSIX)
    , _uijtCloudWatchLogsRoleARN :: !(Maybe Text)
    , _uijtImportedUsers         :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserImportJobType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uijtStatus'
--
-- * 'uijtSkippedUsers'
--
-- * 'uijtJobId'
--
-- * 'uijtUserPoolId'
--
-- * 'uijtJobName'
--
-- * 'uijtPreSignedURL'
--
-- * 'uijtFailedUsers'
--
-- * 'uijtStartDate'
--
-- * 'uijtCompletionMessage'
--
-- * 'uijtCreationDate'
--
-- * 'uijtCompletionDate'
--
-- * 'uijtCloudWatchLogsRoleARN'
--
-- * 'uijtImportedUsers'
userImportJobType
    :: UserImportJobType
userImportJobType =
    UserImportJobType'
    { _uijtStatus = Nothing
    , _uijtSkippedUsers = Nothing
    , _uijtJobId = Nothing
    , _uijtUserPoolId = Nothing
    , _uijtJobName = Nothing
    , _uijtPreSignedURL = Nothing
    , _uijtFailedUsers = Nothing
    , _uijtStartDate = Nothing
    , _uijtCompletionMessage = Nothing
    , _uijtCreationDate = Nothing
    , _uijtCompletionDate = Nothing
    , _uijtCloudWatchLogsRoleARN = Nothing
    , _uijtImportedUsers = Nothing
    }

-- | The status of the user import job. One of the following:
--
-- -   Created - The job was created but not started.
-- -   Pending - A transition state. You have started the job, but it has not begun importing users yet.
-- -   InProgress - The job has started, and users are being imported.
-- -   Stopping - You have stopped the job, but the job has not stopped importing users yet.
-- -   Stopped - You have stopped the job, and the job has stopped importing users.
-- -   Succeeded - The job has completed successfully.
-- -   Failed - The job has stopped due to an error.
-- -   Expired - You created a job, but did not start the job within 24-48 hours. All data associated with the job was deleted, and the job cannot be started.
uijtStatus :: Lens' UserImportJobType (Maybe UserImportJobStatusType)
uijtStatus = lens _uijtStatus (\ s a -> s{_uijtStatus = a});

-- | The number of users that were skipped.
uijtSkippedUsers :: Lens' UserImportJobType (Maybe Integer)
uijtSkippedUsers = lens _uijtSkippedUsers (\ s a -> s{_uijtSkippedUsers = a});

-- | The job ID for the user import job.
uijtJobId :: Lens' UserImportJobType (Maybe Text)
uijtJobId = lens _uijtJobId (\ s a -> s{_uijtJobId = a});

-- | The user pool ID for the user pool that the users are being imported into.
uijtUserPoolId :: Lens' UserImportJobType (Maybe Text)
uijtUserPoolId = lens _uijtUserPoolId (\ s a -> s{_uijtUserPoolId = a});

-- | The job name for the user import job.
uijtJobName :: Lens' UserImportJobType (Maybe Text)
uijtJobName = lens _uijtJobName (\ s a -> s{_uijtJobName = a});

-- | The pre-signed URL to be used to upload the .csv file.
uijtPreSignedURL :: Lens' UserImportJobType (Maybe Text)
uijtPreSignedURL = lens _uijtPreSignedURL (\ s a -> s{_uijtPreSignedURL = a});

-- | The number of users that could not be imported.
uijtFailedUsers :: Lens' UserImportJobType (Maybe Integer)
uijtFailedUsers = lens _uijtFailedUsers (\ s a -> s{_uijtFailedUsers = a});

-- | The date when the user import job was started.
uijtStartDate :: Lens' UserImportJobType (Maybe UTCTime)
uijtStartDate = lens _uijtStartDate (\ s a -> s{_uijtStartDate = a}) . mapping _Time;

-- | The message returned when the user import job is completed.
uijtCompletionMessage :: Lens' UserImportJobType (Maybe Text)
uijtCompletionMessage = lens _uijtCompletionMessage (\ s a -> s{_uijtCompletionMessage = a});

-- | The date when the user import job was created.
uijtCreationDate :: Lens' UserImportJobType (Maybe UTCTime)
uijtCreationDate = lens _uijtCreationDate (\ s a -> s{_uijtCreationDate = a}) . mapping _Time;

-- | The date when the user imoprt job was completed.
uijtCompletionDate :: Lens' UserImportJobType (Maybe UTCTime)
uijtCompletionDate = lens _uijtCompletionDate (\ s a -> s{_uijtCompletionDate = a}) . mapping _Time;

-- | The role ARN for the Amazon CloudWatch Logging role for the user import job. For more information, see \"Creating the CloudWatch Logs IAM Role\" in the Amazon Cognito Developer Guide.
uijtCloudWatchLogsRoleARN :: Lens' UserImportJobType (Maybe Text)
uijtCloudWatchLogsRoleARN = lens _uijtCloudWatchLogsRoleARN (\ s a -> s{_uijtCloudWatchLogsRoleARN = a});

-- | The number of users that were successfully imported.
uijtImportedUsers :: Lens' UserImportJobType (Maybe Integer)
uijtImportedUsers = lens _uijtImportedUsers (\ s a -> s{_uijtImportedUsers = a});

instance FromJSON UserImportJobType where
        parseJSON
          = withObject "UserImportJobType"
              (\ x ->
                 UserImportJobType' <$>
                   (x .:? "Status") <*> (x .:? "SkippedUsers") <*>
                     (x .:? "JobId")
                     <*> (x .:? "UserPoolId")
                     <*> (x .:? "JobName")
                     <*> (x .:? "PreSignedUrl")
                     <*> (x .:? "FailedUsers")
                     <*> (x .:? "StartDate")
                     <*> (x .:? "CompletionMessage")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "CompletionDate")
                     <*> (x .:? "CloudWatchLogsRoleArn")
                     <*> (x .:? "ImportedUsers"))

instance Hashable UserImportJobType

instance NFData UserImportJobType

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

-- | The user pool ID for the user pool where you want to describe the user pool client.
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
    { _upctRefreshTokenValidity :: !(Maybe Nat)
    , _upctClientId             :: !(Maybe (Sensitive Text))
    , _upctExplicitAuthFlows    :: !(Maybe [ExplicitAuthFlowsType])
    , _upctClientSecret         :: !(Maybe (Sensitive Text))
    , _upctLastModifiedDate     :: !(Maybe POSIX)
    , _upctUserPoolId           :: !(Maybe Text)
    , _upctWriteAttributes      :: !(Maybe [Text])
    , _upctCreationDate         :: !(Maybe POSIX)
    , _upctReadAttributes       :: !(Maybe [Text])
    , _upctClientName           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserPoolClientType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upctRefreshTokenValidity'
--
-- * 'upctClientId'
--
-- * 'upctExplicitAuthFlows'
--
-- * 'upctClientSecret'
--
-- * 'upctLastModifiedDate'
--
-- * 'upctUserPoolId'
--
-- * 'upctWriteAttributes'
--
-- * 'upctCreationDate'
--
-- * 'upctReadAttributes'
--
-- * 'upctClientName'
userPoolClientType
    :: UserPoolClientType
userPoolClientType =
    UserPoolClientType'
    { _upctRefreshTokenValidity = Nothing
    , _upctClientId = Nothing
    , _upctExplicitAuthFlows = Nothing
    , _upctClientSecret = Nothing
    , _upctLastModifiedDate = Nothing
    , _upctUserPoolId = Nothing
    , _upctWriteAttributes = Nothing
    , _upctCreationDate = Nothing
    , _upctReadAttributes = Nothing
    , _upctClientName = Nothing
    }

-- | The validity of the refresh token.
upctRefreshTokenValidity :: Lens' UserPoolClientType (Maybe Natural)
upctRefreshTokenValidity = lens _upctRefreshTokenValidity (\ s a -> s{_upctRefreshTokenValidity = a}) . mapping _Nat;

-- | The ID of the client associated with the user pool.
upctClientId :: Lens' UserPoolClientType (Maybe Text)
upctClientId = lens _upctClientId (\ s a -> s{_upctClientId = a}) . mapping _Sensitive;

-- | The explicit authentication flows.
upctExplicitAuthFlows :: Lens' UserPoolClientType [ExplicitAuthFlowsType]
upctExplicitAuthFlows = lens _upctExplicitAuthFlows (\ s a -> s{_upctExplicitAuthFlows = a}) . _Default . _Coerce;

-- | The client secret from the user pool request of the client type.
upctClientSecret :: Lens' UserPoolClientType (Maybe Text)
upctClientSecret = lens _upctClientSecret (\ s a -> s{_upctClientSecret = a}) . mapping _Sensitive;

-- | The last modified date from the user pool request of the client type.
upctLastModifiedDate :: Lens' UserPoolClientType (Maybe UTCTime)
upctLastModifiedDate = lens _upctLastModifiedDate (\ s a -> s{_upctLastModifiedDate = a}) . mapping _Time;

-- | The user pool ID for the user pool client.
upctUserPoolId :: Lens' UserPoolClientType (Maybe Text)
upctUserPoolId = lens _upctUserPoolId (\ s a -> s{_upctUserPoolId = a});

-- | The writeable attributes.
upctWriteAttributes :: Lens' UserPoolClientType [Text]
upctWriteAttributes = lens _upctWriteAttributes (\ s a -> s{_upctWriteAttributes = a}) . _Default . _Coerce;

-- | The creation date from the user pool request of the client type.
upctCreationDate :: Lens' UserPoolClientType (Maybe UTCTime)
upctCreationDate = lens _upctCreationDate (\ s a -> s{_upctCreationDate = a}) . mapping _Time;

-- | The Read-only attributes.
upctReadAttributes :: Lens' UserPoolClientType [Text]
upctReadAttributes = lens _upctReadAttributes (\ s a -> s{_upctReadAttributes = a}) . _Default . _Coerce;

-- | The client name from the user pool request of the client type.
upctClientName :: Lens' UserPoolClientType (Maybe Text)
upctClientName = lens _upctClientName (\ s a -> s{_upctClientName = a});

instance FromJSON UserPoolClientType where
        parseJSON
          = withObject "UserPoolClientType"
              (\ x ->
                 UserPoolClientType' <$>
                   (x .:? "RefreshTokenValidity") <*> (x .:? "ClientId")
                     <*> (x .:? "ExplicitAuthFlows" .!= mempty)
                     <*> (x .:? "ClientSecret")
                     <*> (x .:? "LastModifiedDate")
                     <*> (x .:? "UserPoolId")
                     <*> (x .:? "WriteAttributes" .!= mempty)
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "ReadAttributes" .!= mempty)
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
    { _uptStatus                    :: !(Maybe StatusType)
    , _uptEmailConfigurationFailure :: !(Maybe Text)
    , _uptLastModifiedDate          :: !(Maybe POSIX)
    , _uptEstimatedNumberOfUsers    :: !(Maybe Int)
    , _uptEmailVerificationMessage  :: !(Maybe Text)
    , _uptSmsAuthenticationMessage  :: !(Maybe Text)
    , _uptSchemaAttributes          :: !(Maybe (List1 SchemaAttributeType))
    , _uptEmailVerificationSubject  :: !(Maybe Text)
    , _uptAliasAttributes           :: !(Maybe [AliasAttributeType])
    , _uptEmailConfiguration        :: !(Maybe EmailConfigurationType)
    , _uptSmsVerificationMessage    :: !(Maybe Text)
    , _uptName                      :: !(Maybe Text)
    , _uptMFAConfiguration          :: !(Maybe UserPoolMFAType)
    , _uptId                        :: !(Maybe Text)
    , _uptSmsConfigurationFailure   :: !(Maybe Text)
    , _uptCreationDate              :: !(Maybe POSIX)
    , _uptLambdaConfig              :: !(Maybe LambdaConfigType)
    , _uptSmsConfiguration          :: !(Maybe SmsConfigurationType)
    , _uptDeviceConfiguration       :: !(Maybe DeviceConfigurationType)
    , _uptAutoVerifiedAttributes    :: !(Maybe [VerifiedAttributeType])
    , _uptPolicies                  :: !(Maybe UserPoolPolicyType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserPoolType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uptStatus'
--
-- * 'uptEmailConfigurationFailure'
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
-- * 'uptEmailConfiguration'
--
-- * 'uptSmsVerificationMessage'
--
-- * 'uptName'
--
-- * 'uptMFAConfiguration'
--
-- * 'uptId'
--
-- * 'uptSmsConfigurationFailure'
--
-- * 'uptCreationDate'
--
-- * 'uptLambdaConfig'
--
-- * 'uptSmsConfiguration'
--
-- * 'uptDeviceConfiguration'
--
-- * 'uptAutoVerifiedAttributes'
--
-- * 'uptPolicies'
userPoolType
    :: UserPoolType
userPoolType =
    UserPoolType'
    { _uptStatus = Nothing
    , _uptEmailConfigurationFailure = Nothing
    , _uptLastModifiedDate = Nothing
    , _uptEstimatedNumberOfUsers = Nothing
    , _uptEmailVerificationMessage = Nothing
    , _uptSmsAuthenticationMessage = Nothing
    , _uptSchemaAttributes = Nothing
    , _uptEmailVerificationSubject = Nothing
    , _uptAliasAttributes = Nothing
    , _uptEmailConfiguration = Nothing
    , _uptSmsVerificationMessage = Nothing
    , _uptName = Nothing
    , _uptMFAConfiguration = Nothing
    , _uptId = Nothing
    , _uptSmsConfigurationFailure = Nothing
    , _uptCreationDate = Nothing
    , _uptLambdaConfig = Nothing
    , _uptSmsConfiguration = Nothing
    , _uptDeviceConfiguration = Nothing
    , _uptAutoVerifiedAttributes = Nothing
    , _uptPolicies = Nothing
    }

-- | The status of a user pool.
uptStatus :: Lens' UserPoolType (Maybe StatusType)
uptStatus = lens _uptStatus (\ s a -> s{_uptStatus = a});

-- | The reason why the email configuration cannot send the messages to your users.
uptEmailConfigurationFailure :: Lens' UserPoolType (Maybe Text)
uptEmailConfigurationFailure = lens _uptEmailConfigurationFailure (\ s a -> s{_uptEmailConfigurationFailure = a});

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

-- | The email configuration.
uptEmailConfiguration :: Lens' UserPoolType (Maybe EmailConfigurationType)
uptEmailConfiguration = lens _uptEmailConfiguration (\ s a -> s{_uptEmailConfiguration = a});

-- | The contents of the SMS verification message.
uptSmsVerificationMessage :: Lens' UserPoolType (Maybe Text)
uptSmsVerificationMessage = lens _uptSmsVerificationMessage (\ s a -> s{_uptSmsVerificationMessage = a});

-- | The name of the user pool.
uptName :: Lens' UserPoolType (Maybe Text)
uptName = lens _uptName (\ s a -> s{_uptName = a});

-- | Can be one of the following values:
--
-- -   'OFF' - MFA tokens are not required and cannot be specified during user registration.
-- -   'ON' - MFA tokens are required for all user registrations. You can only specify required when you are initially creating a user pool.
-- -   'OPTIONAL' - Users have the option when registering to create an MFA token.
uptMFAConfiguration :: Lens' UserPoolType (Maybe UserPoolMFAType)
uptMFAConfiguration = lens _uptMFAConfiguration (\ s a -> s{_uptMFAConfiguration = a});

-- | The ID of the user pool.
uptId :: Lens' UserPoolType (Maybe Text)
uptId = lens _uptId (\ s a -> s{_uptId = a});

-- | The reason why the SMS configuration cannot send the message(s) to your users.
uptSmsConfigurationFailure :: Lens' UserPoolType (Maybe Text)
uptSmsConfigurationFailure = lens _uptSmsConfigurationFailure (\ s a -> s{_uptSmsConfigurationFailure = a});

-- | The creation date of a user pool.
uptCreationDate :: Lens' UserPoolType (Maybe UTCTime)
uptCreationDate = lens _uptCreationDate (\ s a -> s{_uptCreationDate = a}) . mapping _Time;

-- | A container describing the AWS Lambda triggers associated with a user pool.
uptLambdaConfig :: Lens' UserPoolType (Maybe LambdaConfigType)
uptLambdaConfig = lens _uptLambdaConfig (\ s a -> s{_uptLambdaConfig = a});

-- | The SMS configuration.
uptSmsConfiguration :: Lens' UserPoolType (Maybe SmsConfigurationType)
uptSmsConfiguration = lens _uptSmsConfiguration (\ s a -> s{_uptSmsConfiguration = a});

-- | The device configuration.
uptDeviceConfiguration :: Lens' UserPoolType (Maybe DeviceConfigurationType)
uptDeviceConfiguration = lens _uptDeviceConfiguration (\ s a -> s{_uptDeviceConfiguration = a});

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
                   (x .:? "Status") <*>
                     (x .:? "EmailConfigurationFailure")
                     <*> (x .:? "LastModifiedDate")
                     <*> (x .:? "EstimatedNumberOfUsers")
                     <*> (x .:? "EmailVerificationMessage")
                     <*> (x .:? "SmsAuthenticationMessage")
                     <*> (x .:? "SchemaAttributes")
                     <*> (x .:? "EmailVerificationSubject")
                     <*> (x .:? "AliasAttributes" .!= mempty)
                     <*> (x .:? "EmailConfiguration")
                     <*> (x .:? "SmsVerificationMessage")
                     <*> (x .:? "Name")
                     <*> (x .:? "MfaConfiguration")
                     <*> (x .:? "Id")
                     <*> (x .:? "SmsConfigurationFailure")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LambdaConfig")
                     <*> (x .:? "SmsConfiguration")
                     <*> (x .:? "DeviceConfiguration")
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
