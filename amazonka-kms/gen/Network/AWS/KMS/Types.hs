{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.KMS.Types
    (
    -- * Service
      KMS

    -- * Errors
    , _InvalidMarkerException
    , _InvalidKeyUsageException
    , _UnsupportedOperationException
    , _MalformedPolicyDocumentException
    , _DisabledException
    , _KeyUnavailableException
    , _KMSInternalException
    , _NotFoundException
    , _InvalidAliasNameException
    , _InvalidARNException
    , _DependencyTimeoutException
    , _InvalidGrantTokenException
    , _InvalidCiphertextException
    , _LimitExceededException
    , _AlreadyExistsException

    -- * DataKeySpec
    , DataKeySpec (..)

    -- * GrantOperation
    , GrantOperation (..)

    -- * KeyUsageType
    , KeyUsageType (..)

    -- * AliasListEntry
    , AliasListEntry
    , aliasListEntry
    , aleTargetKeyId
    , aleAliasName
    , aleAliasARN

    -- * GrantConstraints
    , GrantConstraints
    , grantConstraints
    , gcEncryptionContextEquals
    , gcEncryptionContextSubset

    -- * GrantListEntry
    , GrantListEntry
    , grantListEntry
    , gleRetiringPrincipal
    , gleIssuingAccount
    , gleGrantId
    , gleConstraints
    , gleGranteePrincipal
    , gleOperations

    -- * KeyListEntry
    , KeyListEntry
    , keyListEntry
    , kleKeyARN
    , kleKeyId

    -- * KeyMetadata
    , KeyMetadata
    , keyMetadata
    , kmARN
    , kmEnabled
    , kmAWSAccountId
    , kmKeyUsage
    , kmCreationDate
    , kmDescription
    , kmKeyId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-11-01@ of the Amazon Key Management Service SDK.
data KMS

instance AWSService KMS where
    type Sg KMS = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "KMS"
            , _svcPrefix = "kms"
            , _svcVersion = "2014-11-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The request was rejected because the marker that specifies where
-- pagination should next begin is not valid.
_InvalidMarkerException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidMarkerException =
    _ServiceError . hasStatus 400 . hasCode "InvalidMarker"

-- | The request was rejected because the specified KeySpec parameter is not
-- valid. The currently supported value is ENCRYPT\/DECRYPT.
_InvalidKeyUsageException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidKeyUsageException =
    _ServiceError . hasStatus 400 . hasCode "InvalidKeyUsage"

-- | The request was rejected because a specified parameter is not supported.
_UnsupportedOperationException :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedOperation"

-- | The request was rejected because the specified policy is not
-- syntactically or semantically correct.
_MalformedPolicyDocumentException :: AWSError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _ServiceError . hasStatus 400 . hasCode "MalformedPolicyDocument"

-- | A request was rejected because the specified key was marked as disabled.
_DisabledException :: AWSError a => Getting (First ServiceError) a ServiceError
_DisabledException = _ServiceError . hasStatus 409 . hasCode "Disabled"

-- | The request was rejected because the key was disabled, not found, or
-- otherwise not available.
_KeyUnavailableException :: AWSError a => Getting (First ServiceError) a ServiceError
_KeyUnavailableException =
    _ServiceError . hasStatus 500 . hasCode "KeyUnavailable"

-- | The request was rejected because an internal exception occurred. This
-- error can be retried.
_KMSInternalException :: AWSError a => Getting (First ServiceError) a ServiceError
_KMSInternalException = _ServiceError . hasStatus 500 . hasCode "KMSInternal"

-- | The request was rejected because the specified entity or resource could
-- not be found.
_NotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _ServiceError . hasStatus 404 . hasCode "NotFound"

-- | The request was rejected because the specified alias name is not valid.
_InvalidAliasNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidAliasNameException =
    _ServiceError . hasStatus 400 . hasCode "InvalidAliasName"

-- | The request was rejected because a specified ARN was not valid.
_InvalidARNException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidARNException = _ServiceError . hasStatus 400 . hasCode "InvalidArn"

-- | The system timed out while trying to fulfill the request.
_DependencyTimeoutException :: AWSError a => Getting (First ServiceError) a ServiceError
_DependencyTimeoutException =
    _ServiceError . hasStatus 503 . hasCode "DependencyTimeout"

-- | A grant token provided as part of the request is invalid.
_InvalidGrantTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidGrantTokenException =
    _ServiceError . hasStatus 400 . hasCode "InvalidGrantToken"

-- | The request was rejected because the specified ciphertext has been
-- corrupted or is otherwise invalid.
_InvalidCiphertextException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCiphertextException =
    _ServiceError . hasStatus 400 . hasCode "InvalidCiphertext"

-- | The request was rejected because a quota was exceeded.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceeded"

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_AlreadyExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
    _ServiceError . hasStatus 400 . hasCode "AlreadyExists"

data DataKeySpec
    = AES128
    | AES256
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DataKeySpec where
    parser = takeLowerText >>= \case
        "aes_128" -> pure AES128
        "aes_256" -> pure AES256
        e -> fromTextError $ "Failure parsing DataKeySpec from value: '" <> e
           <> "'. Accepted values: aes_128, aes_256"

instance ToText DataKeySpec where
    toText = \case
        AES128 -> "aes_128"
        AES256 -> "aes_256"

instance Hashable DataKeySpec where
    hashWithSalt = hashUsing fromEnum

instance ToQuery DataKeySpec
instance ToHeader DataKeySpec

instance ToJSON DataKeySpec where
    toJSON = toJSONText

data GrantOperation
    = Encrypt
    | GenerateDataKeyWithoutPlaintext
    | CreateGrant
    | RetireGrant
    | GenerateDataKey
    | Decrypt
    | ReEncryptTo
    | ReEncryptFrom
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText GrantOperation where
    parser = takeLowerText >>= \case
        "creategrant" -> pure CreateGrant
        "decrypt" -> pure Decrypt
        "encrypt" -> pure Encrypt
        "generatedatakey" -> pure GenerateDataKey
        "generatedatakeywithoutplaintext" -> pure GenerateDataKeyWithoutPlaintext
        "reencryptfrom" -> pure ReEncryptFrom
        "reencryptto" -> pure ReEncryptTo
        "retiregrant" -> pure RetireGrant
        e -> fromTextError $ "Failure parsing GrantOperation from value: '" <> e
           <> "'. Accepted values: creategrant, decrypt, encrypt, generatedatakey, generatedatakeywithoutplaintext, reencryptfrom, reencryptto, retiregrant"

instance ToText GrantOperation where
    toText = \case
        CreateGrant -> "creategrant"
        Decrypt -> "decrypt"
        Encrypt -> "encrypt"
        GenerateDataKey -> "generatedatakey"
        GenerateDataKeyWithoutPlaintext -> "generatedatakeywithoutplaintext"
        ReEncryptFrom -> "reencryptfrom"
        ReEncryptTo -> "reencryptto"
        RetireGrant -> "retiregrant"

instance Hashable GrantOperation where
    hashWithSalt = hashUsing fromEnum

instance ToQuery GrantOperation
instance ToHeader GrantOperation

instance ToJSON GrantOperation where
    toJSON = toJSONText

instance FromJSON GrantOperation where
    parseJSON = parseJSONText "GrantOperation"

data KeyUsageType =
    EncryptDecrypt
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText KeyUsageType where
    parser = takeLowerText >>= \case
        "encrypt_decrypt" -> pure EncryptDecrypt
        e -> fromTextError $ "Failure parsing KeyUsageType from value: '" <> e
           <> "'. Accepted values: encrypt_decrypt"

instance ToText KeyUsageType where
    toText = \case
        EncryptDecrypt -> "encrypt_decrypt"

instance Hashable KeyUsageType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery KeyUsageType
instance ToHeader KeyUsageType

instance ToJSON KeyUsageType where
    toJSON = toJSONText

instance FromJSON KeyUsageType where
    parseJSON = parseJSONText "KeyUsageType"

-- | Contains information about an alias.
--
-- /See:/ 'aliasListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aleTargetKeyId'
--
-- * 'aleAliasName'
--
-- * 'aleAliasARN'
data AliasListEntry = AliasListEntry'
    { _aleTargetKeyId :: !(Maybe Text)
    , _aleAliasName   :: !(Maybe Text)
    , _aleAliasARN    :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'AliasListEntry' smart constructor.
aliasListEntry :: AliasListEntry
aliasListEntry =
    AliasListEntry'
    { _aleTargetKeyId = Nothing
    , _aleAliasName = Nothing
    , _aleAliasARN = Nothing
    }

-- | String that contains the key identifier pointed to by the alias.
aleTargetKeyId :: Lens' AliasListEntry (Maybe Text)
aleTargetKeyId = lens _aleTargetKeyId (\ s a -> s{_aleTargetKeyId = a});

-- | String that contains the alias.
aleAliasName :: Lens' AliasListEntry (Maybe Text)
aleAliasName = lens _aleAliasName (\ s a -> s{_aleAliasName = a});

-- | String that contains the key ARN.
aleAliasARN :: Lens' AliasListEntry (Maybe Text)
aleAliasARN = lens _aleAliasARN (\ s a -> s{_aleAliasARN = a});

instance FromJSON AliasListEntry where
        parseJSON
          = withObject "AliasListEntry"
              (\ x ->
                 AliasListEntry' <$>
                   (x .:? "TargetKeyId") <*> (x .:? "AliasName") <*>
                     (x .:? "AliasArn"))

-- | Contains constraints on the grant.
--
-- /See:/ 'grantConstraints' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcEncryptionContextEquals'
--
-- * 'gcEncryptionContextSubset'
data GrantConstraints = GrantConstraints'
    { _gcEncryptionContextEquals :: !(Maybe (Map Text Text))
    , _gcEncryptionContextSubset :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show)

-- | 'GrantConstraints' smart constructor.
grantConstraints :: GrantConstraints
grantConstraints =
    GrantConstraints'
    { _gcEncryptionContextEquals = Nothing
    , _gcEncryptionContextSubset = Nothing
    }

-- | The constraint contains additional key\/value pairs that serve to
-- further limit the grant.
gcEncryptionContextEquals :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextEquals = lens _gcEncryptionContextEquals (\ s a -> s{_gcEncryptionContextEquals = a}) . _Default . _Map;

-- | The constraint equals the full encryption context.
gcEncryptionContextSubset :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextSubset = lens _gcEncryptionContextSubset (\ s a -> s{_gcEncryptionContextSubset = a}) . _Default . _Map;

instance FromJSON GrantConstraints where
        parseJSON
          = withObject "GrantConstraints"
              (\ x ->
                 GrantConstraints' <$>
                   (x .:? "EncryptionContextEquals" .!= mempty) <*>
                     (x .:? "EncryptionContextSubset" .!= mempty))

instance ToJSON GrantConstraints where
        toJSON GrantConstraints'{..}
          = object
              ["EncryptionContextEquals" .=
                 _gcEncryptionContextEquals,
               "EncryptionContextSubset" .=
                 _gcEncryptionContextSubset]

-- | Contains information about each entry in the grant list.
--
-- /See:/ 'grantListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gleRetiringPrincipal'
--
-- * 'gleIssuingAccount'
--
-- * 'gleGrantId'
--
-- * 'gleConstraints'
--
-- * 'gleGranteePrincipal'
--
-- * 'gleOperations'
data GrantListEntry = GrantListEntry'
    { _gleRetiringPrincipal :: !(Maybe Text)
    , _gleIssuingAccount    :: !(Maybe Text)
    , _gleGrantId           :: !(Maybe Text)
    , _gleConstraints       :: !(Maybe GrantConstraints)
    , _gleGranteePrincipal  :: !(Maybe Text)
    , _gleOperations        :: !(Maybe [GrantOperation])
    } deriving (Eq,Read,Show)

-- | 'GrantListEntry' smart constructor.
grantListEntry :: GrantListEntry
grantListEntry =
    GrantListEntry'
    { _gleRetiringPrincipal = Nothing
    , _gleIssuingAccount = Nothing
    , _gleGrantId = Nothing
    , _gleConstraints = Nothing
    , _gleGranteePrincipal = Nothing
    , _gleOperations = Nothing
    }

-- | The principal that can retire the account.
gleRetiringPrincipal :: Lens' GrantListEntry (Maybe Text)
gleRetiringPrincipal = lens _gleRetiringPrincipal (\ s a -> s{_gleRetiringPrincipal = a});

-- | The account under which the grant was issued.
gleIssuingAccount :: Lens' GrantListEntry (Maybe Text)
gleIssuingAccount = lens _gleIssuingAccount (\ s a -> s{_gleIssuingAccount = a});

-- | Unique grant identifier.
gleGrantId :: Lens' GrantListEntry (Maybe Text)
gleGrantId = lens _gleGrantId (\ s a -> s{_gleGrantId = a});

-- | Specifies the conditions under which the actions specified by the
-- @Operations@ parameter are allowed.
gleConstraints :: Lens' GrantListEntry (Maybe GrantConstraints)
gleConstraints = lens _gleConstraints (\ s a -> s{_gleConstraints = a});

-- | The principal that receives the grant permission.
gleGranteePrincipal :: Lens' GrantListEntry (Maybe Text)
gleGranteePrincipal = lens _gleGranteePrincipal (\ s a -> s{_gleGranteePrincipal = a});

-- | List of operations permitted by the grant. This can be any combination
-- of one or more of the following values:
--
-- 1.  Decrypt
-- 2.  Encrypt
-- 3.  GenerateDataKey
-- 4.  GenerateDataKeyWithoutPlaintext
-- 5.  ReEncryptFrom
-- 6.  ReEncryptTo
-- 7.  CreateGrant
gleOperations :: Lens' GrantListEntry [GrantOperation]
gleOperations = lens _gleOperations (\ s a -> s{_gleOperations = a}) . _Default;

instance FromJSON GrantListEntry where
        parseJSON
          = withObject "GrantListEntry"
              (\ x ->
                 GrantListEntry' <$>
                   (x .:? "RetiringPrincipal") <*>
                     (x .:? "IssuingAccount")
                     <*> (x .:? "GrantId")
                     <*> (x .:? "Constraints")
                     <*> (x .:? "GranteePrincipal")
                     <*> (x .:? "Operations" .!= mempty))

-- | Contains information about each entry in the key list.
--
-- /See:/ 'keyListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kleKeyARN'
--
-- * 'kleKeyId'
data KeyListEntry = KeyListEntry'
    { _kleKeyARN :: !(Maybe Text)
    , _kleKeyId  :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'KeyListEntry' smart constructor.
keyListEntry :: KeyListEntry
keyListEntry =
    KeyListEntry'
    { _kleKeyARN = Nothing
    , _kleKeyId = Nothing
    }

-- | ARN of the key.
kleKeyARN :: Lens' KeyListEntry (Maybe Text)
kleKeyARN = lens _kleKeyARN (\ s a -> s{_kleKeyARN = a});

-- | Unique identifier of the key.
kleKeyId :: Lens' KeyListEntry (Maybe Text)
kleKeyId = lens _kleKeyId (\ s a -> s{_kleKeyId = a});

instance FromJSON KeyListEntry where
        parseJSON
          = withObject "KeyListEntry"
              (\ x ->
                 KeyListEntry' <$>
                   (x .:? "KeyArn") <*> (x .:? "KeyId"))

-- | Contains metadata associated with a specific key.
--
-- /See:/ 'keyMetadata' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kmARN'
--
-- * 'kmEnabled'
--
-- * 'kmAWSAccountId'
--
-- * 'kmKeyUsage'
--
-- * 'kmCreationDate'
--
-- * 'kmDescription'
--
-- * 'kmKeyId'
data KeyMetadata = KeyMetadata'
    { _kmARN          :: !(Maybe Text)
    , _kmEnabled      :: !(Maybe Bool)
    , _kmAWSAccountId :: !(Maybe Text)
    , _kmKeyUsage     :: !(Maybe KeyUsageType)
    , _kmCreationDate :: !(Maybe POSIX)
    , _kmDescription  :: !(Maybe Text)
    , _kmKeyId        :: !Text
    } deriving (Eq,Read,Show)

-- | 'KeyMetadata' smart constructor.
keyMetadata :: Text -> KeyMetadata
keyMetadata pKeyId =
    KeyMetadata'
    { _kmARN = Nothing
    , _kmEnabled = Nothing
    , _kmAWSAccountId = Nothing
    , _kmKeyUsage = Nothing
    , _kmCreationDate = Nothing
    , _kmDescription = Nothing
    , _kmKeyId = pKeyId
    }

-- | Key ARN (Amazon Resource Name).
kmARN :: Lens' KeyMetadata (Maybe Text)
kmARN = lens _kmARN (\ s a -> s{_kmARN = a});

-- | Value that specifies whether the key is enabled.
kmEnabled :: Lens' KeyMetadata (Maybe Bool)
kmEnabled = lens _kmEnabled (\ s a -> s{_kmEnabled = a});

-- | Account ID number.
kmAWSAccountId :: Lens' KeyMetadata (Maybe Text)
kmAWSAccountId = lens _kmAWSAccountId (\ s a -> s{_kmAWSAccountId = a});

-- | A value that specifies what operation(s) the key can perform.
kmKeyUsage :: Lens' KeyMetadata (Maybe KeyUsageType)
kmKeyUsage = lens _kmKeyUsage (\ s a -> s{_kmKeyUsage = a});

-- | Date the key was created.
kmCreationDate :: Lens' KeyMetadata (Maybe UTCTime)
kmCreationDate = lens _kmCreationDate (\ s a -> s{_kmCreationDate = a}) . mapping _Time;

-- | The description of the key.
kmDescription :: Lens' KeyMetadata (Maybe Text)
kmDescription = lens _kmDescription (\ s a -> s{_kmDescription = a});

-- | Unique identifier for the key.
kmKeyId :: Lens' KeyMetadata Text
kmKeyId = lens _kmKeyId (\ s a -> s{_kmKeyId = a});

instance FromJSON KeyMetadata where
        parseJSON
          = withObject "KeyMetadata"
              (\ x ->
                 KeyMetadata' <$>
                   (x .:? "Arn") <*> (x .:? "Enabled") <*>
                     (x .:? "AWSAccountId")
                     <*> (x .:? "KeyUsage")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Description")
                     <*> (x .: "KeyId"))
