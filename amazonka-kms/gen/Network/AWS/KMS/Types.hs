{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Errors
    , JSONError

    -- * AliasListEntry
    , AliasListEntry
    , aliasListEntry
    , aleTargetKeyId
    , aleAliasName
    , aleAliasARN

    -- * DataKeySpec
    , DataKeySpec (..)

    -- * GrantConstraints
    , GrantConstraints
    , grantConstraints
    , gcEncryptionContextEquals
    , gcEncryptionContextSubset

    -- * GrantListEntry
    , GrantListEntry
    , grantListEntry
    , gleConstraints
    , gleOperations
    , gleRetiringPrincipal
    , gleIssuingAccount
    , gleGrantId
    , gleGranteePrincipal

    -- * GrantOperation
    , GrantOperation (..)

    -- * KeyListEntry
    , KeyListEntry
    , keyListEntry
    , kleKeyARN
    , kleKeyId

    -- * KeyMetadata
    , KeyMetadata
    , keyMetadata
    , kmEnabled
    , kmAWSAccountId
    , kmKeyUsage
    , kmCreationDate
    , kmDescription
    , kmKeyId
    , kmARN

    -- * KeyUsageType
    , KeyUsageType (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-11-01@ of the Amazon Key Management Service SDK.
data KMS

instance AWSService KMS where
    type Sg KMS = V4
    type Er KMS = JSONError

    service = service'
      where
        service' :: Service KMS
        service' = Service
            { _svcAbbrev  = "KMS"
            , _svcPrefix  = "kms"
            , _svcVersion = "2014-11-01"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry KMS
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'aliasListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aleTargetKeyId'
--
-- * 'aleAliasName'
--
-- * 'aleAliasARN'
data AliasListEntry = AliasListEntry'{_aleTargetKeyId :: Text, _aleAliasName :: Text, _aleAliasARN :: Text} deriving (Eq, Read, Show)

-- | 'AliasListEntry' smart constructor.
aliasListEntry :: Text -> Text -> Text -> AliasListEntry
aliasListEntry pTargetKeyId pAliasName pAliasARN = AliasListEntry'{_aleTargetKeyId = pTargetKeyId, _aleAliasName = pAliasName, _aleAliasARN = pAliasARN};

-- | String that contains the key identifier pointed to by the alias.
aleTargetKeyId :: Lens' AliasListEntry Text
aleTargetKeyId = lens _aleTargetKeyId (\ s a -> s{_aleTargetKeyId = a});

-- | String that contains the alias.
aleAliasName :: Lens' AliasListEntry Text
aleAliasName = lens _aleAliasName (\ s a -> s{_aleAliasName = a});

-- | String that contains the key ARN.
aleAliasARN :: Lens' AliasListEntry Text
aleAliasARN = lens _aleAliasARN (\ s a -> s{_aleAliasARN = a});

instance FromJSON AliasListEntry where
        parseJSON
          = withObject "AliasListEntry"
              (\ x ->
                 AliasListEntry' <$>
                   x .: "TargetKeyId" <*> x .: "AliasName" <*>
                     x .: "AliasArn")

data DataKeySpec = AES128 | AES256 deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DataKeySpec where
    parser = takeLowerText >>= \case
        "AES_128" -> pure AES128
        "AES_256" -> pure AES256
        e -> fail ("Failure parsing DataKeySpec from " ++ show e)

instance ToText DataKeySpec where
    toText = \case
        AES128 -> "AES_128"
        AES256 -> "AES_256"

instance Hashable DataKeySpec
instance ToQuery DataKeySpec
instance ToHeader DataKeySpec

instance ToJSON DataKeySpec where
    toJSON = toJSONText

-- | /See:/ 'grantConstraints' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcEncryptionContextEquals'
--
-- * 'gcEncryptionContextSubset'
data GrantConstraints = GrantConstraints'{_gcEncryptionContextEquals :: HashMap Text Text, _gcEncryptionContextSubset :: HashMap Text Text} deriving (Eq, Read, Show)

-- | 'GrantConstraints' smart constructor.
grantConstraints :: GrantConstraints
grantConstraints = GrantConstraints'{_gcEncryptionContextEquals = mempty, _gcEncryptionContextSubset = mempty};

-- | The constraint contains additional key\/value pairs that serve to
-- further limit the grant.
gcEncryptionContextEquals :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextEquals = lens _gcEncryptionContextEquals (\ s a -> s{_gcEncryptionContextEquals = a}) . _Coerce;

-- | The constraint equals the full encryption context.
gcEncryptionContextSubset :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextSubset = lens _gcEncryptionContextSubset (\ s a -> s{_gcEncryptionContextSubset = a}) . _Coerce;

instance FromJSON GrantConstraints where
        parseJSON
          = withObject "GrantConstraints"
              (\ x ->
                 GrantConstraints' <$>
                   x .:? "EncryptionContextEquals" .!= mempty <*>
                     x .:? "EncryptionContextSubset" .!= mempty)

instance ToJSON GrantConstraints where
        toJSON GrantConstraints'{..}
          = object
              ["EncryptionContextEquals" .=
                 _gcEncryptionContextEquals,
               "EncryptionContextSubset" .=
                 _gcEncryptionContextSubset]

-- | /See:/ 'grantListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gleConstraints'
--
-- * 'gleOperations'
--
-- * 'gleRetiringPrincipal'
--
-- * 'gleIssuingAccount'
--
-- * 'gleGrantId'
--
-- * 'gleGranteePrincipal'
data GrantListEntry = GrantListEntry'{_gleConstraints :: Maybe GrantConstraints, _gleOperations :: [GrantOperation], _gleRetiringPrincipal :: Text, _gleIssuingAccount :: Text, _gleGrantId :: Text, _gleGranteePrincipal :: Text} deriving (Eq, Read, Show)

-- | 'GrantListEntry' smart constructor.
grantListEntry :: Text -> Text -> Text -> Text -> GrantListEntry
grantListEntry pRetiringPrincipal pIssuingAccount pGrantId pGranteePrincipal = GrantListEntry'{_gleConstraints = Nothing, _gleOperations = mempty, _gleRetiringPrincipal = pRetiringPrincipal, _gleIssuingAccount = pIssuingAccount, _gleGrantId = pGrantId, _gleGranteePrincipal = pGranteePrincipal};

-- | Specifies the conditions under which the actions specified by the
-- @Operations@ parameter are allowed.
gleConstraints :: Lens' GrantListEntry (Maybe GrantConstraints)
gleConstraints = lens _gleConstraints (\ s a -> s{_gleConstraints = a});

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
gleOperations = lens _gleOperations (\ s a -> s{_gleOperations = a});

-- | The principal that can retire the account.
gleRetiringPrincipal :: Lens' GrantListEntry Text
gleRetiringPrincipal = lens _gleRetiringPrincipal (\ s a -> s{_gleRetiringPrincipal = a});

-- | The account under which the grant was issued.
gleIssuingAccount :: Lens' GrantListEntry Text
gleIssuingAccount = lens _gleIssuingAccount (\ s a -> s{_gleIssuingAccount = a});

-- | Unique grant identifier.
gleGrantId :: Lens' GrantListEntry Text
gleGrantId = lens _gleGrantId (\ s a -> s{_gleGrantId = a});

-- | The principal that receives the grant permission.
gleGranteePrincipal :: Lens' GrantListEntry Text
gleGranteePrincipal = lens _gleGranteePrincipal (\ s a -> s{_gleGranteePrincipal = a});

instance FromJSON GrantListEntry where
        parseJSON
          = withObject "GrantListEntry"
              (\ x ->
                 GrantListEntry' <$>
                   x .:? "Constraints" <*> x .:? "Operations" .!= mempty
                     <*> x .: "RetiringPrincipal"
                     <*> x .: "IssuingAccount"
                     <*> x .: "GrantId"
                     <*> x .: "GranteePrincipal")

data GrantOperation = Encrypt | GenerateDataKeyWithoutPlaintext | CreateGrant | RetireGrant | GenerateDataKey | Decrypt | ReEncryptTo | ReEncryptFrom deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText GrantOperation where
    parser = takeLowerText >>= \case
        "CreateGrant" -> pure CreateGrant
        "Decrypt" -> pure Decrypt
        "Encrypt" -> pure Encrypt
        "GenerateDataKey" -> pure GenerateDataKey
        "GenerateDataKeyWithoutPlaintext" -> pure GenerateDataKeyWithoutPlaintext
        "ReEncryptFrom" -> pure ReEncryptFrom
        "ReEncryptTo" -> pure ReEncryptTo
        "RetireGrant" -> pure RetireGrant
        e -> fail ("Failure parsing GrantOperation from " ++ show e)

instance ToText GrantOperation where
    toText = \case
        CreateGrant -> "CreateGrant"
        Decrypt -> "Decrypt"
        Encrypt -> "Encrypt"
        GenerateDataKey -> "GenerateDataKey"
        GenerateDataKeyWithoutPlaintext -> "GenerateDataKeyWithoutPlaintext"
        ReEncryptFrom -> "ReEncryptFrom"
        ReEncryptTo -> "ReEncryptTo"
        RetireGrant -> "RetireGrant"

instance Hashable GrantOperation
instance ToQuery GrantOperation
instance ToHeader GrantOperation

instance ToJSON GrantOperation where
    toJSON = toJSONText

instance FromJSON GrantOperation where
    parseJSON = parseJSONText "GrantOperation"

-- | /See:/ 'keyListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kleKeyARN'
--
-- * 'kleKeyId'
data KeyListEntry = KeyListEntry'{_kleKeyARN :: Text, _kleKeyId :: Text} deriving (Eq, Read, Show)

-- | 'KeyListEntry' smart constructor.
keyListEntry :: Text -> Text -> KeyListEntry
keyListEntry pKeyARN pKeyId = KeyListEntry'{_kleKeyARN = pKeyARN, _kleKeyId = pKeyId};

-- | ARN of the key.
kleKeyARN :: Lens' KeyListEntry Text
kleKeyARN = lens _kleKeyARN (\ s a -> s{_kleKeyARN = a});

-- | Unique identifier of the key.
kleKeyId :: Lens' KeyListEntry Text
kleKeyId = lens _kleKeyId (\ s a -> s{_kleKeyId = a});

instance FromJSON KeyListEntry where
        parseJSON
          = withObject "KeyListEntry"
              (\ x ->
                 KeyListEntry' <$> x .: "KeyArn" <*> x .: "KeyId")

-- | /See:/ 'keyMetadata' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
--
-- * 'kmARN'
data KeyMetadata = KeyMetadata'{_kmEnabled :: Maybe Bool, _kmAWSAccountId :: Maybe Text, _kmKeyUsage :: Maybe KeyUsageType, _kmCreationDate :: Maybe POSIX, _kmDescription :: Maybe Text, _kmKeyId :: Text, _kmARN :: Text} deriving (Eq, Read, Show)

-- | 'KeyMetadata' smart constructor.
keyMetadata :: Text -> Text -> KeyMetadata
keyMetadata pKeyId pARN = KeyMetadata'{_kmEnabled = Nothing, _kmAWSAccountId = Nothing, _kmKeyUsage = Nothing, _kmCreationDate = Nothing, _kmDescription = Nothing, _kmKeyId = pKeyId, _kmARN = pARN};

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

-- | Key ARN (Amazon Resource Name).
kmARN :: Lens' KeyMetadata Text
kmARN = lens _kmARN (\ s a -> s{_kmARN = a});

instance FromJSON KeyMetadata where
        parseJSON
          = withObject "KeyMetadata"
              (\ x ->
                 KeyMetadata' <$>
                   x .:? "Enabled" <*> x .:? "AWSAccountId" <*>
                     x .:? "KeyUsage"
                     <*> x .:? "CreationDate"
                     <*> x .:? "Description"
                     <*> x .: "KeyId"
                     <*> x .: "Arn")

data KeyUsageType = EncryptDecrypt deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText KeyUsageType where
    parser = takeLowerText >>= \case
        "ENCRYPT_DECRYPT" -> pure EncryptDecrypt
        e -> fail ("Failure parsing KeyUsageType from " ++ show e)

instance ToText KeyUsageType where
    toText = \case
        EncryptDecrypt -> "ENCRYPT_DECRYPT"

instance Hashable KeyUsageType
instance ToQuery KeyUsageType
instance ToHeader KeyUsageType

instance ToJSON KeyUsageType where
    toJSON = toJSONText

instance FromJSON KeyUsageType where
    parseJSON = parseJSONText "KeyUsageType"
