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
    , gleRetiringPrincipal
    , gleIssuingAccount
    , gleGrantId
    , gleConstraints
    , gleGranteePrincipal
    , gleOperations

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
    , kmARN
    , kmEnabled
    , kmAWSAccountId
    , kmKeyUsage
    , kmCreationDate
    , kmDescription
    , kmKeyId

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
data AliasListEntry = AliasListEntry'{_aleTargetKeyId :: Maybe Text, _aleAliasName :: Maybe Text, _aleAliasARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AliasListEntry' smart constructor.
aliasListEntry :: AliasListEntry
aliasListEntry = AliasListEntry'{_aleTargetKeyId = Nothing, _aleAliasName = Nothing, _aleAliasARN = Nothing};

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
                   x .:? "TargetKeyId" <*> x .:? "AliasName" <*>
                     x .:? "AliasArn")

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
data GrantConstraints = GrantConstraints'{_gcEncryptionContextEquals :: Maybe (HashMap Text Text), _gcEncryptionContextSubset :: Maybe (HashMap Text Text)} deriving (Eq, Read, Show)

-- | 'GrantConstraints' smart constructor.
grantConstraints :: GrantConstraints
grantConstraints = GrantConstraints'{_gcEncryptionContextEquals = Nothing, _gcEncryptionContextSubset = Nothing};

-- | The constraint contains additional key\/value pairs that serve to
-- further limit the grant.
gcEncryptionContextEquals :: Lens' GrantConstraints (Maybe (HashMap Text Text))
gcEncryptionContextEquals = lens _gcEncryptionContextEquals (\ s a -> s{_gcEncryptionContextEquals = a}) . mapping _Coerce;

-- | The constraint equals the full encryption context.
gcEncryptionContextSubset :: Lens' GrantConstraints (Maybe (HashMap Text Text))
gcEncryptionContextSubset = lens _gcEncryptionContextSubset (\ s a -> s{_gcEncryptionContextSubset = a}) . mapping _Coerce;

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
data GrantListEntry = GrantListEntry'{_gleRetiringPrincipal :: Maybe Text, _gleIssuingAccount :: Maybe Text, _gleGrantId :: Maybe Text, _gleConstraints :: Maybe GrantConstraints, _gleGranteePrincipal :: Maybe Text, _gleOperations :: Maybe [GrantOperation]} deriving (Eq, Read, Show)

-- | 'GrantListEntry' smart constructor.
grantListEntry :: GrantListEntry
grantListEntry = GrantListEntry'{_gleRetiringPrincipal = Nothing, _gleIssuingAccount = Nothing, _gleGrantId = Nothing, _gleConstraints = Nothing, _gleGranteePrincipal = Nothing, _gleOperations = Nothing};

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
gleOperations :: Lens' GrantListEntry (Maybe [GrantOperation])
gleOperations = lens _gleOperations (\ s a -> s{_gleOperations = a});

instance FromJSON GrantListEntry where
        parseJSON
          = withObject "GrantListEntry"
              (\ x ->
                 GrantListEntry' <$>
                   x .:? "RetiringPrincipal" <*> x .:? "IssuingAccount"
                     <*> x .:? "GrantId"
                     <*> x .:? "Constraints"
                     <*> x .:? "GranteePrincipal"
                     <*> x .:? "Operations" .!= mempty)

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
data KeyListEntry = KeyListEntry'{_kleKeyARN :: Maybe Text, _kleKeyId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'KeyListEntry' smart constructor.
keyListEntry :: KeyListEntry
keyListEntry = KeyListEntry'{_kleKeyARN = Nothing, _kleKeyId = Nothing};

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
                 KeyListEntry' <$> x .:? "KeyArn" <*> x .:? "KeyId")

-- | /See:/ 'keyMetadata' smart constructor.
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
data KeyMetadata = KeyMetadata'{_kmARN :: Maybe Text, _kmEnabled :: Maybe Bool, _kmAWSAccountId :: Maybe Text, _kmKeyUsage :: Maybe KeyUsageType, _kmCreationDate :: Maybe POSIX, _kmDescription :: Maybe Text, _kmKeyId :: Text} deriving (Eq, Read, Show)

-- | 'KeyMetadata' smart constructor.
keyMetadata :: Text -> KeyMetadata
keyMetadata pKeyId = KeyMetadata'{_kmARN = Nothing, _kmEnabled = Nothing, _kmAWSAccountId = Nothing, _kmKeyUsage = Nothing, _kmCreationDate = Nothing, _kmDescription = Nothing, _kmKeyId = pKeyId};

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
                   x .:? "Arn" <*> x .:? "Enabled" <*>
                     x .:? "AWSAccountId"
                     <*> x .:? "KeyUsage"
                     <*> x .:? "CreationDate"
                     <*> x .:? "Description"
                     <*> x .: "KeyId")

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
