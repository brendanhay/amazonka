{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Error
    , JSONError

    -- * KeyUsageType
    , KeyUsageType (..)

    -- * KeyMetadata
    , KeyMetadata
    , keyMetadata
    , kmAWSAccountId
    , kmArn
    , kmCreationDate
    , kmDescription
    , kmEnabled
    , kmKeyId
    , kmKeyUsage

    -- * DataKeySpec
    , DataKeySpec (..)

    -- * GrantConstraints
    , GrantConstraints
    , grantConstraints
    , gcEncryptionContextEquals
    , gcEncryptionContextSubset

    -- * AliasListEntry
    , AliasListEntry
    , aliasListEntry
    , aleAliasArn
    , aleAliasName
    , aleTargetKeyId

    -- * GrantListEntry
    , GrantListEntry
    , grantListEntry
    , gleConstraints
    , gleGrantId
    , gleGranteePrincipal
    , gleIssuingAccount
    , gleOperations
    , gleRetiringPrincipal

    -- * GrantOperation
    , GrantOperation (..)

    -- * KeyListEntry
    , KeyListEntry
    , keyListEntry
    , kleKeyArn
    , kleKeyId
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-11-01@ of the Amazon Key Management Service service.
data KMS

instance AWSService KMS where
    type Sg KMS = V4
    type Er KMS = JSONError

    service = service'
      where
        service' :: Service KMS
        service' = Service
              { _svcAbbrev       = "KMS"
              , _svcPrefix       = "kms"
              , _svcVersion      = "2014-11-01"
              , _svcTargetPrefix = Just "TrentService"
              , _svcJSONVersion  = Just "1.1"
              , _svcDelay        = Exp 0.05 2 5
              , _svcHandle       = handle
              , _svcRetry        = retry
              }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Status
              -> JSONError
              -> Bool
        retry (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data KeyUsageType
    = EncryptDecrypt -- ^ ENCRYPT_DECRYPT
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable KeyUsageType

instance FromText KeyUsageType where
    parser = takeText >>= \case
        "ENCRYPT_DECRYPT" -> pure EncryptDecrypt
        e                 -> fail $
            "Failure parsing KeyUsageType from " ++ show e

instance ToText KeyUsageType where
    toText EncryptDecrypt = "ENCRYPT_DECRYPT"

instance ToByteString KeyUsageType
instance ToHeader     KeyUsageType
instance ToQuery      KeyUsageType

instance FromJSON KeyUsageType where
    parseJSON = parseJSONText "KeyUsageType"

instance ToJSON KeyUsageType where
    toJSON = toJSONText

data KeyMetadata = KeyMetadata
    { _kmAWSAccountId :: Maybe Text
    , _kmArn          :: Maybe Text
    , _kmCreationDate :: Maybe ISO8601
    , _kmDescription  :: Maybe Text
    , _kmEnabled      :: Maybe Bool
    , _kmKeyId        :: Text
    , _kmKeyUsage     :: Maybe KeyUsageType
    } deriving (Eq, Show)

-- | 'KeyMetadata' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kmAWSAccountId' @::@ 'Maybe' 'Text'
--
-- * 'kmArn' @::@ 'Maybe' 'Text'
--
-- * 'kmCreationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'kmDescription' @::@ 'Maybe' 'Text'
--
-- * 'kmEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'kmKeyId' @::@ 'Text'
--
-- * 'kmKeyUsage' @::@ 'Maybe' 'KeyUsageType'
--
keyMetadata :: Text -- ^ 'kmKeyId'
            -> KeyMetadata
keyMetadata p1 = KeyMetadata
    { _kmKeyId        = p1
    , _kmAWSAccountId = Nothing
    , _kmArn          = Nothing
    , _kmCreationDate = Nothing
    , _kmEnabled      = Nothing
    , _kmDescription  = Nothing
    , _kmKeyUsage     = Nothing
    }

-- | Account ID number.
kmAWSAccountId :: Lens' KeyMetadata (Maybe Text)
kmAWSAccountId = lens _kmAWSAccountId (\s a -> s { _kmAWSAccountId = a })

-- | Key ARN (Amazon Resource Name).
kmArn :: Lens' KeyMetadata (Maybe Text)
kmArn = lens _kmArn (\s a -> s { _kmArn = a })

-- | Date the key was created.
kmCreationDate :: Lens' KeyMetadata (Maybe UTCTime)
kmCreationDate = lens _kmCreationDate (\s a -> s { _kmCreationDate = a }) . mapping _Time

-- | The description of the key.
kmDescription :: Lens' KeyMetadata (Maybe Text)
kmDescription = lens _kmDescription (\s a -> s { _kmDescription = a })

-- | Value that specifies whether the key is enabled.
kmEnabled :: Lens' KeyMetadata (Maybe Bool)
kmEnabled = lens _kmEnabled (\s a -> s { _kmEnabled = a })

-- | Unique identifier for the key.
kmKeyId :: Lens' KeyMetadata Text
kmKeyId = lens _kmKeyId (\s a -> s { _kmKeyId = a })

-- | A value that specifies what operation(s) the key can perform.
kmKeyUsage :: Lens' KeyMetadata (Maybe KeyUsageType)
kmKeyUsage = lens _kmKeyUsage (\s a -> s { _kmKeyUsage = a })

instance FromJSON KeyMetadata where
    parseJSON = withObject "KeyMetadata" $ \o -> KeyMetadata
        <$> o .:? "AWSAccountId"
        <*> o .:? "Arn"
        <*> o .:? "CreationDate"
        <*> o .:? "Description"
        <*> o .:? "Enabled"
        <*> o .:  "KeyId"
        <*> o .:? "KeyUsage"

instance ToJSON KeyMetadata where
    toJSON KeyMetadata{..} = object
        [ "AWSAccountId" .= _kmAWSAccountId
        , "KeyId"        .= _kmKeyId
        , "Arn"          .= _kmArn
        , "CreationDate" .= _kmCreationDate
        , "Enabled"      .= _kmEnabled
        , "Description"  .= _kmDescription
        , "KeyUsage"     .= _kmKeyUsage
        ]

data DataKeySpec
    = AES128 -- ^ AES_128
    | AES256 -- ^ AES_256
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DataKeySpec

instance FromText DataKeySpec where
    parser = takeText >>= \case
        "AES_128" -> pure AES128
        "AES_256" -> pure AES256
        e         -> fail $
            "Failure parsing DataKeySpec from " ++ show e

instance ToText DataKeySpec where
    toText = \case
        AES128 -> "AES_128"
        AES256 -> "AES_256"

instance ToByteString DataKeySpec
instance ToHeader     DataKeySpec
instance ToQuery      DataKeySpec

instance FromJSON DataKeySpec where
    parseJSON = parseJSONText "DataKeySpec"

instance ToJSON DataKeySpec where
    toJSON = toJSONText

data GrantConstraints = GrantConstraints
    { _gcEncryptionContextEquals :: Map Text Text
    , _gcEncryptionContextSubset :: Map Text Text
    } deriving (Eq, Show)

-- | 'GrantConstraints' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcEncryptionContextEquals' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'gcEncryptionContextSubset' @::@ 'HashMap' 'Text' 'Text'
--
grantConstraints :: GrantConstraints
grantConstraints = GrantConstraints
    { _gcEncryptionContextSubset = mempty
    , _gcEncryptionContextEquals = mempty
    }

-- | The constraint contains additional key/value pairs that serve to further
-- limit the grant.
gcEncryptionContextEquals :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextEquals =
    lens _gcEncryptionContextEquals
        (\s a -> s { _gcEncryptionContextEquals = a })
            . _Map

-- | The constraint equals the full encryption context.
gcEncryptionContextSubset :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextSubset =
    lens _gcEncryptionContextSubset
        (\s a -> s { _gcEncryptionContextSubset = a })
            . _Map

instance FromJSON GrantConstraints where
    parseJSON = withObject "GrantConstraints" $ \o -> GrantConstraints
        <$> o .:? "EncryptionContextEquals" .!= mempty
        <*> o .:? "EncryptionContextSubset" .!= mempty

instance ToJSON GrantConstraints where
    toJSON GrantConstraints{..} = object
        [ "EncryptionContextSubset" .= _gcEncryptionContextSubset
        , "EncryptionContextEquals" .= _gcEncryptionContextEquals
        ]

data AliasListEntry = AliasListEntry
    { _aleAliasArn    :: Maybe Text
    , _aleAliasName   :: Maybe Text
    , _aleTargetKeyId :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'AliasListEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aleAliasArn' @::@ 'Maybe' 'Text'
--
-- * 'aleAliasName' @::@ 'Maybe' 'Text'
--
-- * 'aleTargetKeyId' @::@ 'Maybe' 'Text'
--
aliasListEntry :: AliasListEntry
aliasListEntry = AliasListEntry
    { _aleAliasName   = Nothing
    , _aleAliasArn    = Nothing
    , _aleTargetKeyId = Nothing
    }

-- | String that contains the key ARN.
aleAliasArn :: Lens' AliasListEntry (Maybe Text)
aleAliasArn = lens _aleAliasArn (\s a -> s { _aleAliasArn = a })

-- | String that contains the alias.
aleAliasName :: Lens' AliasListEntry (Maybe Text)
aleAliasName = lens _aleAliasName (\s a -> s { _aleAliasName = a })

-- | String that contains the key identifier pointed to by the alias.
aleTargetKeyId :: Lens' AliasListEntry (Maybe Text)
aleTargetKeyId = lens _aleTargetKeyId (\s a -> s { _aleTargetKeyId = a })

instance FromJSON AliasListEntry where
    parseJSON = withObject "AliasListEntry" $ \o -> AliasListEntry
        <$> o .:? "AliasArn"
        <*> o .:? "AliasName"
        <*> o .:? "TargetKeyId"

instance ToJSON AliasListEntry where
    toJSON AliasListEntry{..} = object
        [ "AliasName"   .= _aleAliasName
        , "AliasArn"    .= _aleAliasArn
        , "TargetKeyId" .= _aleTargetKeyId
        ]

data GrantListEntry = GrantListEntry
    { _gleConstraints       :: Maybe GrantConstraints
    , _gleGrantId           :: Maybe Text
    , _gleGranteePrincipal  :: Maybe Text
    , _gleIssuingAccount    :: Maybe Text
    , _gleOperations        :: List "Operations" GrantOperation
    , _gleRetiringPrincipal :: Maybe Text
    } deriving (Eq, Show)

-- | 'GrantListEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gleConstraints' @::@ 'Maybe' 'GrantConstraints'
--
-- * 'gleGrantId' @::@ 'Maybe' 'Text'
--
-- * 'gleGranteePrincipal' @::@ 'Maybe' 'Text'
--
-- * 'gleIssuingAccount' @::@ 'Maybe' 'Text'
--
-- * 'gleOperations' @::@ ['GrantOperation']
--
-- * 'gleRetiringPrincipal' @::@ 'Maybe' 'Text'
--
grantListEntry :: GrantListEntry
grantListEntry = GrantListEntry
    { _gleGrantId           = Nothing
    , _gleGranteePrincipal  = Nothing
    , _gleRetiringPrincipal = Nothing
    , _gleIssuingAccount    = Nothing
    , _gleOperations        = mempty
    , _gleConstraints       = Nothing
    }

-- | Specifies the conditions under which the actions specified by the 'Operations'
-- parameter are allowed.
gleConstraints :: Lens' GrantListEntry (Maybe GrantConstraints)
gleConstraints = lens _gleConstraints (\s a -> s { _gleConstraints = a })

-- | Unique grant identifier.
gleGrantId :: Lens' GrantListEntry (Maybe Text)
gleGrantId = lens _gleGrantId (\s a -> s { _gleGrantId = a })

-- | The principal that receives the grant permission.
gleGranteePrincipal :: Lens' GrantListEntry (Maybe Text)
gleGranteePrincipal =
    lens _gleGranteePrincipal (\s a -> s { _gleGranteePrincipal = a })

-- | The account under which the grant was issued.
gleIssuingAccount :: Lens' GrantListEntry (Maybe Text)
gleIssuingAccount =
    lens _gleIssuingAccount (\s a -> s { _gleIssuingAccount = a })

-- | List of operations permitted by the grant. This can be any combination of one
-- or more of the following values:  Decrypt Encrypt GenerateDataKey GenerateDataKeyWithoutPlaintext
-- ReEncryptFrom ReEncryptTo CreateGrant
gleOperations :: Lens' GrantListEntry [GrantOperation]
gleOperations = lens _gleOperations (\s a -> s { _gleOperations = a }) . _List

-- | The principal that can retire the account.
gleRetiringPrincipal :: Lens' GrantListEntry (Maybe Text)
gleRetiringPrincipal =
    lens _gleRetiringPrincipal (\s a -> s { _gleRetiringPrincipal = a })

instance FromJSON GrantListEntry where
    parseJSON = withObject "GrantListEntry" $ \o -> GrantListEntry
        <$> o .:? "Constraints"
        <*> o .:? "GrantId"
        <*> o .:? "GranteePrincipal"
        <*> o .:? "IssuingAccount"
        <*> o .:? "Operations" .!= mempty
        <*> o .:? "RetiringPrincipal"

instance ToJSON GrantListEntry where
    toJSON GrantListEntry{..} = object
        [ "GrantId"           .= _gleGrantId
        , "GranteePrincipal"  .= _gleGranteePrincipal
        , "RetiringPrincipal" .= _gleRetiringPrincipal
        , "IssuingAccount"    .= _gleIssuingAccount
        , "Operations"        .= _gleOperations
        , "Constraints"       .= _gleConstraints
        ]

data GrantOperation
    = GOCreateGrant                     -- ^ CreateGrant
    | GODecrypt                         -- ^ Decrypt
    | GOEncrypt                         -- ^ Encrypt
    | GOGenerateDataKey                 -- ^ GenerateDataKey
    | GOGenerateDataKeyWithoutPlaintext -- ^ GenerateDataKeyWithoutPlaintext
    | GOReEncryptFrom                   -- ^ ReEncryptFrom
    | GOReEncryptTo                     -- ^ ReEncryptTo
    | GORetireGrant                     -- ^ RetireGrant
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable GrantOperation

instance FromText GrantOperation where
    parser = takeText >>= \case
        "CreateGrant"                     -> pure GOCreateGrant
        "Decrypt"                         -> pure GODecrypt
        "Encrypt"                         -> pure GOEncrypt
        "GenerateDataKey"                 -> pure GOGenerateDataKey
        "GenerateDataKeyWithoutPlaintext" -> pure GOGenerateDataKeyWithoutPlaintext
        "ReEncryptFrom"                   -> pure GOReEncryptFrom
        "ReEncryptTo"                     -> pure GOReEncryptTo
        "RetireGrant"                     -> pure GORetireGrant
        e                                 -> fail $
            "Failure parsing GrantOperation from " ++ show e

instance ToText GrantOperation where
    toText = \case
        GOCreateGrant                     -> "CreateGrant"
        GODecrypt                         -> "Decrypt"
        GOEncrypt                         -> "Encrypt"
        GOGenerateDataKey                 -> "GenerateDataKey"
        GOGenerateDataKeyWithoutPlaintext -> "GenerateDataKeyWithoutPlaintext"
        GOReEncryptFrom                   -> "ReEncryptFrom"
        GOReEncryptTo                     -> "ReEncryptTo"
        GORetireGrant                     -> "RetireGrant"

instance ToByteString GrantOperation
instance ToHeader     GrantOperation
instance ToQuery      GrantOperation

instance FromJSON GrantOperation where
    parseJSON = parseJSONText "GrantOperation"

instance ToJSON GrantOperation where
    toJSON = toJSONText

data KeyListEntry = KeyListEntry
    { _kleKeyArn :: Maybe Text
    , _kleKeyId  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'KeyListEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kleKeyArn' @::@ 'Maybe' 'Text'
--
-- * 'kleKeyId' @::@ 'Maybe' 'Text'
--
keyListEntry :: KeyListEntry
keyListEntry = KeyListEntry
    { _kleKeyId  = Nothing
    , _kleKeyArn = Nothing
    }

-- | ARN of the key.
kleKeyArn :: Lens' KeyListEntry (Maybe Text)
kleKeyArn = lens _kleKeyArn (\s a -> s { _kleKeyArn = a })

-- | Unique identifier of the key.
kleKeyId :: Lens' KeyListEntry (Maybe Text)
kleKeyId = lens _kleKeyId (\s a -> s { _kleKeyId = a })

instance FromJSON KeyListEntry where
    parseJSON = withObject "KeyListEntry" $ \o -> KeyListEntry
        <$> o .:? "KeyArn"
        <*> o .:? "KeyId"

instance ToJSON KeyListEntry where
    toJSON KeyListEntry{..} = object
        [ "KeyId"  .= _kleKeyId
        , "KeyArn" .= _kleKeyArn
        ]
