{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

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

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Supported version (@2014-11-01@) of the Amazon Key Management Service.
data KMS deriving (Typeable)

instance AWSService KMS where
    type Sg KMS = V4
    type Er KMS = JSONError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "KMS"
        , _svcPrefix   = "kms"
        , _svcVersion  = "2014-11-01"
        , _svcTarget   = Nothing
        }

    handle = jsonError alwaysFail

data KeyUsageType
    = EncryptDecrypt -- ^ ENCRYPT_DECRYPT
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable KeyUsageType

instance FromText KeyUsageType where
    parser = match "ENCRYPT_DECRYPT" EncryptDecrypt

instance ToText KeyUsageType where
    toText EncryptDecrypt = "ENCRYPT_DECRYPT"

data KeyMetadata = KeyMetadata
    { _kmAWSAccountId :: Maybe Text
    , _kmArn          :: Maybe Text
    , _kmCreationDate :: Maybe RFC822
    , _kmDescription  :: Maybe Text
    , _kmEnabled      :: Maybe Bool
    , _kmKeyId        :: Text
    , _kmKeyUsage     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

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
-- * 'kmKeyUsage' @::@ 'Maybe' 'Text'
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
kmCreationDate = lens _kmCreationDate (\s a -> s { _kmCreationDate = a })
    . mapping _Time

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
kmKeyUsage :: Lens' KeyMetadata (Maybe Text)
kmKeyUsage = lens _kmKeyUsage (\s a -> s { _kmKeyUsage = a })

data DataKeySpec
    = AES128 -- ^ AES_128
    | AES256 -- ^ AES_256
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DataKeySpec

instance FromText DataKeySpec where
    parser = match "AES_128" AES128
         <|> match "AES_256" AES256

instance ToText DataKeySpec where
    toText = \case
        AES128 -> "AES_128"
        AES256 -> "AES_256"

data GrantConstraints = GrantConstraints
    { _gcEncryptionContextEquals :: Map Text Text
    , _gcEncryptionContextSubset :: Map Text Text
    } deriving (Eq, Show, Generic)

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

data AliasListEntry = AliasListEntry
    { _aleAliasArn    :: Maybe Text
    , _aleAliasName   :: Maybe Text
    , _aleTargetKeyId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

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

data GrantListEntry = GrantListEntry
    { _gleConstraints       :: Maybe GrantConstraints
    , _gleGrantId           :: Maybe Text
    , _gleGranteePrincipal  :: Maybe Text
    , _gleIssuingAccount    :: Maybe Text
    , _gleOperations        :: [Text]
    , _gleRetiringPrincipal :: Maybe Text
    } deriving (Eq, Show, Generic)

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
-- * 'gleOperations' @::@ ['Text']
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

-- | Specifies the conditions under which the actions specified by the
-- Operations parameter are allowed.
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

-- | List of operations permitted by the grant. This can be any combination of
-- one or more of the following values: Decrypt Encrypt GenerateDataKey
-- GenerateDataKeyWithoutPlaintext ReEncryptFrom ReEncryptTo CreateGrant.
gleOperations :: Lens' GrantListEntry [Text]
gleOperations = lens _gleOperations (\s a -> s { _gleOperations = a })

-- | The principal that can retire the account.
gleRetiringPrincipal :: Lens' GrantListEntry (Maybe Text)
gleRetiringPrincipal =
    lens _gleRetiringPrincipal (\s a -> s { _gleRetiringPrincipal = a })

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
    parser = match "CreateGrant"                     GOCreateGrant
         <|> match "Decrypt"                         GODecrypt
         <|> match "Encrypt"                         GOEncrypt
         <|> match "GenerateDataKey"                 GOGenerateDataKey
         <|> match "GenerateDataKeyWithoutPlaintext" GOGenerateDataKeyWithoutPlaintext
         <|> match "ReEncryptFrom"                   GOReEncryptFrom
         <|> match "ReEncryptTo"                     GOReEncryptTo
         <|> match "RetireGrant"                     GORetireGrant

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

data KeyListEntry = KeyListEntry
    { _kleKeyArn :: Maybe Text
    , _kleKeyId  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

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
