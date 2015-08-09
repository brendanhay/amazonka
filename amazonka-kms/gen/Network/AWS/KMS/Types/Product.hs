{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.Product where

import Network.AWS.KMS.Types.Sum
import Network.AWS.Prelude

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
    , _aleAliasName :: !(Maybe Text)
    , _aleAliasARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    , _gleIssuingAccount :: !(Maybe Text)
    , _gleGrantId :: !(Maybe Text)
    , _gleConstraints :: !(Maybe GrantConstraints)
    , _gleGranteePrincipal :: !(Maybe Text)
    , _gleOperations :: !(Maybe [GrantOperation])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
gleOperations = lens _gleOperations (\ s a -> s{_gleOperations = a}) . _Default . _Coerce;

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
    , _kleKeyId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _kmARN :: !(Maybe Text)
    , _kmEnabled :: !(Maybe Bool)
    , _kmAWSAccountId :: !(Maybe Text)
    , _kmKeyUsage :: !(Maybe KeyUsageType)
    , _kmCreationDate :: !(Maybe POSIX)
    , _kmDescription :: !(Maybe Text)
    , _kmKeyId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'KeyMetadata' smart constructor.
keyMetadata :: Text -> KeyMetadata
keyMetadata pKeyId_ = 
    KeyMetadata'
    { _kmARN = Nothing
    , _kmEnabled = Nothing
    , _kmAWSAccountId = Nothing
    , _kmKeyUsage = Nothing
    , _kmCreationDate = Nothing
    , _kmDescription = Nothing
    , _kmKeyId = pKeyId_
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
