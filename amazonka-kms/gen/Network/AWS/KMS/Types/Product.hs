{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.Product where

import           Network.AWS.KMS.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Contains information about an alias.
--
-- /See:/ 'aliasListEntry' smart constructor.
data AliasListEntry = AliasListEntry'
    { _aleTargetKeyId :: !(Maybe Text)
    , _aleAliasName   :: !(Maybe Text)
    , _aleAliasARN    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AliasListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aleTargetKeyId'
--
-- * 'aleAliasName'
--
-- * 'aleAliasARN'
aliasListEntry
    :: AliasListEntry
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

instance Hashable AliasListEntry

instance NFData AliasListEntry

-- | A structure for specifying the conditions under which the operations permitted by the grant are allowed.
--
-- You can use this structure to allow the operations permitted by the grant only when a specified encryption context is present. For more information about encryption context, see <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/.
--
-- /See:/ 'grantConstraints' smart constructor.
data GrantConstraints = GrantConstraints'
    { _gcEncryptionContextEquals :: !(Maybe (Map Text Text))
    , _gcEncryptionContextSubset :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GrantConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcEncryptionContextEquals'
--
-- * 'gcEncryptionContextSubset'
grantConstraints
    :: GrantConstraints
grantConstraints =
    GrantConstraints'
    { _gcEncryptionContextEquals = Nothing
    , _gcEncryptionContextSubset = Nothing
    }

-- | Contains a list of key-value pairs that must be present in the encryption context of a subsequent operation permitted by the grant. When a subsequent operation permitted by the grant includes an encryption context that matches this list, the grant allows the operation. Otherwise, the operation is not allowed.
gcEncryptionContextEquals :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextEquals = lens _gcEncryptionContextEquals (\ s a -> s{_gcEncryptionContextEquals = a}) . _Default . _Map;

-- | Contains a list of key-value pairs, a subset of which must be present in the encryption context of a subsequent operation permitted by the grant. When a subsequent operation permitted by the grant includes an encryption context that matches this list or is a subset of this list, the grant allows the operation. Otherwise, the operation is not allowed.
gcEncryptionContextSubset :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextSubset = lens _gcEncryptionContextSubset (\ s a -> s{_gcEncryptionContextSubset = a}) . _Default . _Map;

instance FromJSON GrantConstraints where
        parseJSON
          = withObject "GrantConstraints"
              (\ x ->
                 GrantConstraints' <$>
                   (x .:? "EncryptionContextEquals" .!= mempty) <*>
                     (x .:? "EncryptionContextSubset" .!= mempty))

instance Hashable GrantConstraints

instance NFData GrantConstraints

instance ToJSON GrantConstraints where
        toJSON GrantConstraints'{..}
          = object
              (catMaybes
                 [("EncryptionContextEquals" .=) <$>
                    _gcEncryptionContextEquals,
                  ("EncryptionContextSubset" .=) <$>
                    _gcEncryptionContextSubset])

-- | Contains information about an entry in a list of grants.
--
-- /See:/ 'grantListEntry' smart constructor.
data GrantListEntry = GrantListEntry'
    { _gleKeyId             :: !(Maybe Text)
    , _gleRetiringPrincipal :: !(Maybe Text)
    , _gleIssuingAccount    :: !(Maybe Text)
    , _gleGrantId           :: !(Maybe Text)
    , _gleConstraints       :: !(Maybe GrantConstraints)
    , _gleGranteePrincipal  :: !(Maybe Text)
    , _gleName              :: !(Maybe Text)
    , _gleCreationDate      :: !(Maybe POSIX)
    , _gleOperations        :: !(Maybe [GrantOperation])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GrantListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gleKeyId'
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
-- * 'gleName'
--
-- * 'gleCreationDate'
--
-- * 'gleOperations'
grantListEntry
    :: GrantListEntry
grantListEntry =
    GrantListEntry'
    { _gleKeyId = Nothing
    , _gleRetiringPrincipal = Nothing
    , _gleIssuingAccount = Nothing
    , _gleGrantId = Nothing
    , _gleConstraints = Nothing
    , _gleGranteePrincipal = Nothing
    , _gleName = Nothing
    , _gleCreationDate = Nothing
    , _gleOperations = Nothing
    }

-- | The unique identifier for the customer master key (CMK) to which the grant applies.
gleKeyId :: Lens' GrantListEntry (Maybe Text)
gleKeyId = lens _gleKeyId (\ s a -> s{_gleKeyId = a});

-- | The principal that can retire the grant.
gleRetiringPrincipal :: Lens' GrantListEntry (Maybe Text)
gleRetiringPrincipal = lens _gleRetiringPrincipal (\ s a -> s{_gleRetiringPrincipal = a});

-- | The AWS account under which the grant was issued.
gleIssuingAccount :: Lens' GrantListEntry (Maybe Text)
gleIssuingAccount = lens _gleIssuingAccount (\ s a -> s{_gleIssuingAccount = a});

-- | The unique identifier for the grant.
gleGrantId :: Lens' GrantListEntry (Maybe Text)
gleGrantId = lens _gleGrantId (\ s a -> s{_gleGrantId = a});

-- | The conditions under which the grant\'s operations are allowed.
gleConstraints :: Lens' GrantListEntry (Maybe GrantConstraints)
gleConstraints = lens _gleConstraints (\ s a -> s{_gleConstraints = a});

-- | The principal that receives the grant\'s permissions.
gleGranteePrincipal :: Lens' GrantListEntry (Maybe Text)
gleGranteePrincipal = lens _gleGranteePrincipal (\ s a -> s{_gleGranteePrincipal = a});

-- | The friendly name that identifies the grant. If a name was provided in the < CreateGrant> request, that name is returned. Otherwise this value is null.
gleName :: Lens' GrantListEntry (Maybe Text)
gleName = lens _gleName (\ s a -> s{_gleName = a});

-- | The date and time when the grant was created.
gleCreationDate :: Lens' GrantListEntry (Maybe UTCTime)
gleCreationDate = lens _gleCreationDate (\ s a -> s{_gleCreationDate = a}) . mapping _Time;

-- | The list of operations permitted by the grant.
gleOperations :: Lens' GrantListEntry [GrantOperation]
gleOperations = lens _gleOperations (\ s a -> s{_gleOperations = a}) . _Default . _Coerce;

instance FromJSON GrantListEntry where
        parseJSON
          = withObject "GrantListEntry"
              (\ x ->
                 GrantListEntry' <$>
                   (x .:? "KeyId") <*> (x .:? "RetiringPrincipal") <*>
                     (x .:? "IssuingAccount")
                     <*> (x .:? "GrantId")
                     <*> (x .:? "Constraints")
                     <*> (x .:? "GranteePrincipal")
                     <*> (x .:? "Name")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Operations" .!= mempty))

instance Hashable GrantListEntry

instance NFData GrantListEntry

-- | Contains information about each entry in the key list.
--
-- /See:/ 'keyListEntry' smart constructor.
data KeyListEntry = KeyListEntry'
    { _kleKeyId  :: !(Maybe Text)
    , _kleKeyARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kleKeyId'
--
-- * 'kleKeyARN'
keyListEntry
    :: KeyListEntry
keyListEntry =
    KeyListEntry'
    { _kleKeyId = Nothing
    , _kleKeyARN = Nothing
    }

-- | Unique identifier of the key.
kleKeyId :: Lens' KeyListEntry (Maybe Text)
kleKeyId = lens _kleKeyId (\ s a -> s{_kleKeyId = a});

-- | ARN of the key.
kleKeyARN :: Lens' KeyListEntry (Maybe Text)
kleKeyARN = lens _kleKeyARN (\ s a -> s{_kleKeyARN = a});

instance FromJSON KeyListEntry where
        parseJSON
          = withObject "KeyListEntry"
              (\ x ->
                 KeyListEntry' <$>
                   (x .:? "KeyId") <*> (x .:? "KeyArn"))

instance Hashable KeyListEntry

instance NFData KeyListEntry

-- | Contains metadata about a customer master key (CMK).
--
-- This data type is used as a response element for the < CreateKey> and < DescribeKey> operations.
--
-- /See:/ 'keyMetadata' smart constructor.
data KeyMetadata = KeyMetadata'
    { _kmEnabled      :: !(Maybe Bool)
    , _kmARN          :: !(Maybe Text)
    , _kmKeyState     :: !(Maybe KeyState)
    , _kmAWSAccountId :: !(Maybe Text)
    , _kmKeyUsage     :: !(Maybe KeyUsageType)
    , _kmCreationDate :: !(Maybe POSIX)
    , _kmDeletionDate :: !(Maybe POSIX)
    , _kmDescription  :: !(Maybe Text)
    , _kmKeyId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kmEnabled'
--
-- * 'kmARN'
--
-- * 'kmKeyState'
--
-- * 'kmAWSAccountId'
--
-- * 'kmKeyUsage'
--
-- * 'kmCreationDate'
--
-- * 'kmDeletionDate'
--
-- * 'kmDescription'
--
-- * 'kmKeyId'
keyMetadata
    :: Text -- ^ 'kmKeyId'
    -> KeyMetadata
keyMetadata pKeyId_ =
    KeyMetadata'
    { _kmEnabled = Nothing
    , _kmARN = Nothing
    , _kmKeyState = Nothing
    , _kmAWSAccountId = Nothing
    , _kmKeyUsage = Nothing
    , _kmCreationDate = Nothing
    , _kmDeletionDate = Nothing
    , _kmDescription = Nothing
    , _kmKeyId = pKeyId_
    }

-- | Specifies whether the key is enabled. When 'KeyState' is 'Enabled' this value is true, otherwise it is false.
kmEnabled :: Lens' KeyMetadata (Maybe Bool)
kmEnabled = lens _kmEnabled (\ s a -> s{_kmEnabled = a});

-- | The Amazon Resource Name (ARN) of the key. For examples, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/.
kmARN :: Lens' KeyMetadata (Maybe Text)
kmARN = lens _kmARN (\ s a -> s{_kmARN = a});

-- | The state of the customer master key (CMK).
--
-- For more information about how key state affects the use of a CMK, go to <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/.
kmKeyState :: Lens' KeyMetadata (Maybe KeyState)
kmKeyState = lens _kmKeyState (\ s a -> s{_kmKeyState = a});

-- | The twelve-digit account ID of the AWS account that owns the key.
kmAWSAccountId :: Lens' KeyMetadata (Maybe Text)
kmAWSAccountId = lens _kmAWSAccountId (\ s a -> s{_kmAWSAccountId = a});

-- | The cryptographic operations for which you can use the key. Currently the only allowed value is 'ENCRYPT_DECRYPT', which means you can use the key for the < Encrypt> and < Decrypt> operations.
kmKeyUsage :: Lens' KeyMetadata (Maybe KeyUsageType)
kmKeyUsage = lens _kmKeyUsage (\ s a -> s{_kmKeyUsage = a});

-- | The date and time when the key was created.
kmCreationDate :: Lens' KeyMetadata (Maybe UTCTime)
kmCreationDate = lens _kmCreationDate (\ s a -> s{_kmCreationDate = a}) . mapping _Time;

-- | The date and time after which AWS KMS deletes the customer master key (CMK). This value is present only when 'KeyState' is 'PendingDeletion', otherwise this value is null.
kmDeletionDate :: Lens' KeyMetadata (Maybe UTCTime)
kmDeletionDate = lens _kmDeletionDate (\ s a -> s{_kmDeletionDate = a}) . mapping _Time;

-- | The friendly description of the key.
kmDescription :: Lens' KeyMetadata (Maybe Text)
kmDescription = lens _kmDescription (\ s a -> s{_kmDescription = a});

-- | The globally unique identifier for the key.
kmKeyId :: Lens' KeyMetadata Text
kmKeyId = lens _kmKeyId (\ s a -> s{_kmKeyId = a});

instance FromJSON KeyMetadata where
        parseJSON
          = withObject "KeyMetadata"
              (\ x ->
                 KeyMetadata' <$>
                   (x .:? "Enabled") <*> (x .:? "Arn") <*>
                     (x .:? "KeyState")
                     <*> (x .:? "AWSAccountId")
                     <*> (x .:? "KeyUsage")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "DeletionDate")
                     <*> (x .:? "Description")
                     <*> (x .: "KeyId"))

instance Hashable KeyMetadata

instance NFData KeyMetadata

-- | /See:/ 'listGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
    { _lgTruncated  :: !(Maybe Bool)
    , _lgGrants     :: !(Maybe [GrantListEntry])
    , _lgNextMarker :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGrantsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgTruncated'
--
-- * 'lgGrants'
--
-- * 'lgNextMarker'
listGrantsResponse
    :: ListGrantsResponse
listGrantsResponse =
    ListGrantsResponse'
    { _lgTruncated = Nothing
    , _lgGrants = Nothing
    , _lgNextMarker = Nothing
    }

-- | A flag that indicates whether there are more items in the list. If your results were truncated, you can use the 'Marker' parameter to make a subsequent pagination request to retrieve more items in the list.
lgTruncated :: Lens' ListGrantsResponse (Maybe Bool)
lgTruncated = lens _lgTruncated (\ s a -> s{_lgTruncated = a});

-- | A list of grants.
lgGrants :: Lens' ListGrantsResponse [GrantListEntry]
lgGrants = lens _lgGrants (\ s a -> s{_lgGrants = a}) . _Default . _Coerce;

-- | When 'Truncated' is true, this value is present and contains the value to use for the 'Marker' parameter in a subsequent pagination request.
lgNextMarker :: Lens' ListGrantsResponse (Maybe Text)
lgNextMarker = lens _lgNextMarker (\ s a -> s{_lgNextMarker = a});

instance FromJSON ListGrantsResponse where
        parseJSON
          = withObject "ListGrantsResponse"
              (\ x ->
                 ListGrantsResponse' <$>
                   (x .:? "Truncated") <*> (x .:? "Grants" .!= mempty)
                     <*> (x .:? "NextMarker"))

instance Hashable ListGrantsResponse

instance NFData ListGrantsResponse
