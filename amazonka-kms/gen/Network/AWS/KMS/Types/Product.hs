{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.Product where

import Network.AWS.KMS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an alias.
--
--
--
-- /See:/ 'aliasListEntry' smart constructor.
data AliasListEntry = AliasListEntry'
  { _aleTargetKeyId :: !(Maybe Text)
  , _aleAliasName   :: !(Maybe Text)
  , _aleAliasARN    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AliasListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aleTargetKeyId' - String that contains the key identifier referred to by the alias.
--
-- * 'aleAliasName' - String that contains the alias.
--
-- * 'aleAliasARN' - String that contains the key ARN.
aliasListEntry
    :: AliasListEntry
aliasListEntry =
  AliasListEntry'
    {_aleTargetKeyId = Nothing, _aleAliasName = Nothing, _aleAliasARN = Nothing}


-- | String that contains the key identifier referred to by the alias.
aleTargetKeyId :: Lens' AliasListEntry (Maybe Text)
aleTargetKeyId = lens _aleTargetKeyId (\ s a -> s{_aleTargetKeyId = a})

-- | String that contains the alias.
aleAliasName :: Lens' AliasListEntry (Maybe Text)
aleAliasName = lens _aleAliasName (\ s a -> s{_aleAliasName = a})

-- | String that contains the key ARN.
aleAliasARN :: Lens' AliasListEntry (Maybe Text)
aleAliasARN = lens _aleAliasARN (\ s a -> s{_aleAliasARN = a})

instance FromJSON AliasListEntry where
        parseJSON
          = withObject "AliasListEntry"
              (\ x ->
                 AliasListEntry' <$>
                   (x .:? "TargetKeyId") <*> (x .:? "AliasName") <*>
                     (x .:? "AliasArn"))

instance Hashable AliasListEntry where

instance NFData AliasListEntry where

-- | A structure that you can use to allow certain operations in the grant only when the desired encryption context is present. For more information about encryption context, see <http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
--
-- Grant constraints apply only to operations that accept encryption context as input. For example, the @'DescribeKey' @ operation does not accept encryption context as input. A grant that allows the @DescribeKey@ operation does so regardless of the grant constraints. In constrast, the @'Encrypt' @ operation accepts encryption context as input. A grant that allows the @Encrypt@ operation does so only when the encryption context of the @Encrypt@ operation satisfies the grant constraints.
--
--
-- /See:/ 'grantConstraints' smart constructor.
data GrantConstraints = GrantConstraints'
  { _gcEncryptionContextEquals :: !(Maybe (Map Text Text))
  , _gcEncryptionContextSubset :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GrantConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcEncryptionContextEquals' - A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows. When certain subsequent operations allowed by the grant include encryption context that matches this list, the grant allows the operation. Otherwise, the grant does not allow the operation.
--
-- * 'gcEncryptionContextSubset' - A list of key-value pairs, all of which must be present in the encryption context of certain subsequent operations that the grant allows. When certain subsequent operations allowed by the grant include encryption context that matches this list or is a superset of this list, the grant allows the operation. Otherwise, the grant does not allow the operation.
grantConstraints
    :: GrantConstraints
grantConstraints =
  GrantConstraints'
    {_gcEncryptionContextEquals = Nothing, _gcEncryptionContextSubset = Nothing}


-- | A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows. When certain subsequent operations allowed by the grant include encryption context that matches this list, the grant allows the operation. Otherwise, the grant does not allow the operation.
gcEncryptionContextEquals :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextEquals = lens _gcEncryptionContextEquals (\ s a -> s{_gcEncryptionContextEquals = a}) . _Default . _Map

-- | A list of key-value pairs, all of which must be present in the encryption context of certain subsequent operations that the grant allows. When certain subsequent operations allowed by the grant include encryption context that matches this list or is a superset of this list, the grant allows the operation. Otherwise, the grant does not allow the operation.
gcEncryptionContextSubset :: Lens' GrantConstraints (HashMap Text Text)
gcEncryptionContextSubset = lens _gcEncryptionContextSubset (\ s a -> s{_gcEncryptionContextSubset = a}) . _Default . _Map

instance FromJSON GrantConstraints where
        parseJSON
          = withObject "GrantConstraints"
              (\ x ->
                 GrantConstraints' <$>
                   (x .:? "EncryptionContextEquals" .!= mempty) <*>
                     (x .:? "EncryptionContextSubset" .!= mempty))

instance Hashable GrantConstraints where

instance NFData GrantConstraints where

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
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GrantListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gleKeyId' - The unique identifier for the customer master key (CMK) to which the grant applies.
--
-- * 'gleRetiringPrincipal' - The principal that can retire the grant.
--
-- * 'gleIssuingAccount' - The AWS account under which the grant was issued.
--
-- * 'gleGrantId' - The unique identifier for the grant.
--
-- * 'gleConstraints' - A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows.
--
-- * 'gleGranteePrincipal' - The principal that receives the grant's permissions.
--
-- * 'gleName' - The friendly name that identifies the grant. If a name was provided in the 'CreateGrant' request, that name is returned. Otherwise this value is null.
--
-- * 'gleCreationDate' - The date and time when the grant was created.
--
-- * 'gleOperations' - The list of operations permitted by the grant.
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
gleKeyId = lens _gleKeyId (\ s a -> s{_gleKeyId = a})

-- | The principal that can retire the grant.
gleRetiringPrincipal :: Lens' GrantListEntry (Maybe Text)
gleRetiringPrincipal = lens _gleRetiringPrincipal (\ s a -> s{_gleRetiringPrincipal = a})

-- | The AWS account under which the grant was issued.
gleIssuingAccount :: Lens' GrantListEntry (Maybe Text)
gleIssuingAccount = lens _gleIssuingAccount (\ s a -> s{_gleIssuingAccount = a})

-- | The unique identifier for the grant.
gleGrantId :: Lens' GrantListEntry (Maybe Text)
gleGrantId = lens _gleGrantId (\ s a -> s{_gleGrantId = a})

-- | A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows.
gleConstraints :: Lens' GrantListEntry (Maybe GrantConstraints)
gleConstraints = lens _gleConstraints (\ s a -> s{_gleConstraints = a})

-- | The principal that receives the grant's permissions.
gleGranteePrincipal :: Lens' GrantListEntry (Maybe Text)
gleGranteePrincipal = lens _gleGranteePrincipal (\ s a -> s{_gleGranteePrincipal = a})

-- | The friendly name that identifies the grant. If a name was provided in the 'CreateGrant' request, that name is returned. Otherwise this value is null.
gleName :: Lens' GrantListEntry (Maybe Text)
gleName = lens _gleName (\ s a -> s{_gleName = a})

-- | The date and time when the grant was created.
gleCreationDate :: Lens' GrantListEntry (Maybe UTCTime)
gleCreationDate = lens _gleCreationDate (\ s a -> s{_gleCreationDate = a}) . mapping _Time

-- | The list of operations permitted by the grant.
gleOperations :: Lens' GrantListEntry [GrantOperation]
gleOperations = lens _gleOperations (\ s a -> s{_gleOperations = a}) . _Default . _Coerce

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

instance Hashable GrantListEntry where

instance NFData GrantListEntry where

-- | Contains information about each entry in the key list.
--
--
--
-- /See:/ 'keyListEntry' smart constructor.
data KeyListEntry = KeyListEntry'
  { _kleKeyId  :: !(Maybe Text)
  , _kleKeyARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kleKeyId' - Unique identifier of the key.
--
-- * 'kleKeyARN' - ARN of the key.
keyListEntry
    :: KeyListEntry
keyListEntry = KeyListEntry' {_kleKeyId = Nothing, _kleKeyARN = Nothing}


-- | Unique identifier of the key.
kleKeyId :: Lens' KeyListEntry (Maybe Text)
kleKeyId = lens _kleKeyId (\ s a -> s{_kleKeyId = a})

-- | ARN of the key.
kleKeyARN :: Lens' KeyListEntry (Maybe Text)
kleKeyARN = lens _kleKeyARN (\ s a -> s{_kleKeyARN = a})

instance FromJSON KeyListEntry where
        parseJSON
          = withObject "KeyListEntry"
              (\ x ->
                 KeyListEntry' <$>
                   (x .:? "KeyId") <*> (x .:? "KeyArn"))

instance Hashable KeyListEntry where

instance NFData KeyListEntry where

-- | Contains metadata about a customer master key (CMK).
--
--
-- This data type is used as a response element for the 'CreateKey' and 'DescribeKey' operations.
--
--
-- /See:/ 'keyMetadata' smart constructor.
data KeyMetadata = KeyMetadata'
  { _kmOrigin          :: !(Maybe OriginType)
  , _kmExpirationModel :: !(Maybe ExpirationModelType)
  , _kmKeyManager      :: !(Maybe KeyManagerType)
  , _kmEnabled         :: !(Maybe Bool)
  , _kmValidTo         :: !(Maybe POSIX)
  , _kmARN             :: !(Maybe Text)
  , _kmKeyState        :: !(Maybe KeyState)
  , _kmAWSAccountId    :: !(Maybe Text)
  , _kmKeyUsage        :: !(Maybe KeyUsageType)
  , _kmCreationDate    :: !(Maybe POSIX)
  , _kmDeletionDate    :: !(Maybe POSIX)
  , _kmDescription     :: !(Maybe Text)
  , _kmKeyId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kmOrigin' - The source of the CMK's key material. When this value is @AWS_KMS@ , AWS KMS created the key material. When this value is @EXTERNAL@ , the key material was imported from your existing key management infrastructure or the CMK lacks key material.
--
-- * 'kmExpirationModel' - Specifies whether the CMK's key material expires. This value is present only when @Origin@ is @EXTERNAL@ , otherwise this value is omitted.
--
-- * 'kmKeyManager' - The CMK's manager. CMKs are either customer-managed or AWS-managed. For more information about the difference, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'kmEnabled' - Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this value is true, otherwise it is false.
--
-- * 'kmValidTo' - The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. This value is present only for CMKs whose @Origin@ is @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@ , otherwise this value is omitted.
--
-- * 'kmARN' - The Amazon Resource Name (ARN) of the CMK. For examples, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/ .
--
-- * 'kmKeyState' - The state of the CMK. For more information about how key state affects the use of a CMK, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'kmAWSAccountId' - The twelve-digit account ID of the AWS account that owns the CMK.
--
-- * 'kmKeyUsage' - The cryptographic operations for which you can use the CMK. Currently the only allowed value is @ENCRYPT_DECRYPT@ , which means you can use the CMK for the 'Encrypt' and 'Decrypt' operations.
--
-- * 'kmCreationDate' - The date and time when the CMK was created.
--
-- * 'kmDeletionDate' - The date and time after which AWS KMS deletes the CMK. This value is present only when @KeyState@ is @PendingDeletion@ , otherwise this value is omitted.
--
-- * 'kmDescription' - The description of the CMK.
--
-- * 'kmKeyId' - The globally unique identifier for the CMK.
keyMetadata
    :: Text -- ^ 'kmKeyId'
    -> KeyMetadata
keyMetadata pKeyId_ =
  KeyMetadata'
    { _kmOrigin = Nothing
    , _kmExpirationModel = Nothing
    , _kmKeyManager = Nothing
    , _kmEnabled = Nothing
    , _kmValidTo = Nothing
    , _kmARN = Nothing
    , _kmKeyState = Nothing
    , _kmAWSAccountId = Nothing
    , _kmKeyUsage = Nothing
    , _kmCreationDate = Nothing
    , _kmDeletionDate = Nothing
    , _kmDescription = Nothing
    , _kmKeyId = pKeyId_
    }


-- | The source of the CMK's key material. When this value is @AWS_KMS@ , AWS KMS created the key material. When this value is @EXTERNAL@ , the key material was imported from your existing key management infrastructure or the CMK lacks key material.
kmOrigin :: Lens' KeyMetadata (Maybe OriginType)
kmOrigin = lens _kmOrigin (\ s a -> s{_kmOrigin = a})

-- | Specifies whether the CMK's key material expires. This value is present only when @Origin@ is @EXTERNAL@ , otherwise this value is omitted.
kmExpirationModel :: Lens' KeyMetadata (Maybe ExpirationModelType)
kmExpirationModel = lens _kmExpirationModel (\ s a -> s{_kmExpirationModel = a})

-- | The CMK's manager. CMKs are either customer-managed or AWS-managed. For more information about the difference, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
kmKeyManager :: Lens' KeyMetadata (Maybe KeyManagerType)
kmKeyManager = lens _kmKeyManager (\ s a -> s{_kmKeyManager = a})

-- | Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this value is true, otherwise it is false.
kmEnabled :: Lens' KeyMetadata (Maybe Bool)
kmEnabled = lens _kmEnabled (\ s a -> s{_kmEnabled = a})

-- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. This value is present only for CMKs whose @Origin@ is @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@ , otherwise this value is omitted.
kmValidTo :: Lens' KeyMetadata (Maybe UTCTime)
kmValidTo = lens _kmValidTo (\ s a -> s{_kmValidTo = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the CMK. For examples, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/ .
kmARN :: Lens' KeyMetadata (Maybe Text)
kmARN = lens _kmARN (\ s a -> s{_kmARN = a})

-- | The state of the CMK. For more information about how key state affects the use of a CMK, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
kmKeyState :: Lens' KeyMetadata (Maybe KeyState)
kmKeyState = lens _kmKeyState (\ s a -> s{_kmKeyState = a})

-- | The twelve-digit account ID of the AWS account that owns the CMK.
kmAWSAccountId :: Lens' KeyMetadata (Maybe Text)
kmAWSAccountId = lens _kmAWSAccountId (\ s a -> s{_kmAWSAccountId = a})

-- | The cryptographic operations for which you can use the CMK. Currently the only allowed value is @ENCRYPT_DECRYPT@ , which means you can use the CMK for the 'Encrypt' and 'Decrypt' operations.
kmKeyUsage :: Lens' KeyMetadata (Maybe KeyUsageType)
kmKeyUsage = lens _kmKeyUsage (\ s a -> s{_kmKeyUsage = a})

-- | The date and time when the CMK was created.
kmCreationDate :: Lens' KeyMetadata (Maybe UTCTime)
kmCreationDate = lens _kmCreationDate (\ s a -> s{_kmCreationDate = a}) . mapping _Time

-- | The date and time after which AWS KMS deletes the CMK. This value is present only when @KeyState@ is @PendingDeletion@ , otherwise this value is omitted.
kmDeletionDate :: Lens' KeyMetadata (Maybe UTCTime)
kmDeletionDate = lens _kmDeletionDate (\ s a -> s{_kmDeletionDate = a}) . mapping _Time

-- | The description of the CMK.
kmDescription :: Lens' KeyMetadata (Maybe Text)
kmDescription = lens _kmDescription (\ s a -> s{_kmDescription = a})

-- | The globally unique identifier for the CMK.
kmKeyId :: Lens' KeyMetadata Text
kmKeyId = lens _kmKeyId (\ s a -> s{_kmKeyId = a})

instance FromJSON KeyMetadata where
        parseJSON
          = withObject "KeyMetadata"
              (\ x ->
                 KeyMetadata' <$>
                   (x .:? "Origin") <*> (x .:? "ExpirationModel") <*>
                     (x .:? "KeyManager")
                     <*> (x .:? "Enabled")
                     <*> (x .:? "ValidTo")
                     <*> (x .:? "Arn")
                     <*> (x .:? "KeyState")
                     <*> (x .:? "AWSAccountId")
                     <*> (x .:? "KeyUsage")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "DeletionDate")
                     <*> (x .:? "Description")
                     <*> (x .: "KeyId"))

instance Hashable KeyMetadata where

instance NFData KeyMetadata where

-- | /See:/ 'listGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
  { _lgTruncated  :: !(Maybe Bool)
  , _lgGrants     :: !(Maybe [GrantListEntry])
  , _lgNextMarker :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGrantsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgTruncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in this response to the @Marker@ parameter in a subsequent request.
--
-- * 'lgGrants' - A list of grants.
--
-- * 'lgNextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
listGrantsResponse
    :: ListGrantsResponse
listGrantsResponse =
  ListGrantsResponse'
    {_lgTruncated = Nothing, _lgGrants = Nothing, _lgNextMarker = Nothing}


-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in this response to the @Marker@ parameter in a subsequent request.
lgTruncated :: Lens' ListGrantsResponse (Maybe Bool)
lgTruncated = lens _lgTruncated (\ s a -> s{_lgTruncated = a})

-- | A list of grants.
lgGrants :: Lens' ListGrantsResponse [GrantListEntry]
lgGrants = lens _lgGrants (\ s a -> s{_lgGrants = a}) . _Default . _Coerce

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
lgNextMarker :: Lens' ListGrantsResponse (Maybe Text)
lgNextMarker = lens _lgNextMarker (\ s a -> s{_lgNextMarker = a})

instance FromJSON ListGrantsResponse where
        parseJSON
          = withObject "ListGrantsResponse"
              (\ x ->
                 ListGrantsResponse' <$>
                   (x .:? "Truncated") <*> (x .:? "Grants" .!= mempty)
                     <*> (x .:? "NextMarker"))

instance Hashable ListGrantsResponse where

instance NFData ListGrantsResponse where

-- | A key-value pair. A tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings.
--
--
-- For information about the rules that apply to tag keys and tag values, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions> in the /AWS Billing and Cost Management User Guide/ .
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagTagKey   :: !Text
  , _tagTagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagTagKey' - The key of the tag.
--
-- * 'tagTagValue' - The value of the tag.
tag
    :: Text -- ^ 'tagTagKey'
    -> Text -- ^ 'tagTagValue'
    -> Tag
tag pTagKey_ pTagValue_ =
  Tag' {_tagTagKey = pTagKey_, _tagTagValue = pTagValue_}


-- | The key of the tag.
tagTagKey :: Lens' Tag Text
tagTagKey = lens _tagTagKey (\ s a -> s{_tagTagKey = a})

-- | The value of the tag.
tagTagValue :: Lens' Tag Text
tagTagValue = lens _tagTagValue (\ s a -> s{_tagTagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x ->
                 Tag' <$> (x .: "TagKey") <*> (x .: "TagValue"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("TagKey" .= _tagTagKey),
                  Just ("TagValue" .= _tagTagValue)])
