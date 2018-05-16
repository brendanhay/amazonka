{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SecretsManager.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SecretsManager.Types.Sum

-- | A structure that defines the rotation configuration for the secret.
--
--
--
-- /See:/ 'rotationRulesType' smart constructor.
newtype RotationRulesType = RotationRulesType'
  { _rrtAutomaticallyAfterDays :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RotationRulesType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrtAutomaticallyAfterDays' - Specifies the number of days between automatic scheduled rotations of the secret.
rotationRulesType
    :: RotationRulesType
rotationRulesType = RotationRulesType' {_rrtAutomaticallyAfterDays = Nothing}


-- | Specifies the number of days between automatic scheduled rotations of the secret.
rrtAutomaticallyAfterDays :: Lens' RotationRulesType (Maybe Natural)
rrtAutomaticallyAfterDays = lens _rrtAutomaticallyAfterDays (\ s a -> s{_rrtAutomaticallyAfterDays = a}) . mapping _Nat

instance FromJSON RotationRulesType where
        parseJSON
          = withObject "RotationRulesType"
              (\ x ->
                 RotationRulesType' <$>
                   (x .:? "AutomaticallyAfterDays"))

instance Hashable RotationRulesType where

instance NFData RotationRulesType where

instance ToJSON RotationRulesType where
        toJSON RotationRulesType'{..}
          = object
              (catMaybes
                 [("AutomaticallyAfterDays" .=) <$>
                    _rrtAutomaticallyAfterDays])

-- | A structure that contains the details about a secret. It does not include the encrypted @SecretString@ and @SecretBinary@ values. To get those values, use the 'GetSecretValue' operation.
--
--
--
-- /See:/ 'secretListEntry' smart constructor.
data SecretListEntry = SecretListEntry'
  { _sleLastChangedDate        :: !(Maybe POSIX)
  , _sleARN                    :: !(Maybe Text)
  , _sleSecretVersionsToStages :: !(Maybe (Map Text (List1 Text)))
  , _sleRotationRules          :: !(Maybe RotationRulesType)
  , _sleDeletedDate            :: !(Maybe POSIX)
  , _sleRotationEnabled        :: !(Maybe Bool)
  , _sleKMSKeyId               :: !(Maybe Text)
  , _sleName                   :: !(Maybe Text)
  , _sleLastRotatedDate        :: !(Maybe POSIX)
  , _sleLastAccessedDate       :: !(Maybe POSIX)
  , _sleDescription            :: !(Maybe Text)
  , _sleRotationLambdaARN      :: !(Maybe Text)
  , _sleTags                   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecretListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sleLastChangedDate' - The last date and time that this secret was modified in any way.
--
-- * 'sleARN' - The Amazon Resource Name (ARN) of the secret. For more information about ARNs in Secrets Manager, see <http://docs.aws.amazon.com/http:/docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#iam-resources Policy Resources> in the /AWS Secrets Manager User Guide/ .
--
-- * 'sleSecretVersionsToStages' - A list of all of the currently assigned @SecretVersionStage@ staging labels and the @SecretVersionId@ that each is attached to. Staging labels are used to keep track of the different versions during the rotation process.
--
-- * 'sleRotationRules' - A structure that defines the rotation configuration for the secret.
--
-- * 'sleDeletedDate' - The date and time on which this secret was deleted. Not present on active secrets. The secret can be recovered until the number of days in the recovery window has passed, as specified in the @RecoveryWindowInDays@ parameter of the 'DeleteSecret' operation.
--
-- * 'sleRotationEnabled' - Indicated whether automatic, scheduled rotation is enabled for this secret.
--
-- * 'sleKMSKeyId' - The ARN or alias of the AWS KMS customer master key (CMK) that's used to encrypt the @SecretString@ and @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default KMS CMK (the one named @awssecretsmanager@ ) for this account.
--
-- * 'sleName' - The friendly name of the secret. You can use forward slashes in the name to represent a path hierarchy. For example, @/prod/databases/dbserver1@ could represent the secret for a server named @dbserver1@ in the folder @databases@ in the folder @prod@ .
--
-- * 'sleLastRotatedDate' - The last date and time that the rotation process for this secret was invoked.
--
-- * 'sleLastAccessedDate' - The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
--
-- * 'sleDescription' - The user-provided description of the secret.
--
-- * 'sleRotationLambdaARN' - The ARN of an AWS Lambda function that's invoked by Secrets Manager to rotate and expire the secret either automatically per the schedule or manually by a call to 'RotateSecret' .
--
-- * 'sleTags' - The list of user-defined tags that are associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
secretListEntry
    :: SecretListEntry
secretListEntry =
  SecretListEntry'
    { _sleLastChangedDate = Nothing
    , _sleARN = Nothing
    , _sleSecretVersionsToStages = Nothing
    , _sleRotationRules = Nothing
    , _sleDeletedDate = Nothing
    , _sleRotationEnabled = Nothing
    , _sleKMSKeyId = Nothing
    , _sleName = Nothing
    , _sleLastRotatedDate = Nothing
    , _sleLastAccessedDate = Nothing
    , _sleDescription = Nothing
    , _sleRotationLambdaARN = Nothing
    , _sleTags = Nothing
    }


-- | The last date and time that this secret was modified in any way.
sleLastChangedDate :: Lens' SecretListEntry (Maybe UTCTime)
sleLastChangedDate = lens _sleLastChangedDate (\ s a -> s{_sleLastChangedDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the secret. For more information about ARNs in Secrets Manager, see <http://docs.aws.amazon.com/http:/docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#iam-resources Policy Resources> in the /AWS Secrets Manager User Guide/ .
sleARN :: Lens' SecretListEntry (Maybe Text)
sleARN = lens _sleARN (\ s a -> s{_sleARN = a})

-- | A list of all of the currently assigned @SecretVersionStage@ staging labels and the @SecretVersionId@ that each is attached to. Staging labels are used to keep track of the different versions during the rotation process.
sleSecretVersionsToStages :: Lens' SecretListEntry (HashMap Text (NonEmpty Text))
sleSecretVersionsToStages = lens _sleSecretVersionsToStages (\ s a -> s{_sleSecretVersionsToStages = a}) . _Default . _Map

-- | A structure that defines the rotation configuration for the secret.
sleRotationRules :: Lens' SecretListEntry (Maybe RotationRulesType)
sleRotationRules = lens _sleRotationRules (\ s a -> s{_sleRotationRules = a})

-- | The date and time on which this secret was deleted. Not present on active secrets. The secret can be recovered until the number of days in the recovery window has passed, as specified in the @RecoveryWindowInDays@ parameter of the 'DeleteSecret' operation.
sleDeletedDate :: Lens' SecretListEntry (Maybe UTCTime)
sleDeletedDate = lens _sleDeletedDate (\ s a -> s{_sleDeletedDate = a}) . mapping _Time

-- | Indicated whether automatic, scheduled rotation is enabled for this secret.
sleRotationEnabled :: Lens' SecretListEntry (Maybe Bool)
sleRotationEnabled = lens _sleRotationEnabled (\ s a -> s{_sleRotationEnabled = a})

-- | The ARN or alias of the AWS KMS customer master key (CMK) that's used to encrypt the @SecretString@ and @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default KMS CMK (the one named @awssecretsmanager@ ) for this account.
sleKMSKeyId :: Lens' SecretListEntry (Maybe Text)
sleKMSKeyId = lens _sleKMSKeyId (\ s a -> s{_sleKMSKeyId = a})

-- | The friendly name of the secret. You can use forward slashes in the name to represent a path hierarchy. For example, @/prod/databases/dbserver1@ could represent the secret for a server named @dbserver1@ in the folder @databases@ in the folder @prod@ .
sleName :: Lens' SecretListEntry (Maybe Text)
sleName = lens _sleName (\ s a -> s{_sleName = a})

-- | The last date and time that the rotation process for this secret was invoked.
sleLastRotatedDate :: Lens' SecretListEntry (Maybe UTCTime)
sleLastRotatedDate = lens _sleLastRotatedDate (\ s a -> s{_sleLastRotatedDate = a}) . mapping _Time

-- | The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
sleLastAccessedDate :: Lens' SecretListEntry (Maybe UTCTime)
sleLastAccessedDate = lens _sleLastAccessedDate (\ s a -> s{_sleLastAccessedDate = a}) . mapping _Time

-- | The user-provided description of the secret.
sleDescription :: Lens' SecretListEntry (Maybe Text)
sleDescription = lens _sleDescription (\ s a -> s{_sleDescription = a})

-- | The ARN of an AWS Lambda function that's invoked by Secrets Manager to rotate and expire the secret either automatically per the schedule or manually by a call to 'RotateSecret' .
sleRotationLambdaARN :: Lens' SecretListEntry (Maybe Text)
sleRotationLambdaARN = lens _sleRotationLambdaARN (\ s a -> s{_sleRotationLambdaARN = a})

-- | The list of user-defined tags that are associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
sleTags :: Lens' SecretListEntry [Tag]
sleTags = lens _sleTags (\ s a -> s{_sleTags = a}) . _Default . _Coerce

instance FromJSON SecretListEntry where
        parseJSON
          = withObject "SecretListEntry"
              (\ x ->
                 SecretListEntry' <$>
                   (x .:? "LastChangedDate") <*> (x .:? "ARN") <*>
                     (x .:? "SecretVersionsToStages" .!= mempty)
                     <*> (x .:? "RotationRules")
                     <*> (x .:? "DeletedDate")
                     <*> (x .:? "RotationEnabled")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "Name")
                     <*> (x .:? "LastRotatedDate")
                     <*> (x .:? "LastAccessedDate")
                     <*> (x .:? "Description")
                     <*> (x .:? "RotationLambdaARN")
                     <*> (x .:? "Tags" .!= mempty))

instance Hashable SecretListEntry where

instance NFData SecretListEntry where

-- | A structure that contains information about one version of a secret.
--
--
--
-- /See:/ 'secretVersionsListEntry' smart constructor.
data SecretVersionsListEntry = SecretVersionsListEntry'
  { _svleVersionId        :: !(Maybe Text)
  , _svleVersionStages    :: !(Maybe (List1 Text))
  , _svleCreatedDate      :: !(Maybe POSIX)
  , _svleLastAccessedDate :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecretVersionsListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svleVersionId' - The unique version identifier of this version of the secret.
--
-- * 'svleVersionStages' - An array of staging labels that are currently associated with this version of the secret.
--
-- * 'svleCreatedDate' - The date and time this version of the secret was created.
--
-- * 'svleLastAccessedDate' - The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
secretVersionsListEntry
    :: SecretVersionsListEntry
secretVersionsListEntry =
  SecretVersionsListEntry'
    { _svleVersionId = Nothing
    , _svleVersionStages = Nothing
    , _svleCreatedDate = Nothing
    , _svleLastAccessedDate = Nothing
    }


-- | The unique version identifier of this version of the secret.
svleVersionId :: Lens' SecretVersionsListEntry (Maybe Text)
svleVersionId = lens _svleVersionId (\ s a -> s{_svleVersionId = a})

-- | An array of staging labels that are currently associated with this version of the secret.
svleVersionStages :: Lens' SecretVersionsListEntry (Maybe (NonEmpty Text))
svleVersionStages = lens _svleVersionStages (\ s a -> s{_svleVersionStages = a}) . mapping _List1

-- | The date and time this version of the secret was created.
svleCreatedDate :: Lens' SecretVersionsListEntry (Maybe UTCTime)
svleCreatedDate = lens _svleCreatedDate (\ s a -> s{_svleCreatedDate = a}) . mapping _Time

-- | The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
svleLastAccessedDate :: Lens' SecretVersionsListEntry (Maybe UTCTime)
svleLastAccessedDate = lens _svleLastAccessedDate (\ s a -> s{_svleLastAccessedDate = a}) . mapping _Time

instance FromJSON SecretVersionsListEntry where
        parseJSON
          = withObject "SecretVersionsListEntry"
              (\ x ->
                 SecretVersionsListEntry' <$>
                   (x .:? "VersionId") <*> (x .:? "VersionStages") <*>
                     (x .:? "CreatedDate")
                     <*> (x .:? "LastAccessedDate"))

instance Hashable SecretVersionsListEntry where

instance NFData SecretVersionsListEntry where

-- | A structure that contains information about a tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The string value that's associated with the key of the tag.
--
-- * 'tagKey' - The key identifier, or name, of the tag.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The string value that's associated with the key of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key identifier, or name, of the tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])
