{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.DescribeSecret
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a secret. It does not include the encrypted fields. Only those fields that are populated with a value are returned in the response.
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:DescribeSecret
--
--
--
-- __Related operations__
--
--     * To create a secret, use 'CreateSecret' .
--
--     * To modify a secret, use 'UpdateSecret' .
--
--     * To retrieve the encrypted secret information in a version of the secret, use 'GetSecretValue' .
--
--     * To list all of the secrets in the AWS account, use 'ListSecrets' .
--
--
--
module Network.AWS.SecretsManager.DescribeSecret
    (
    -- * Creating a Request
      describeSecret
    , DescribeSecret
    -- * Request Lenses
    , dSecretId

    -- * Destructuring the Response
    , describeSecretResponse
    , DescribeSecretResponse
    -- * Response Lenses
    , drsLastChangedDate
    , drsARN
    , drsRotationRules
    , drsDeletedDate
    , drsRotationEnabled
    , drsKMSKeyId
    , drsName
    , drsVersionIdsToStages
    , drsLastRotatedDate
    , drsLastAccessedDate
    , drsDescription
    , drsRotationLambdaARN
    , drsTags
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'describeSecret' smart constructor.
newtype DescribeSecret = DescribeSecret'
  { _dSecretId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSecret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSecretId' - The identifier of the secret whose details you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
describeSecret
    :: Text -- ^ 'dSecretId'
    -> DescribeSecret
describeSecret pSecretId_ = DescribeSecret' {_dSecretId = pSecretId_}


-- | The identifier of the secret whose details you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
dSecretId :: Lens' DescribeSecret Text
dSecretId = lens _dSecretId (\ s a -> s{_dSecretId = a})

instance AWSRequest DescribeSecret where
        type Rs DescribeSecret = DescribeSecretResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSecretResponse' <$>
                   (x .?> "LastChangedDate") <*> (x .?> "ARN") <*>
                     (x .?> "RotationRules")
                     <*> (x .?> "DeletedDate")
                     <*> (x .?> "RotationEnabled")
                     <*> (x .?> "KmsKeyId")
                     <*> (x .?> "Name")
                     <*> (x .?> "VersionIdsToStages" .!@ mempty)
                     <*> (x .?> "LastRotatedDate")
                     <*> (x .?> "LastAccessedDate")
                     <*> (x .?> "Description")
                     <*> (x .?> "RotationLambdaARN")
                     <*> (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSecret where

instance NFData DescribeSecret where

instance ToHeaders DescribeSecret where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.DescribeSecret" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSecret where
        toJSON DescribeSecret'{..}
          = object
              (catMaybes [Just ("SecretId" .= _dSecretId)])

instance ToPath DescribeSecret where
        toPath = const "/"

instance ToQuery DescribeSecret where
        toQuery = const mempty

-- | /See:/ 'describeSecretResponse' smart constructor.
data DescribeSecretResponse = DescribeSecretResponse'
  { _drsLastChangedDate    :: !(Maybe POSIX)
  , _drsARN                :: !(Maybe Text)
  , _drsRotationRules      :: !(Maybe RotationRulesType)
  , _drsDeletedDate        :: !(Maybe POSIX)
  , _drsRotationEnabled    :: !(Maybe Bool)
  , _drsKMSKeyId           :: !(Maybe Text)
  , _drsName               :: !(Maybe Text)
  , _drsVersionIdsToStages :: !(Maybe (Map Text (List1 Text)))
  , _drsLastRotatedDate    :: !(Maybe POSIX)
  , _drsLastAccessedDate   :: !(Maybe POSIX)
  , _drsDescription        :: !(Maybe Text)
  , _drsRotationLambdaARN  :: !(Maybe Text)
  , _drsTags               :: !(Maybe [Tag])
  , _drsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSecretResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsLastChangedDate' - The last date and time that this secret was modified in any way.
--
-- * 'drsARN' - The ARN of the secret.
--
-- * 'drsRotationRules' - A structure that contains the rotation configuration for this secret.
--
-- * 'drsDeletedDate' - This value exists if the secret is scheduled for deletion. Some time after the specified date and time, Secrets Manager deletes the secret and all of its versions. If a secret is scheduled for deletion, then its details, including the encrypted secret information, is not accessible. To cancel a scheduled deletion and restore access, use 'RestoreSecret' .
--
-- * 'drsRotationEnabled' - Specifies whether automatic rotation is enabled for this secret. To enable rotation, use 'RotateSecret' with @AutomaticallyRotateAfterDays@ set to a value greater than 0. To disable rotation, use 'CancelRotateSecret' .
--
-- * 'drsKMSKeyId' - The ARN or alias of the AWS KMS customer master key (CMK) that's used to encrypt the @SecretString@ or @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default KMS CMK (the one named @awssecretsmanager@ ) for this account.
--
-- * 'drsName' - The user-provided friendly name of the secret.
--
-- * 'drsVersionIdsToStages' - A list of all of the currently assigned @VersionStage@ staging labels and the @SecretVersionId@ that each is attached to. Staging labels are used to keep track of the different versions during the rotation process.
--
-- * 'drsLastRotatedDate' - The last date and time that the Secrets Manager rotation process for this secret was invoked.
--
-- * 'drsLastAccessedDate' - The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
--
-- * 'drsDescription' - The user-provided description of the secret.
--
-- * 'drsRotationLambdaARN' - The ARN of a Lambda function that's invoked by Secrets Manager to rotate the secret either automatically per the schedule or manually by a call to @RotateSecret@ .
--
-- * 'drsTags' - The list of user-defined tags that are associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
--
-- * 'drsResponseStatus' - -- | The response status code.
describeSecretResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeSecretResponse
describeSecretResponse pResponseStatus_ =
  DescribeSecretResponse'
    { _drsLastChangedDate = Nothing
    , _drsARN = Nothing
    , _drsRotationRules = Nothing
    , _drsDeletedDate = Nothing
    , _drsRotationEnabled = Nothing
    , _drsKMSKeyId = Nothing
    , _drsName = Nothing
    , _drsVersionIdsToStages = Nothing
    , _drsLastRotatedDate = Nothing
    , _drsLastAccessedDate = Nothing
    , _drsDescription = Nothing
    , _drsRotationLambdaARN = Nothing
    , _drsTags = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The last date and time that this secret was modified in any way.
drsLastChangedDate :: Lens' DescribeSecretResponse (Maybe UTCTime)
drsLastChangedDate = lens _drsLastChangedDate (\ s a -> s{_drsLastChangedDate = a}) . mapping _Time

-- | The ARN of the secret.
drsARN :: Lens' DescribeSecretResponse (Maybe Text)
drsARN = lens _drsARN (\ s a -> s{_drsARN = a})

-- | A structure that contains the rotation configuration for this secret.
drsRotationRules :: Lens' DescribeSecretResponse (Maybe RotationRulesType)
drsRotationRules = lens _drsRotationRules (\ s a -> s{_drsRotationRules = a})

-- | This value exists if the secret is scheduled for deletion. Some time after the specified date and time, Secrets Manager deletes the secret and all of its versions. If a secret is scheduled for deletion, then its details, including the encrypted secret information, is not accessible. To cancel a scheduled deletion and restore access, use 'RestoreSecret' .
drsDeletedDate :: Lens' DescribeSecretResponse (Maybe UTCTime)
drsDeletedDate = lens _drsDeletedDate (\ s a -> s{_drsDeletedDate = a}) . mapping _Time

-- | Specifies whether automatic rotation is enabled for this secret. To enable rotation, use 'RotateSecret' with @AutomaticallyRotateAfterDays@ set to a value greater than 0. To disable rotation, use 'CancelRotateSecret' .
drsRotationEnabled :: Lens' DescribeSecretResponse (Maybe Bool)
drsRotationEnabled = lens _drsRotationEnabled (\ s a -> s{_drsRotationEnabled = a})

-- | The ARN or alias of the AWS KMS customer master key (CMK) that's used to encrypt the @SecretString@ or @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default KMS CMK (the one named @awssecretsmanager@ ) for this account.
drsKMSKeyId :: Lens' DescribeSecretResponse (Maybe Text)
drsKMSKeyId = lens _drsKMSKeyId (\ s a -> s{_drsKMSKeyId = a})

-- | The user-provided friendly name of the secret.
drsName :: Lens' DescribeSecretResponse (Maybe Text)
drsName = lens _drsName (\ s a -> s{_drsName = a})

-- | A list of all of the currently assigned @VersionStage@ staging labels and the @SecretVersionId@ that each is attached to. Staging labels are used to keep track of the different versions during the rotation process.
drsVersionIdsToStages :: Lens' DescribeSecretResponse (HashMap Text (NonEmpty Text))
drsVersionIdsToStages = lens _drsVersionIdsToStages (\ s a -> s{_drsVersionIdsToStages = a}) . _Default . _Map

-- | The last date and time that the Secrets Manager rotation process for this secret was invoked.
drsLastRotatedDate :: Lens' DescribeSecretResponse (Maybe UTCTime)
drsLastRotatedDate = lens _drsLastRotatedDate (\ s a -> s{_drsLastRotatedDate = a}) . mapping _Time

-- | The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
drsLastAccessedDate :: Lens' DescribeSecretResponse (Maybe UTCTime)
drsLastAccessedDate = lens _drsLastAccessedDate (\ s a -> s{_drsLastAccessedDate = a}) . mapping _Time

-- | The user-provided description of the secret.
drsDescription :: Lens' DescribeSecretResponse (Maybe Text)
drsDescription = lens _drsDescription (\ s a -> s{_drsDescription = a})

-- | The ARN of a Lambda function that's invoked by Secrets Manager to rotate the secret either automatically per the schedule or manually by a call to @RotateSecret@ .
drsRotationLambdaARN :: Lens' DescribeSecretResponse (Maybe Text)
drsRotationLambdaARN = lens _drsRotationLambdaARN (\ s a -> s{_drsRotationLambdaARN = a})

-- | The list of user-defined tags that are associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
drsTags :: Lens' DescribeSecretResponse [Tag]
drsTags = lens _drsTags (\ s a -> s{_drsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeSecretResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeSecretResponse where
