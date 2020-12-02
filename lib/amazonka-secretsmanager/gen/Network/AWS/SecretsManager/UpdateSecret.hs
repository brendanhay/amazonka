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
-- Module      : Network.AWS.SecretsManager.UpdateSecret
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies many of the details of a secret. If you include a @ClientRequestToken@ and either @SecretString@ or @SecretBinary@ then it also creates a new version attached to the secret.
--
--
-- To modify the rotation configuration of a secret, use 'RotateSecret' instead.
--
--     * If a version with a @SecretVersionId@ with the same value as the @ClientRequestToken@ parameter already exists, the operation generates an error. You cannot modify an existing version, you can only create new ones.
--
--     * If you include @SecretString@ or @SecretBinary@ to create a new secret version, Secrets Manager automatically attaches the staging label @AWSCURRENT@ to the new version.
--
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:UpdateSecret
--
--     * kms:GenerateDataKey - needed only if you use a custom KMS key to encrypt the secret. You do not need this permission to use the account's AWS managed CMK for Secrets Manager.
--
--     * kms:Decrypt - needed only if you use a custom KMS key to encrypt the secret. You do not need this permission to use the account's AWS managed CMK for Secrets Manager.
--
--
--
-- __Related operations__
--
--     * To create a new secret, use 'CreateSecret' .
--
--     * To add only a new version to an existing secret, use 'PutSecretValue' .
--
--     * To get the details for a secret, use 'DescribeSecret' .
--
--     * To list the versions contained in a secret, use 'ListSecretVersionIds' .
--
--
--
module Network.AWS.SecretsManager.UpdateSecret
    (
    -- * Creating a Request
      updateSecret
    , UpdateSecret
    -- * Request Lenses
    , usSecretBinary
    , usKMSKeyId
    , usSecretString
    , usClientRequestToken
    , usDescription
    , usSecretId

    -- * Destructuring the Response
    , updateSecretResponse
    , UpdateSecretResponse
    -- * Response Lenses
    , usrsVersionId
    , usrsARN
    , usrsName
    , usrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'updateSecret' smart constructor.
data UpdateSecret = UpdateSecret'
  { _usSecretBinary       :: !(Maybe (Sensitive Base64))
  , _usKMSKeyId           :: !(Maybe Text)
  , _usSecretString       :: !(Maybe (Sensitive Text))
  , _usClientRequestToken :: !(Maybe Text)
  , _usDescription        :: !(Maybe Text)
  , _usSecretId           :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSecret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usSecretBinary' - (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty. This parameter is not accessible using the Secrets Manager console.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'usKMSKeyId' - (Optional) Specifies the ARN or alias of the KMS customer master key (CMK) to be used to encrypt the protected text in the versions of this secret. If you don't specify this value, then Secrets Manager defaults to using the default CMK in the account (the one named @aws/secretsmanager@ ). If a KMS CMK with that name doesn't exist, then Secrets Manager creates it for you automatically the first time it needs to encrypt a version's @Plaintext@ or @PlaintextString@ fields. /Important:/ You can only use the account's default CMK to encrypt and decrypt if you call this operation using credentials from the same account that owns the secret. If the secret is in a different account, then you must create a custom CMK and provide the ARN in this field.
--
-- * 'usSecretString' - (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty. If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse. For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
--
-- * 'usClientRequestToken' - (Optional) If you want to add a new version to the secret, this parameter specifies a unique identifier for the new version that helps ensure idempotency.  If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request. You typically only need to interact with this value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.  Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing.     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from the request then an error occurs because you cannot modify an existing secret value. This value becomes the @SecretVersionId@ of the new version.
--
-- * 'usDescription' - (Optional) Specifies a user-provided description of the secret.
--
-- * 'usSecretId' - Specifies the secret that you want to update or to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
updateSecret
    :: Text -- ^ 'usSecretId'
    -> UpdateSecret
updateSecret pSecretId_ =
  UpdateSecret'
    { _usSecretBinary = Nothing
    , _usKMSKeyId = Nothing
    , _usSecretString = Nothing
    , _usClientRequestToken = Nothing
    , _usDescription = Nothing
    , _usSecretId = pSecretId_
    }


-- | (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty. This parameter is not accessible using the Secrets Manager console.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
usSecretBinary :: Lens' UpdateSecret (Maybe ByteString)
usSecretBinary = lens _usSecretBinary (\ s a -> s{_usSecretBinary = a}) . mapping (_Sensitive . _Base64)

-- | (Optional) Specifies the ARN or alias of the KMS customer master key (CMK) to be used to encrypt the protected text in the versions of this secret. If you don't specify this value, then Secrets Manager defaults to using the default CMK in the account (the one named @aws/secretsmanager@ ). If a KMS CMK with that name doesn't exist, then Secrets Manager creates it for you automatically the first time it needs to encrypt a version's @Plaintext@ or @PlaintextString@ fields. /Important:/ You can only use the account's default CMK to encrypt and decrypt if you call this operation using credentials from the same account that owns the secret. If the secret is in a different account, then you must create a custom CMK and provide the ARN in this field.
usKMSKeyId :: Lens' UpdateSecret (Maybe Text)
usKMSKeyId = lens _usKMSKeyId (\ s a -> s{_usKMSKeyId = a})

-- | (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty. If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse. For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
usSecretString :: Lens' UpdateSecret (Maybe Text)
usSecretString = lens _usSecretString (\ s a -> s{_usSecretString = a}) . mapping _Sensitive

-- | (Optional) If you want to add a new version to the secret, this parameter specifies a unique identifier for the new version that helps ensure idempotency.  If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request. You typically only need to interact with this value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.  Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing.     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from the request then an error occurs because you cannot modify an existing secret value. This value becomes the @SecretVersionId@ of the new version.
usClientRequestToken :: Lens' UpdateSecret (Maybe Text)
usClientRequestToken = lens _usClientRequestToken (\ s a -> s{_usClientRequestToken = a})

-- | (Optional) Specifies a user-provided description of the secret.
usDescription :: Lens' UpdateSecret (Maybe Text)
usDescription = lens _usDescription (\ s a -> s{_usDescription = a})

-- | Specifies the secret that you want to update or to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
usSecretId :: Lens' UpdateSecret Text
usSecretId = lens _usSecretId (\ s a -> s{_usSecretId = a})

instance AWSRequest UpdateSecret where
        type Rs UpdateSecret = UpdateSecretResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSecretResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "ARN") <*>
                     (x .?> "Name")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateSecret where

instance NFData UpdateSecret where

instance ToHeaders UpdateSecret where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.UpdateSecret" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSecret where
        toJSON UpdateSecret'{..}
          = object
              (catMaybes
                 [("SecretBinary" .=) <$> _usSecretBinary,
                  ("KmsKeyId" .=) <$> _usKMSKeyId,
                  ("SecretString" .=) <$> _usSecretString,
                  ("ClientRequestToken" .=) <$> _usClientRequestToken,
                  ("Description" .=) <$> _usDescription,
                  Just ("SecretId" .= _usSecretId)])

instance ToPath UpdateSecret where
        toPath = const "/"

instance ToQuery UpdateSecret where
        toQuery = const mempty

-- | /See:/ 'updateSecretResponse' smart constructor.
data UpdateSecretResponse = UpdateSecretResponse'
  { _usrsVersionId      :: !(Maybe Text)
  , _usrsARN            :: !(Maybe Text)
  , _usrsName           :: !(Maybe Text)
  , _usrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSecretResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsVersionId' - If a version of the secret was created or updated by this operation, then its unique identifier is returned.
--
-- * 'usrsARN' - The ARN of this secret.
--
-- * 'usrsName' - The friendly name of this secret.
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateSecretResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateSecretResponse
updateSecretResponse pResponseStatus_ =
  UpdateSecretResponse'
    { _usrsVersionId = Nothing
    , _usrsARN = Nothing
    , _usrsName = Nothing
    , _usrsResponseStatus = pResponseStatus_
    }


-- | If a version of the secret was created or updated by this operation, then its unique identifier is returned.
usrsVersionId :: Lens' UpdateSecretResponse (Maybe Text)
usrsVersionId = lens _usrsVersionId (\ s a -> s{_usrsVersionId = a})

-- | The ARN of this secret.
usrsARN :: Lens' UpdateSecretResponse (Maybe Text)
usrsARN = lens _usrsARN (\ s a -> s{_usrsARN = a})

-- | The friendly name of this secret.
usrsName :: Lens' UpdateSecretResponse (Maybe Text)
usrsName = lens _usrsName (\ s a -> s{_usrsName = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateSecretResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateSecretResponse where
