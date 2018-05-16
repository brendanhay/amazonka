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
-- Module      : Network.AWS.SecretsManager.PutSecretValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a new encrypted secret value in the specified secret. To do this, the operation creates a new version and attaches it to the secret. The version can contain a new @SecretString@ value or a new @SecretBinary@ value. You can also specify the staging labels that are initially attached to the new version.
--
--
--     * If this operation creates the first version for the secret then Secrets Manager automatically attaches the staging label @AWSCURRENT@ to the new version.
--
--     * If another version of this secret already exists, then this operation does not automatically move any staging labels other than those that you explicitly specify in the @VersionStages@ parameter.
--
--     * If this operation moves the staging label @AWSCURRENT@ from another version to this version (because you included it in the @StagingLabels@ parameter) then Secrets Manager also automatically moves the staging label @AWSPREVIOUS@ to the version that @AWSCURRENT@ was removed from.
--
--     * This operation is idempotent. If a version with a @SecretVersionId@ with the same value as the @ClientRequestToken@ parameter already exists and you specify the same secret data, the operation succeeds but does nothing. However, if the secret data is different, then the operation fails because you cannot modify an existing version; you can only create new ones.
--
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:PutSecretValue
--
--     * kms:GenerateDataKey - needed only if you use a customer-created KMS key to encrypt the secret. You do not need this permission to use the account's AWS managed CMK for Secrets Manager.
--
--     * kms:Encrypt - needed only if you use a customer-created KMS key to encrypt the secret. You do not need this permission to use the account's AWS managed CMK for Secrets Manager.
--
--
--
-- __Related operations__
--
--     * To retrieve the encrypted value you store in the version of a secret, use 'GetSecretValue' .
--
--     * To create a secret, use 'CreateSecret' .
--
--     * To get the details for a secret, use 'DescribeSecret' .
--
--     * To list the versions attached to a secret, use 'ListSecretVersionIds' .
--
--
--
module Network.AWS.SecretsManager.PutSecretValue
    (
    -- * Creating a Request
      putSecretValue
    , PutSecretValue
    -- * Request Lenses
    , psvVersionStages
    , psvSecretBinary
    , psvSecretString
    , psvClientRequestToken
    , psvSecretId

    -- * Destructuring the Response
    , putSecretValueResponse
    , PutSecretValueResponse
    -- * Response Lenses
    , psvrsVersionId
    , psvrsARN
    , psvrsVersionStages
    , psvrsName
    , psvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'putSecretValue' smart constructor.
data PutSecretValue = PutSecretValue'
  { _psvVersionStages      :: !(Maybe (List1 Text))
  , _psvSecretBinary       :: !(Maybe (Sensitive Base64))
  , _psvSecretString       :: !(Maybe (Sensitive Text))
  , _psvClientRequestToken :: !(Maybe Text)
  , _psvSecretId           :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutSecretValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psvVersionStages' - (Optional) Specifies a list of staging labels that are attached to this version of the secret. These staging labels are used to track the versions through the rotation process by the Lambda rotation function. A staging label must be unique to a single version of the secret. If you specify a staging label that's already associated with a different version of the same secret then that staging label is automatically removed from the other version and attached to this version. If you do not specify a value for @VersionStages@ then Secrets Manager automatically moves the staging label @AWSCURRENT@ to this new version.
--
-- * 'psvSecretBinary' - (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty. This parameter is not accessible if the secret using the Secrets Manager console.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'psvSecretString' - (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty. If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse. For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
--
-- * 'psvClientRequestToken' - (Optional) Specifies a unique identifier for the new version of the secret.  This value helps ensure idempotency. Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.      * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.      * If a version with this value already exists and that version's @SecretString@ or @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from those in the request then the request fails because you cannot modify an existing secret version. You can only create new versions to store new secret values. This value becomes the @SecretVersionId@ of the new version.
--
-- * 'psvSecretId' - Specifies the secret to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret. The secret must already exist.
putSecretValue
    :: Text -- ^ 'psvSecretId'
    -> PutSecretValue
putSecretValue pSecretId_ =
  PutSecretValue'
    { _psvVersionStages = Nothing
    , _psvSecretBinary = Nothing
    , _psvSecretString = Nothing
    , _psvClientRequestToken = Nothing
    , _psvSecretId = pSecretId_
    }


-- | (Optional) Specifies a list of staging labels that are attached to this version of the secret. These staging labels are used to track the versions through the rotation process by the Lambda rotation function. A staging label must be unique to a single version of the secret. If you specify a staging label that's already associated with a different version of the same secret then that staging label is automatically removed from the other version and attached to this version. If you do not specify a value for @VersionStages@ then Secrets Manager automatically moves the staging label @AWSCURRENT@ to this new version.
psvVersionStages :: Lens' PutSecretValue (Maybe (NonEmpty Text))
psvVersionStages = lens _psvVersionStages (\ s a -> s{_psvVersionStages = a}) . mapping _List1

-- | (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty. This parameter is not accessible if the secret using the Secrets Manager console.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
psvSecretBinary :: Lens' PutSecretValue (Maybe ByteString)
psvSecretBinary = lens _psvSecretBinary (\ s a -> s{_psvSecretBinary = a}) . mapping (_Sensitive . _Base64)

-- | (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty. If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse. For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
psvSecretString :: Lens' PutSecretValue (Maybe Text)
psvSecretString = lens _psvSecretString (\ s a -> s{_psvSecretString = a}) . mapping _Sensitive

-- | (Optional) Specifies a unique identifier for the new version of the secret.  This value helps ensure idempotency. Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.      * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.      * If a version with this value already exists and that version's @SecretString@ or @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from those in the request then the request fails because you cannot modify an existing secret version. You can only create new versions to store new secret values. This value becomes the @SecretVersionId@ of the new version.
psvClientRequestToken :: Lens' PutSecretValue (Maybe Text)
psvClientRequestToken = lens _psvClientRequestToken (\ s a -> s{_psvClientRequestToken = a})

-- | Specifies the secret to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret. The secret must already exist.
psvSecretId :: Lens' PutSecretValue Text
psvSecretId = lens _psvSecretId (\ s a -> s{_psvSecretId = a})

instance AWSRequest PutSecretValue where
        type Rs PutSecretValue = PutSecretValueResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 PutSecretValueResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "ARN") <*>
                     (x .?> "VersionStages")
                     <*> (x .?> "Name")
                     <*> (pure (fromEnum s)))

instance Hashable PutSecretValue where

instance NFData PutSecretValue where

instance ToHeaders PutSecretValue where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.PutSecretValue" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutSecretValue where
        toJSON PutSecretValue'{..}
          = object
              (catMaybes
                 [("VersionStages" .=) <$> _psvVersionStages,
                  ("SecretBinary" .=) <$> _psvSecretBinary,
                  ("SecretString" .=) <$> _psvSecretString,
                  ("ClientRequestToken" .=) <$> _psvClientRequestToken,
                  Just ("SecretId" .= _psvSecretId)])

instance ToPath PutSecretValue where
        toPath = const "/"

instance ToQuery PutSecretValue where
        toQuery = const mempty

-- | /See:/ 'putSecretValueResponse' smart constructor.
data PutSecretValueResponse = PutSecretValueResponse'
  { _psvrsVersionId      :: !(Maybe Text)
  , _psvrsARN            :: !(Maybe Text)
  , _psvrsVersionStages  :: !(Maybe (List1 Text))
  , _psvrsName           :: !(Maybe Text)
  , _psvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutSecretValueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psvrsVersionId' - The unique identifier of the version of the secret you just created or updated.
--
-- * 'psvrsARN' - The Amazon Resource Name (ARN) for the secret for which you just created a version.
--
-- * 'psvrsVersionStages' - The list of staging labels that are currently attached to this version of the secret. Staging labels are used to track a version as it progresses through the secret rotation process.
--
-- * 'psvrsName' - The friendly name of the secret for which you just created or updated a version.
--
-- * 'psvrsResponseStatus' - -- | The response status code.
putSecretValueResponse
    :: Int -- ^ 'psvrsResponseStatus'
    -> PutSecretValueResponse
putSecretValueResponse pResponseStatus_ =
  PutSecretValueResponse'
    { _psvrsVersionId = Nothing
    , _psvrsARN = Nothing
    , _psvrsVersionStages = Nothing
    , _psvrsName = Nothing
    , _psvrsResponseStatus = pResponseStatus_
    }


-- | The unique identifier of the version of the secret you just created or updated.
psvrsVersionId :: Lens' PutSecretValueResponse (Maybe Text)
psvrsVersionId = lens _psvrsVersionId (\ s a -> s{_psvrsVersionId = a})

-- | The Amazon Resource Name (ARN) for the secret for which you just created a version.
psvrsARN :: Lens' PutSecretValueResponse (Maybe Text)
psvrsARN = lens _psvrsARN (\ s a -> s{_psvrsARN = a})

-- | The list of staging labels that are currently attached to this version of the secret. Staging labels are used to track a version as it progresses through the secret rotation process.
psvrsVersionStages :: Lens' PutSecretValueResponse (Maybe (NonEmpty Text))
psvrsVersionStages = lens _psvrsVersionStages (\ s a -> s{_psvrsVersionStages = a}) . mapping _List1

-- | The friendly name of the secret for which you just created or updated a version.
psvrsName :: Lens' PutSecretValueResponse (Maybe Text)
psvrsName = lens _psvrsName (\ s a -> s{_psvrsName = a})

-- | -- | The response status code.
psvrsResponseStatus :: Lens' PutSecretValueResponse Int
psvrsResponseStatus = lens _psvrsResponseStatus (\ s a -> s{_psvrsResponseStatus = a})

instance NFData PutSecretValueResponse where
