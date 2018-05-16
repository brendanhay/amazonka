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
-- Module      : Network.AWS.SecretsManager.RotateSecret
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures and starts the asynchronous process of rotating this secret. If you include the configuration parameters, the operation sets those values for the secret and then immediately starts a rotation. If you do not include the configuration parameters, the operation starts a rotation with the values already stored in the secret. After the rotation completes, the protected service and its clients all use the new version of the secret.
--
--
-- This required configuration information includes the ARN of an AWS Lambda function and the time between scheduled rotations. The Lambda rotation function creates a new version of the secret and creates or updates the credentials on the protected service to match. After testing the new credentials, the function marks the new secret with the staging label @AWSCURRENT@ so that your clients all immediately begin to use the new version. For more information about rotating secrets and how to configure a Lambda function to rotate the secrets for your protected service, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/rotating-secrets.html Rotating Secrets in AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
--
-- The rotation function must end with the versions of the secret in one of two states:
--
--     * The @AWSPENDING@ and @AWSCURRENT@ staging labels are attached to the same version of the secret, or
--
--     * The @AWSPENDING@ staging label is not attached to any version of the secret.
--
--
--
-- If instead the @AWSPENDING@ staging label is present but is not attached to the same version as @AWSCURRENT@ then any later invocation of @RotateSecret@ assumes that a previous rotation request is still in progress and returns an error.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:RotateSecret
--
--     * lambda:InvokeFunction (on the function specified in the secret's metadata)
--
--
--
-- __Related operations__
--
--     * To list the secrets in your account, use 'ListSecrets' .
--
--     * To get the details for a version of a secret, use 'DescribeSecret' .
--
--     * To create a new version of a secret, use 'CreateSecret' .
--
--     * To attach staging labels to or remove staging labels from a version of a secret, use 'UpdateSecretVersionStage' .
--
--
--
module Network.AWS.SecretsManager.RotateSecret
    (
    -- * Creating a Request
      rotateSecret
    , RotateSecret
    -- * Request Lenses
    , rsRotationRules
    , rsClientRequestToken
    , rsRotationLambdaARN
    , rsSecretId

    -- * Destructuring the Response
    , rotateSecretResponse
    , RotateSecretResponse
    -- * Response Lenses
    , rsrsVersionId
    , rsrsARN
    , rsrsName
    , rsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'rotateSecret' smart constructor.
data RotateSecret = RotateSecret'
  { _rsRotationRules      :: !(Maybe RotationRulesType)
  , _rsClientRequestToken :: !(Maybe Text)
  , _rsRotationLambdaARN  :: !(Maybe Text)
  , _rsSecretId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RotateSecret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsRotationRules' - A structure that defines the rotation configuration for this secret.
--
-- * 'rsClientRequestToken' - (Optional) Specifies a unique identifier for the new version of the secret that helps ensure idempotency.  If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request for this parameter. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request. You only need to specify your own value if you are implementing your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.  Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the function's processing.     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are the same as the request, then the request is ignored (the operation is idempotent).      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from the request then an error occurs because you cannot modify an existing secret value. This value becomes the @SecretVersionId@ of the new version.
--
-- * 'rsRotationLambdaARN' - (Optional) Specifies the ARN of the Lambda function that can rotate the secret.
--
-- * 'rsSecretId' - Specifies the secret that you want to rotate. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
rotateSecret
    :: Text -- ^ 'rsSecretId'
    -> RotateSecret
rotateSecret pSecretId_ =
  RotateSecret'
    { _rsRotationRules = Nothing
    , _rsClientRequestToken = Nothing
    , _rsRotationLambdaARN = Nothing
    , _rsSecretId = pSecretId_
    }


-- | A structure that defines the rotation configuration for this secret.
rsRotationRules :: Lens' RotateSecret (Maybe RotationRulesType)
rsRotationRules = lens _rsRotationRules (\ s a -> s{_rsRotationRules = a})

-- | (Optional) Specifies a unique identifier for the new version of the secret that helps ensure idempotency.  If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request for this parameter. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request. You only need to specify your own value if you are implementing your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.  Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the function's processing.     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are the same as the request, then the request is ignored (the operation is idempotent).      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from the request then an error occurs because you cannot modify an existing secret value. This value becomes the @SecretVersionId@ of the new version.
rsClientRequestToken :: Lens' RotateSecret (Maybe Text)
rsClientRequestToken = lens _rsClientRequestToken (\ s a -> s{_rsClientRequestToken = a})

-- | (Optional) Specifies the ARN of the Lambda function that can rotate the secret.
rsRotationLambdaARN :: Lens' RotateSecret (Maybe Text)
rsRotationLambdaARN = lens _rsRotationLambdaARN (\ s a -> s{_rsRotationLambdaARN = a})

-- | Specifies the secret that you want to rotate. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
rsSecretId :: Lens' RotateSecret Text
rsSecretId = lens _rsSecretId (\ s a -> s{_rsSecretId = a})

instance AWSRequest RotateSecret where
        type Rs RotateSecret = RotateSecretResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 RotateSecretResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "ARN") <*>
                     (x .?> "Name")
                     <*> (pure (fromEnum s)))

instance Hashable RotateSecret where

instance NFData RotateSecret where

instance ToHeaders RotateSecret where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.RotateSecret" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RotateSecret where
        toJSON RotateSecret'{..}
          = object
              (catMaybes
                 [("RotationRules" .=) <$> _rsRotationRules,
                  ("ClientRequestToken" .=) <$> _rsClientRequestToken,
                  ("RotationLambdaARN" .=) <$> _rsRotationLambdaARN,
                  Just ("SecretId" .= _rsSecretId)])

instance ToPath RotateSecret where
        toPath = const "/"

instance ToQuery RotateSecret where
        toQuery = const mempty

-- | /See:/ 'rotateSecretResponse' smart constructor.
data RotateSecretResponse = RotateSecretResponse'
  { _rsrsVersionId      :: !(Maybe Text)
  , _rsrsARN            :: !(Maybe Text)
  , _rsrsName           :: !(Maybe Text)
  , _rsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RotateSecretResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsrsVersionId' - The ID of the new version of the secret created by the rotation started by this request.
--
-- * 'rsrsARN' - The ARN of the secret.
--
-- * 'rsrsName' - The friendly name of the secret.
--
-- * 'rsrsResponseStatus' - -- | The response status code.
rotateSecretResponse
    :: Int -- ^ 'rsrsResponseStatus'
    -> RotateSecretResponse
rotateSecretResponse pResponseStatus_ =
  RotateSecretResponse'
    { _rsrsVersionId = Nothing
    , _rsrsARN = Nothing
    , _rsrsName = Nothing
    , _rsrsResponseStatus = pResponseStatus_
    }


-- | The ID of the new version of the secret created by the rotation started by this request.
rsrsVersionId :: Lens' RotateSecretResponse (Maybe Text)
rsrsVersionId = lens _rsrsVersionId (\ s a -> s{_rsrsVersionId = a})

-- | The ARN of the secret.
rsrsARN :: Lens' RotateSecretResponse (Maybe Text)
rsrsARN = lens _rsrsARN (\ s a -> s{_rsrsARN = a})

-- | The friendly name of the secret.
rsrsName :: Lens' RotateSecretResponse (Maybe Text)
rsrsName = lens _rsrsName (\ s a -> s{_rsrsName = a})

-- | -- | The response status code.
rsrsResponseStatus :: Lens' RotateSecretResponse Int
rsrsResponseStatus = lens _rsrsResponseStatus (\ s a -> s{_rsrsResponseStatus = a})

instance NFData RotateSecretResponse where
