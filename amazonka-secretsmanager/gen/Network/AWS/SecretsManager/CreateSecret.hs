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
-- Module      : Network.AWS.SecretsManager.CreateSecret
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new secret. A secret in Secrets Manager consists of both the protected secret data and the important information needed to manage the secret.
--
--
-- Secrets Manager stores the encrypted secret data in one of a collection of "versions" associated with the secret. Each version contains a copy of the encrypted secret data. Each version is associated with one or more "staging labels" that identify where the version is in the rotation cycle. The @SecretVersionsToStages@ field of the secret contains the mapping of staging labels to the active versions of the secret. Versions without a staging label are considered deprecated and are not included in the list.
--
-- You provide the secret data to be encrypted by putting text in either the @SecretString@ parameter or binary data in the @SecretBinary@ parameter, but not both. If you include @SecretString@ or @SecretBinary@ then Secrets Manager also creates an initial secret version and automatically attaches the staging label @AWSCURRENT@ to the new version.
--
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:CreateSecret
--
--     * kms:GenerateDataKey - needed only if you use a customer-managed AWS KMS key to encrypt the secret. You do not need this permission to use the account's default AWS managed CMK for Secrets Manager.
--
--     * kms:Decrypt - needed only if you use a customer-managed AWS KMS key to encrypt the secret. You do not need this permission to use the account's default AWS managed CMK for Secrets Manager.
--
--     * secretsmanager:TagResource - needed only if you include the @Tags@ parameter.
--
--
--
-- __Related operations__
--
--     * To delete a secret, use 'DeleteSecret' .
--
--     * To modify an existing secret, use 'UpdateSecret' .
--
--     * To create a new version of a secret, use 'PutSecretValue' .
--
--     * To retrieve the encrypted secure string and secure binary values, use 'GetSecretValue' .
--
--     * To retrieve all other details for a secret, use 'DescribeSecret' . This does not include the encrypted secure string and secure binary values.
--
--     * To retrieve the list of secret versions associated with the current secret, use 'DescribeSecret' and examine the @SecretVersionsToStages@ response value.
--
--
--
module Network.AWS.SecretsManager.CreateSecret
    (
    -- * Creating a Request
      createSecret
    , CreateSecret
    -- * Request Lenses
    , csSecretBinary
    , csKMSKeyId
    , csSecretString
    , csClientRequestToken
    , csDescription
    , csTags
    , csName

    -- * Destructuring the Response
    , createSecretResponse
    , CreateSecretResponse
    -- * Response Lenses
    , csrsVersionId
    , csrsARN
    , csrsName
    , csrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'createSecret' smart constructor.
data CreateSecret = CreateSecret'
  { _csSecretBinary       :: !(Maybe (Sensitive Base64))
  , _csKMSKeyId           :: !(Maybe Text)
  , _csSecretString       :: !(Maybe (Sensitive Text))
  , _csClientRequestToken :: !(Maybe Text)
  , _csDescription        :: !(Maybe Text)
  , _csTags               :: !(Maybe [Tag])
  , _csName               :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSecret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csSecretBinary' - (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty. This parameter is not available using the Secrets Manager console. It can be accessed only by using the AWS CLI or one of the AWS SDKs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'csKMSKeyId' - (Optional) Specifies the ARN, Key ID, or alias of the AWS KMS customer master key (CMK) to be used to encrypt the @SecretString@ or @SecretBinary@ values in the versions stored in this secret. You can specify any of the supported ways to identify a AWS KMS key ID. If you need to reference a CMK in a different account, you can use only the key ARN or the alias ARN. If you don't specify this value, then Secrets Manager defaults to using the AWS account's default CMK (the one named @aws/secretsmanager@ ). If a AWS KMS CMK with that name doesn't yet exist, then Secrets Manager creates it for you automatically the first time it needs to encrypt a version's @SecretString@ or @SecretBinary@ fields. /Important:/ You can use the account's default CMK to encrypt and decrypt only if you call this operation using credentials from the same account that owns the secret. If the secret is in a different account, then you must create a custom CMK and specify the ARN in this field.
--
-- * 'csSecretString' - (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty. If you create a secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse. For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example: @[{"username":"bob"},{"password":"abc123xyz456"}]@  If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
--
-- * 'csClientRequestToken' - (Optional) If you include @SecretString@ or @SecretBinary@ , then an initial version is created as part of the secret, and this parameter specifies a unique identifier for the new version.  This value helps ensure idempotency. Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during a rotation. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness of your versions within the specified secret.      * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.      * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are the same as those in the request, then the request is ignored (the operation is idempotent).     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from those in the request then the request fails because you cannot modify an existing version. Instead, use 'PutSecretValue' to create a new version. This value becomes the @VersionId@ of the new version.
--
-- * 'csDescription' - (Optional) Specifies a user-provided description of the secret.
--
-- * 'csTags' - (Optional) Specifies a list of user-defined tags that are attached to the secret. Each tag is a "Key" and "Value" pair of strings. This operation only appends tags to the existing list of tags. To remove tags, you must use 'UntagResource' . /Important:/     * Secrets Manager tag key names are case sensitive. A tag with the key "ABC" is a different tag from one with key "abc".     * If you check tags in IAM policy @Condition@ elements as part of your security strategy, then adding or removing a tag can change permissions. If the successful completion of this operation would result in you losing your permissions for this secret, then this operation is blocked and returns an @Access Denied@ error. This parameter requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example: @[{"Key":"CostCenter","Value":"12345"},{"Key":"environment","Value":"production"}]@  If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.  The following basic restrictions apply to tags:     * Maximum number of tags per secret
