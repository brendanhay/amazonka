{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.UpdateSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies many of the details of the specified secret. If you include a @ClientRequestToken@ and /either/ @SecretString@ or @SecretBinary@ then it also creates a new version attached to the secret.
--
-- To modify the rotation configuration of a secret, use 'RotateSecret' instead.
--
--     * If a version with a @VersionId@ with the same value as the @ClientRequestToken@ parameter already exists, the operation results in an error. You cannot modify an existing version, you can only create a new version.
--
--
--     * If you include @SecretString@ or @SecretBinary@ to create a new secret version, Secrets Manager automatically attaches the staging label @AWSCURRENT@ to the new version.
--
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:UpdateSecret
--
--
--     * kms:GenerateDataKey - needed only if you use a custom AWS KMS key to encrypt the secret. You do not need this permission to use the account's AWS managed CMK for Secrets Manager.
--
--
--     * kms:Decrypt - needed only if you use a custom AWS KMS key to encrypt the secret. You do not need this permission to use the account's AWS managed CMK for Secrets Manager.
--
--
-- __Related operations__
--
--     * To create a new secret, use 'CreateSecret' .
--
--
--     * To add only a new version to an existing secret, use 'PutSecretValue' .
--
--
--     * To get the details for a secret, use 'DescribeSecret' .
--
--
--     * To list the versions contained in a secret, use 'ListSecretVersionIds' .
module Network.AWS.SecretsManager.UpdateSecret
  ( -- * Creating a request
    UpdateSecret (..),
    mkUpdateSecret,

    -- ** Request lenses
    usSecretId,
    usSecretBinary,
    usKMSKeyId,
    usSecretString,
    usClientRequestToken,
    usDescription,

    -- * Destructuring the response
    UpdateSecretResponse (..),
    mkUpdateSecretResponse,

    -- ** Response lenses
    usrsVersionId,
    usrsARN,
    usrsName,
    usrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkUpdateSecret' smart constructor.
data UpdateSecret = UpdateSecret'
  { -- | Specifies the secret that you want to modify or to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Lude.Text,
    -- | (Optional) Specifies updated binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
    --
    -- This parameter is not accessible using the Secrets Manager console.
    secretBinary :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    -- | (Optional) Specifies an updated ARN or alias of the AWS KMS customer master key (CMK) to be used to encrypt the protected text in new versions of this secret.
    --
    -- /Important:/ You can only use the account's default CMK to encrypt and decrypt if you call this operation using credentials from the same account that owns the secret. If the secret is in a different account, then you must create a custom CMK and provide the ARN of that CMK in this field. The user making the call must have permissions to both the secret and the CMK in their respective accounts.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | (Optional) Specifies updated text data that you want to encrypt and store in this new version of the secret. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
    --
    -- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
    -- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
    -- @[{"username":"bob"},{"password":"abc123xyz456"}]@
    -- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text. You can also 'escape' the double quote character in the embedded JSON text by prefacing each with a backslash. For example, the following string is surrounded by double-quotes. All of the embedded double quotes are escaped:
    -- @"[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]"@
    secretString :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | (Optional) If you want to add a new version to the secret, this parameter specifies a unique identifier for the new version that helps ensure idempotency.
    --
    -- If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request.
    -- You typically only need to interact with this value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
    -- Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing.
    --
    --     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.
    --
    --
    --     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).
    --
    --
    --     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from the request then an error occurs because you cannot modify an existing secret value.
    --
    --
    -- This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | (Optional) Specifies an updated user-provided description of the secret.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecret' with the minimum fields required to make a request.
--
-- * 'secretId' - Specifies the secret that you want to modify or to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
-- * 'secretBinary' - (Optional) Specifies updated binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- This parameter is not accessible using the Secrets Manager console.
-- * 'kmsKeyId' - (Optional) Specifies an updated ARN or alias of the AWS KMS customer master key (CMK) to be used to encrypt the protected text in new versions of this secret.
--
-- /Important:/ You can only use the account's default CMK to encrypt and decrypt if you call this operation using credentials from the same account that owns the secret. If the secret is in a different account, then you must create a custom CMK and provide the ARN of that CMK in this field. The user making the call must have permissions to both the secret and the CMK in their respective accounts.
-- * 'secretString' - (Optional) Specifies updated text data that you want to encrypt and store in this new version of the secret. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
-- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
-- @[{"username":"bob"},{"password":"abc123xyz456"}]@
-- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text. You can also 'escape' the double quote character in the embedded JSON text by prefacing each with a backslash. For example, the following string is surrounded by double-quotes. All of the embedded double quotes are escaped:
-- @"[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]"@
-- * 'clientRequestToken' - (Optional) If you want to add a new version to the secret, this parameter specifies a unique identifier for the new version that helps ensure idempotency.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request.
-- You typically only need to interact with this value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
-- Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing.
--
--     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.
--
--
--     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).
--
--
--     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from the request then an error occurs because you cannot modify an existing secret value.
--
--
-- This value becomes the @VersionId@ of the new version.
-- * 'description' - (Optional) Specifies an updated user-provided description of the secret.
mkUpdateSecret ::
  -- | 'secretId'
  Lude.Text ->
  UpdateSecret
mkUpdateSecret pSecretId_ =
  UpdateSecret'
    { secretId = pSecretId_,
      secretBinary = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      secretString = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Specifies the secret that you want to modify or to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSecretId :: Lens.Lens' UpdateSecret Lude.Text
usSecretId = Lens.lens (secretId :: UpdateSecret -> Lude.Text) (\s a -> s {secretId = a} :: UpdateSecret)
{-# DEPRECATED usSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | (Optional) Specifies updated binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- This parameter is not accessible using the Secrets Manager console.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'secretBinary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSecretBinary :: Lens.Lens' UpdateSecret (Lude.Maybe (Lude.Sensitive Lude.Base64))
usSecretBinary = Lens.lens (secretBinary :: UpdateSecret -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {secretBinary = a} :: UpdateSecret)
{-# DEPRECATED usSecretBinary "Use generic-lens or generic-optics with 'secretBinary' instead." #-}

-- | (Optional) Specifies an updated ARN or alias of the AWS KMS customer master key (CMK) to be used to encrypt the protected text in new versions of this secret.
--
-- /Important:/ You can only use the account's default CMK to encrypt and decrypt if you call this operation using credentials from the same account that owns the secret. If the secret is in a different account, then you must create a custom CMK and provide the ARN of that CMK in this field. The user making the call must have permissions to both the secret and the CMK in their respective accounts.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usKMSKeyId :: Lens.Lens' UpdateSecret (Lude.Maybe Lude.Text)
usKMSKeyId = Lens.lens (kmsKeyId :: UpdateSecret -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: UpdateSecret)
{-# DEPRECATED usKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | (Optional) Specifies updated text data that you want to encrypt and store in this new version of the secret. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
-- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
-- @[{"username":"bob"},{"password":"abc123xyz456"}]@
-- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text. You can also 'escape' the double quote character in the embedded JSON text by prefacing each with a backslash. For example, the following string is surrounded by double-quotes. All of the embedded double quotes are escaped:
-- @"[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]"@
--
-- /Note:/ Consider using 'secretString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSecretString :: Lens.Lens' UpdateSecret (Lude.Maybe (Lude.Sensitive Lude.Text))
usSecretString = Lens.lens (secretString :: UpdateSecret -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretString = a} :: UpdateSecret)
{-# DEPRECATED usSecretString "Use generic-lens or generic-optics with 'secretString' instead." #-}

-- | (Optional) If you want to add a new version to the secret, this parameter specifies a unique identifier for the new version that helps ensure idempotency.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request.
-- You typically only need to interact with this value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
-- Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing.
--
--     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.
--
--
--     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).
--
--
--     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from the request then an error occurs because you cannot modify an existing secret value.
--
--
-- This value becomes the @VersionId@ of the new version.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usClientRequestToken :: Lens.Lens' UpdateSecret (Lude.Maybe Lude.Text)
usClientRequestToken = Lens.lens (clientRequestToken :: UpdateSecret -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: UpdateSecret)
{-# DEPRECATED usClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | (Optional) Specifies an updated user-provided description of the secret.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDescription :: Lens.Lens' UpdateSecret (Lude.Maybe Lude.Text)
usDescription = Lens.lens (description :: UpdateSecret -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateSecret)
{-# DEPRECATED usDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateSecret where
  type Rs UpdateSecret = UpdateSecretResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSecretResponse'
            Lude.<$> (x Lude..?> "VersionId")
            Lude.<*> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSecret where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.UpdateSecret" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSecret where
  toJSON UpdateSecret' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SecretId" Lude..= secretId),
            ("SecretBinary" Lude..=) Lude.<$> secretBinary,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("SecretString" Lude..=) Lude.<$> secretString,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateSecret where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSecret where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSecretResponse' smart constructor.
data UpdateSecretResponse = UpdateSecretResponse'
  { -- | If a new version of the secret was created by this operation, then @VersionId@ contains the unique identifier of the new version.
    versionId :: Lude.Maybe Lude.Text,
    -- | The ARN of the secret that was updated.
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the secret that was updated.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecretResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - If a new version of the secret was created by this operation, then @VersionId@ contains the unique identifier of the new version.
-- * 'arn' - The ARN of the secret that was updated.
-- * 'name' - The friendly name of the secret that was updated.
-- * 'responseStatus' - The response status code.
mkUpdateSecretResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSecretResponse
mkUpdateSecretResponse pResponseStatus_ =
  UpdateSecretResponse'
    { versionId = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a new version of the secret was created by this operation, then @VersionId@ contains the unique identifier of the new version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsVersionId :: Lens.Lens' UpdateSecretResponse (Lude.Maybe Lude.Text)
usrsVersionId = Lens.lens (versionId :: UpdateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: UpdateSecretResponse)
{-# DEPRECATED usrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ARN of the secret that was updated.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsARN :: Lens.Lens' UpdateSecretResponse (Lude.Maybe Lude.Text)
usrsARN = Lens.lens (arn :: UpdateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UpdateSecretResponse)
{-# DEPRECATED usrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that was updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsName :: Lens.Lens' UpdateSecretResponse (Lude.Maybe Lude.Text)
usrsName = Lens.lens (name :: UpdateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateSecretResponse)
{-# DEPRECATED usrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateSecretResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateSecretResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSecretResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
