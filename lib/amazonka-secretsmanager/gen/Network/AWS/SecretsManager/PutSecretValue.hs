{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.PutSecretValue
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--
--     * If another version of this secret already exists, then this operation does not automatically move any staging labels other than those that you explicitly specify in the @VersionStages@ parameter.
--
--
--     * If this operation moves the staging label @AWSCURRENT@ from another version to this version (because you included it in the @StagingLabels@ parameter) then Secrets Manager also automatically moves the staging label @AWSPREVIOUS@ to the version that @AWSCURRENT@ was removed from.
--
--
--     * This operation is idempotent. If a version with a @VersionId@ with the same value as the @ClientRequestToken@ parameter already exists and you specify the same secret data, the operation succeeds but does nothing. However, if the secret data is different, then the operation fails because you cannot modify an existing version; you can only create new ones.
--
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:PutSecretValue
--
--
--     * kms:GenerateDataKey - needed only if you use a customer-managed AWS KMS key to encrypt the secret. You do not need this permission to use the account's default AWS managed CMK for Secrets Manager.
--
--
-- __Related operations__
--
--     * To retrieve the encrypted value you store in the version of a secret, use 'GetSecretValue' .
--
--
--     * To create a secret, use 'CreateSecret' .
--
--
--     * To get the details for a secret, use 'DescribeSecret' .
--
--
--     * To list the versions attached to a secret, use 'ListSecretVersionIds' .
module Network.AWS.SecretsManager.PutSecretValue
  ( -- * Creating a request
    PutSecretValue (..),
    mkPutSecretValue,

    -- ** Request lenses
    psvVersionStages,
    psvSecretId,
    psvSecretBinary,
    psvSecretString,
    psvClientRequestToken,

    -- * Destructuring the response
    PutSecretValueResponse (..),
    mkPutSecretValueResponse,

    -- ** Response lenses
    psvrsVersionId,
    psvrsARN,
    psvrsVersionStages,
    psvrsName,
    psvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkPutSecretValue' smart constructor.
data PutSecretValue = PutSecretValue'
  { -- | (Optional) Specifies a list of staging labels that are attached to this version of the secret. These staging labels are used to track the versions through the rotation process by the Lambda rotation function.
    --
    -- A staging label must be unique to a single version of the secret. If you specify a staging label that's already associated with a different version of the same secret then that staging label is automatically removed from the other version and attached to this version.
    -- If you do not specify a value for @VersionStages@ then Secrets Manager automatically moves the staging label @AWSCURRENT@ to this new version.
    versionStages :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | Specifies the secret to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret. The secret must already exist.
    secretId :: Lude.Text,
    -- | (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
    --
    -- This parameter is not accessible if the secret using the Secrets Manager console.
    secretBinary :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    -- | (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty.
    --
    -- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
    -- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
    -- For example:
    -- @[{"username":"bob"},{"password":"abc123xyz456"}]@
    -- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
    secretString :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | (Optional) Specifies a unique identifier for the new version of the secret.
    --
    -- This value helps ensure idempotency. Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
    --
    --     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.
    --
    --
    --     * If a version with this value already exists and that version's @SecretString@ or @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).
    --
    --
    --     * If a version with this value already exists and the version of the @SecretString@ and @SecretBinary@ values are different from those in the request then the request fails because you cannot modify an existing secret version. You can only create new versions to store new secret values.
    --
    --
    -- This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSecretValue' with the minimum fields required to make a request.
--
-- * 'versionStages' - (Optional) Specifies a list of staging labels that are attached to this version of the secret. These staging labels are used to track the versions through the rotation process by the Lambda rotation function.
--
-- A staging label must be unique to a single version of the secret. If you specify a staging label that's already associated with a different version of the same secret then that staging label is automatically removed from the other version and attached to this version.
-- If you do not specify a value for @VersionStages@ then Secrets Manager automatically moves the staging label @AWSCURRENT@ to this new version.
-- * 'secretId' - Specifies the secret to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret. The secret must already exist.
-- * 'secretBinary' - (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- This parameter is not accessible if the secret using the Secrets Manager console.
--
-- * 'secretString' - (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty.
--
-- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
-- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
-- For example:
-- @[{"username":"bob"},{"password":"abc123xyz456"}]@
-- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
-- * 'clientRequestToken' - (Optional) Specifies a unique identifier for the new version of the secret.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
--
--     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.
--
--
--     * If a version with this value already exists and that version's @SecretString@ or @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).
--
--
--     * If a version with this value already exists and the version of the @SecretString@ and @SecretBinary@ values are different from those in the request then the request fails because you cannot modify an existing secret version. You can only create new versions to store new secret values.
--
--
-- This value becomes the @VersionId@ of the new version.
mkPutSecretValue ::
  -- | 'secretId'
  Lude.Text ->
  PutSecretValue
mkPutSecretValue pSecretId_ =
  PutSecretValue'
    { versionStages = Lude.Nothing,
      secretId = pSecretId_,
      secretBinary = Lude.Nothing,
      secretString = Lude.Nothing,
      clientRequestToken = Lude.Nothing
    }

-- | (Optional) Specifies a list of staging labels that are attached to this version of the secret. These staging labels are used to track the versions through the rotation process by the Lambda rotation function.
--
-- A staging label must be unique to a single version of the secret. If you specify a staging label that's already associated with a different version of the same secret then that staging label is automatically removed from the other version and attached to this version.
-- If you do not specify a value for @VersionStages@ then Secrets Manager automatically moves the staging label @AWSCURRENT@ to this new version.
--
-- /Note:/ Consider using 'versionStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvVersionStages :: Lens.Lens' PutSecretValue (Lude.Maybe (Lude.NonEmpty Lude.Text))
psvVersionStages = Lens.lens (versionStages :: PutSecretValue -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {versionStages = a} :: PutSecretValue)
{-# DEPRECATED psvVersionStages "Use generic-lens or generic-optics with 'versionStages' instead." #-}

-- | Specifies the secret to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret. The secret must already exist.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvSecretId :: Lens.Lens' PutSecretValue Lude.Text
psvSecretId = Lens.lens (secretId :: PutSecretValue -> Lude.Text) (\s a -> s {secretId = a} :: PutSecretValue)
{-# DEPRECATED psvSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- This parameter is not accessible if the secret using the Secrets Manager console.

----
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'secretBinary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvSecretBinary :: Lens.Lens' PutSecretValue (Lude.Maybe (Lude.Sensitive Lude.Base64))
psvSecretBinary = Lens.lens (secretBinary :: PutSecretValue -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {secretBinary = a} :: PutSecretValue)
{-# DEPRECATED psvSecretBinary "Use generic-lens or generic-optics with 'secretBinary' instead." #-}

-- | (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty.
--
-- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
-- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
-- For example:
-- @[{"username":"bob"},{"password":"abc123xyz456"}]@
-- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
--
-- /Note:/ Consider using 'secretString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvSecretString :: Lens.Lens' PutSecretValue (Lude.Maybe (Lude.Sensitive Lude.Text))
psvSecretString = Lens.lens (secretString :: PutSecretValue -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretString = a} :: PutSecretValue)
{-# DEPRECATED psvSecretString "Use generic-lens or generic-optics with 'secretString' instead." #-}

-- | (Optional) Specifies a unique identifier for the new version of the secret.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the Lambda rotation function's processing. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
--
--     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.
--
--
--     * If a version with this value already exists and that version's @SecretString@ or @SecretBinary@ values are the same as those in the request then the request is ignored (the operation is idempotent).
--
--
--     * If a version with this value already exists and the version of the @SecretString@ and @SecretBinary@ values are different from those in the request then the request fails because you cannot modify an existing secret version. You can only create new versions to store new secret values.
--
--
-- This value becomes the @VersionId@ of the new version.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvClientRequestToken :: Lens.Lens' PutSecretValue (Lude.Maybe Lude.Text)
psvClientRequestToken = Lens.lens (clientRequestToken :: PutSecretValue -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: PutSecretValue)
{-# DEPRECATED psvClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest PutSecretValue where
  type Rs PutSecretValue = PutSecretValueResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutSecretValueResponse'
            Lude.<$> (x Lude..?> "VersionId")
            Lude.<*> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "VersionStages")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutSecretValue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.PutSecretValue" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutSecretValue where
  toJSON PutSecretValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VersionStages" Lude..=) Lude.<$> versionStages,
            Lude.Just ("SecretId" Lude..= secretId),
            ("SecretBinary" Lude..=) Lude.<$> secretBinary,
            ("SecretString" Lude..=) Lude.<$> secretString,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath PutSecretValue where
  toPath = Lude.const "/"

instance Lude.ToQuery PutSecretValue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutSecretValueResponse' smart constructor.
data PutSecretValueResponse = PutSecretValueResponse'
  { -- | The unique identifier of the version of the secret you just created or updated.
    versionId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the secret for which you just created a version.
    arn :: Lude.Maybe Lude.Text,
    -- | The list of staging labels that are currently attached to this version of the secret. Staging labels are used to track a version as it progresses through the secret rotation process.
    versionStages :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The friendly name of the secret for which you just created or updated a version.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSecretValueResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The unique identifier of the version of the secret you just created or updated.
-- * 'arn' - The Amazon Resource Name (ARN) for the secret for which you just created a version.
-- * 'versionStages' - The list of staging labels that are currently attached to this version of the secret. Staging labels are used to track a version as it progresses through the secret rotation process.
-- * 'name' - The friendly name of the secret for which you just created or updated a version.
-- * 'responseStatus' - The response status code.
mkPutSecretValueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutSecretValueResponse
mkPutSecretValueResponse pResponseStatus_ =
  PutSecretValueResponse'
    { versionId = Lude.Nothing,
      arn = Lude.Nothing,
      versionStages = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier of the version of the secret you just created or updated.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrsVersionId :: Lens.Lens' PutSecretValueResponse (Lude.Maybe Lude.Text)
psvrsVersionId = Lens.lens (versionId :: PutSecretValueResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: PutSecretValueResponse)
{-# DEPRECATED psvrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The Amazon Resource Name (ARN) for the secret for which you just created a version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrsARN :: Lens.Lens' PutSecretValueResponse (Lude.Maybe Lude.Text)
psvrsARN = Lens.lens (arn :: PutSecretValueResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PutSecretValueResponse)
{-# DEPRECATED psvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The list of staging labels that are currently attached to this version of the secret. Staging labels are used to track a version as it progresses through the secret rotation process.
--
-- /Note:/ Consider using 'versionStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrsVersionStages :: Lens.Lens' PutSecretValueResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
psvrsVersionStages = Lens.lens (versionStages :: PutSecretValueResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {versionStages = a} :: PutSecretValueResponse)
{-# DEPRECATED psvrsVersionStages "Use generic-lens or generic-optics with 'versionStages' instead." #-}

-- | The friendly name of the secret for which you just created or updated a version.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrsName :: Lens.Lens' PutSecretValueResponse (Lude.Maybe Lude.Text)
psvrsName = Lens.lens (name :: PutSecretValueResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PutSecretValueResponse)
{-# DEPRECATED psvrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrsResponseStatus :: Lens.Lens' PutSecretValueResponse Lude.Int
psvrsResponseStatus = Lens.lens (responseStatus :: PutSecretValueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutSecretValueResponse)
{-# DEPRECATED psvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
