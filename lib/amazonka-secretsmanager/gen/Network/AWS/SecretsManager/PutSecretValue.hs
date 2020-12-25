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
    psvSecretId,
    psvClientRequestToken,
    psvSecretBinary,
    psvSecretString,
    psvVersionStages,

    -- * Destructuring the response
    PutSecretValueResponse (..),
    mkPutSecretValueResponse,

    -- ** Response lenses
    psvrrsARN,
    psvrrsName,
    psvrrsVersionId,
    psvrrsVersionStages,
    psvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkPutSecretValue' smart constructor.
data PutSecretValue = PutSecretValue'
  { -- | Specifies the secret to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret. The secret must already exist.
    secretId :: Types.SecretId,
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
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
    --
    -- This parameter is not accessible if the secret using the Secrets Manager console.
    secretBinary :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | (Optional) Specifies text data that you want to encrypt and store in this new version of the secret. Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty.
    --
    -- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
    -- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
    -- For example:
    -- @[{"username":"bob"},{"password":"abc123xyz456"}]@
    -- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
    secretString :: Core.Maybe Types.SecretString,
    -- | (Optional) Specifies a list of staging labels that are attached to this version of the secret. These staging labels are used to track the versions through the rotation process by the Lambda rotation function.
    --
    -- A staging label must be unique to a single version of the secret. If you specify a staging label that's already associated with a different version of the same secret then that staging label is automatically removed from the other version and attached to this version.
    -- If you do not specify a value for @VersionStages@ then Secrets Manager automatically moves the staging label @AWSCURRENT@ to this new version.
    versionStages :: Core.Maybe (Core.NonEmpty Types.SecretVersionStageType)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSecretValue' value with any optional fields omitted.
mkPutSecretValue ::
  -- | 'secretId'
  Types.SecretId ->
  PutSecretValue
mkPutSecretValue secretId =
  PutSecretValue'
    { secretId,
      clientRequestToken = Core.Nothing,
      secretBinary = Core.Nothing,
      secretString = Core.Nothing,
      versionStages = Core.Nothing
    }

-- | Specifies the secret to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret. The secret must already exist.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvSecretId :: Lens.Lens' PutSecretValue Types.SecretId
psvSecretId = Lens.field @"secretId"
{-# DEPRECATED psvSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

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
psvClientRequestToken :: Lens.Lens' PutSecretValue (Core.Maybe Types.ClientRequestToken)
psvClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED psvClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

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
psvSecretBinary :: Lens.Lens' PutSecretValue (Core.Maybe (Core.Sensitive Core.Base64))
psvSecretBinary = Lens.field @"secretBinary"
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
psvSecretString :: Lens.Lens' PutSecretValue (Core.Maybe Types.SecretString)
psvSecretString = Lens.field @"secretString"
{-# DEPRECATED psvSecretString "Use generic-lens or generic-optics with 'secretString' instead." #-}

-- | (Optional) Specifies a list of staging labels that are attached to this version of the secret. These staging labels are used to track the versions through the rotation process by the Lambda rotation function.
--
-- A staging label must be unique to a single version of the secret. If you specify a staging label that's already associated with a different version of the same secret then that staging label is automatically removed from the other version and attached to this version.
-- If you do not specify a value for @VersionStages@ then Secrets Manager automatically moves the staging label @AWSCURRENT@ to this new version.
--
-- /Note:/ Consider using 'versionStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvVersionStages :: Lens.Lens' PutSecretValue (Core.Maybe (Core.NonEmpty Types.SecretVersionStageType))
psvVersionStages = Lens.field @"versionStages"
{-# DEPRECATED psvVersionStages "Use generic-lens or generic-optics with 'versionStages' instead." #-}

instance Core.FromJSON PutSecretValue where
  toJSON PutSecretValue {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SecretId" Core..= secretId),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("SecretBinary" Core..=) Core.<$> secretBinary,
            ("SecretString" Core..=) Core.<$> secretString,
            ("VersionStages" Core..=) Core.<$> versionStages
          ]
      )

instance Core.AWSRequest PutSecretValue where
  type Rs PutSecretValue = PutSecretValueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.PutSecretValue")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSecretValueResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "VersionId")
            Core.<*> (x Core..:? "VersionStages")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutSecretValueResponse' smart constructor.
data PutSecretValueResponse = PutSecretValueResponse'
  { -- | The Amazon Resource Name (ARN) for the secret for which you just created a version.
    arn :: Core.Maybe Types.SecretARNType,
    -- | The friendly name of the secret for which you just created or updated a version.
    name :: Core.Maybe Types.Name,
    -- | The unique identifier of the version of the secret you just created or updated.
    versionId :: Core.Maybe Types.SecretVersionIdType,
    -- | The list of staging labels that are currently attached to this version of the secret. Staging labels are used to track a version as it progresses through the secret rotation process.
    versionStages :: Core.Maybe (Core.NonEmpty Types.SecretVersionStageType),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSecretValueResponse' value with any optional fields omitted.
mkPutSecretValueResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutSecretValueResponse
mkPutSecretValueResponse responseStatus =
  PutSecretValueResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      versionId = Core.Nothing,
      versionStages = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) for the secret for which you just created a version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrrsARN :: Lens.Lens' PutSecretValueResponse (Core.Maybe Types.SecretARNType)
psvrrsARN = Lens.field @"arn"
{-# DEPRECATED psvrrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret for which you just created or updated a version.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrrsName :: Lens.Lens' PutSecretValueResponse (Core.Maybe Types.Name)
psvrrsName = Lens.field @"name"
{-# DEPRECATED psvrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier of the version of the secret you just created or updated.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrrsVersionId :: Lens.Lens' PutSecretValueResponse (Core.Maybe Types.SecretVersionIdType)
psvrrsVersionId = Lens.field @"versionId"
{-# DEPRECATED psvrrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The list of staging labels that are currently attached to this version of the secret. Staging labels are used to track a version as it progresses through the secret rotation process.
--
-- /Note:/ Consider using 'versionStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrrsVersionStages :: Lens.Lens' PutSecretValueResponse (Core.Maybe (Core.NonEmpty Types.SecretVersionStageType))
psvrrsVersionStages = Lens.field @"versionStages"
{-# DEPRECATED psvrrsVersionStages "Use generic-lens or generic-optics with 'versionStages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvrrsResponseStatus :: Lens.Lens' PutSecretValueResponse Core.Int
psvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED psvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
