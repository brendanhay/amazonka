{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.CreateSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new secret. A secret in Secrets Manager consists of both the protected secret data and the important information needed to manage the secret.
--
-- Secrets Manager stores the encrypted secret data in one of a collection of "versions" associated with the secret. Each version contains a copy of the encrypted secret data. Each version is associated with one or more "staging labels" that identify where the version is in the rotation cycle. The @SecretVersionsToStages@ field of the secret contains the mapping of staging labels to the active versions of the secret. Versions without a staging label are considered deprecated and not included in the list.
-- You provide the secret data to be encrypted by putting text in either the @SecretString@ parameter or binary data in the @SecretBinary@ parameter, but not both. If you include @SecretString@ or @SecretBinary@ then Secrets Manager also creates an initial secret version and automatically attaches the staging label @AWSCURRENT@ to the new version.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:CreateSecret
--
--
--     * kms:GenerateDataKey - needed only if you use a customer-managed AWS KMS key to encrypt the secret. You do not need this permission to use the account default AWS managed CMK for Secrets Manager.
--
--
--     * kms:Decrypt - needed only if you use a customer-managed AWS KMS key to encrypt the secret. You do not need this permission to use the account default AWS managed CMK for Secrets Manager.
--
--
--     * secretsmanager:TagResource - needed only if you include the @Tags@ parameter.
--
--
-- __Related operations__
--
--     * To delete a secret, use 'DeleteSecret' .
--
--
--     * To modify an existing secret, use 'UpdateSecret' .
--
--
--     * To create a new version of a secret, use 'PutSecretValue' .
--
--
--     * To retrieve the encrypted secure string and secure binary values, use 'GetSecretValue' .
--
--
--     * To retrieve all other details for a secret, use 'DescribeSecret' . This does not include the encrypted secure string and secure binary values.
--
--
--     * To retrieve the list of secret versions associated with the current secret, use 'DescribeSecret' and examine the @SecretVersionsToStages@ response value.
module Network.AWS.SecretsManager.CreateSecret
  ( -- * Creating a request
    CreateSecret (..),
    mkCreateSecret,

    -- ** Request lenses
    csName,
    csClientRequestToken,
    csDescription,
    csKmsKeyId,
    csSecretBinary,
    csSecretString,
    csTags,

    -- * Destructuring the response
    CreateSecretResponse (..),
    mkCreateSecretResponse,

    -- ** Response lenses
    csrrsARN,
    csrrsName,
    csrrsVersionId,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkCreateSecret' smart constructor.
data CreateSecret = CreateSecret'
  { -- | Specifies the friendly name of the new secret.
    --
    -- The secret name must be ASCII letters, digits, or the following characters : /_+=.@-
    name :: Types.NameType,
    -- | (Optional) If you include @SecretString@ or @SecretBinary@ , then an initial version is created as part of the secret, and this parameter specifies a unique identifier for the new version.
    --
    -- This value helps ensure idempotency. Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during a rotation. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness of your versions within the specified secret.
    --
    --     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.
    --
    --
    --     * If a version with this value already exists and the version @SecretString@ and @SecretBinary@ values are the same as those in the request, then the request is ignored.
    --
    --
    --     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from those in the request then the request fails because you cannot modify an existing version. Instead, use 'PutSecretValue' to create a new version.
    --
    --
    -- This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Core.Maybe Types.ClientRequestTokenType,
    -- | (Optional) Specifies a user-provided description of the secret.
    description :: Core.Maybe Types.DescriptionType,
    -- | (Optional) Specifies the ARN, Key ID, or alias of the AWS KMS customer master key (CMK) to be used to encrypt the @SecretString@ or @SecretBinary@ values in the versions stored in this secret.
    --
    -- You can specify any of the supported ways to identify a AWS KMS key ID. If you need to reference a CMK in a different account, you can use only the key ARN or the alias ARN.
    -- If you don't specify this value, then Secrets Manager defaults to using the AWS account's default CMK (the one named @aws/secretsmanager@ ). If a AWS KMS CMK with that name doesn't yet exist, then Secrets Manager creates it for you automatically the first time it needs to encrypt a version's @SecretString@ or @SecretBinary@ fields.
    -- /Important:/ You can use the account default CMK to encrypt and decrypt only if you call this operation using credentials from the same account that owns the secret. If the secret resides in a different account, then you must create a custom CMK and specify the ARN in this field.
    kmsKeyId :: Core.Maybe Types.KmsKeyIdType,
    -- | (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter.
    --
    -- Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty.
    -- This parameter is not available using the Secrets Manager console. It can be accessed only by using the AWS CLI or one of the AWS SDKs.
    secretBinary :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | (Optional) Specifies text data that you want to encrypt and store in this new version of the secret.
    --
    -- Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty.
    -- If you create a secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse.
    -- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
    -- @{"username":"bob","password":"abc123xyz456"}@
    -- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
    secretString :: Core.Maybe Types.SecretStringType,
    -- | (Optional) Specifies a list of user-defined tags that are attached to the secret. Each tag is a "Key" and "Value" pair of strings. This operation only appends tags to the existing list of tags. To remove tags, you must use 'UntagResource' .
    --
    -- /Important:/
    --     * Secrets Manager tag key names are case sensitive. A tag with the key "ABC" is a different tag from one with key "abc".
    --
    --
    --     * If you check tags in IAM policy @Condition@ elements as part of your security strategy, then adding or removing a tag can change permissions. If the successful completion of this operation would result in you losing your permissions for this secret, then this operation is blocked and returns an @Access Denied@ error.
    --
    --
    -- This parameter requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
    -- @[{"Key":"CostCenter","Value":"12345"},{"Key":"environment","Value":"production"}]@
    -- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
    -- The following basic restrictions apply to tags:
    --
    --     * Maximum number of tags per secret—50
    --
    --
    --     * Maximum key length—127 Unicode characters in UTF-8
    --
    --
    --     * Maximum value length—255 Unicode characters in UTF-8
    --
    --
    --     * Tag keys and values are case sensitive.
    --
    --
    --     * Do not use the @aws:@ prefix in your tag names or values because AWS reserves it for AWS use. You can't edit or delete tag names or values with this prefix. Tags with this prefix do not count against your tags per secret limit.
    --
    --
    --     * If you use your tagging schema across multiple services and resources, remember other services might have restrictions on allowed characters. Generally allowed characters: letters, spaces, and numbers representable in UTF-8, plus the following special characters: + - = . _ : / @.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecret' value with any optional fields omitted.
mkCreateSecret ::
  -- | 'name'
  Types.NameType ->
  CreateSecret
mkCreateSecret name =
  CreateSecret'
    { name,
      clientRequestToken = Core.Nothing,
      description = Core.Nothing,
      kmsKeyId = Core.Nothing,
      secretBinary = Core.Nothing,
      secretString = Core.Nothing,
      tags = Core.Nothing
    }

-- | Specifies the friendly name of the new secret.
--
-- The secret name must be ASCII letters, digits, or the following characters : /_+=.@-
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateSecret Types.NameType
csName = Lens.field @"name"
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | (Optional) If you include @SecretString@ or @SecretBinary@ , then an initial version is created as part of the secret, and this parameter specifies a unique identifier for the new version.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during a rotation. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness of your versions within the specified secret.
--
--     * If the @ClientRequestToken@ value isn't already associated with a version of the secret then a new version of the secret is created.
--
--
--     * If a version with this value already exists and the version @SecretString@ and @SecretBinary@ values are the same as those in the request, then the request is ignored.
--
--
--     * If a version with this value already exists and that version's @SecretString@ and @SecretBinary@ values are different from those in the request then the request fails because you cannot modify an existing version. Instead, use 'PutSecretValue' to create a new version.
--
--
-- This value becomes the @VersionId@ of the new version.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClientRequestToken :: Lens.Lens' CreateSecret (Core.Maybe Types.ClientRequestTokenType)
csClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED csClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | (Optional) Specifies a user-provided description of the secret.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateSecret (Core.Maybe Types.DescriptionType)
csDescription = Lens.field @"description"
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | (Optional) Specifies the ARN, Key ID, or alias of the AWS KMS customer master key (CMK) to be used to encrypt the @SecretString@ or @SecretBinary@ values in the versions stored in this secret.
--
-- You can specify any of the supported ways to identify a AWS KMS key ID. If you need to reference a CMK in a different account, you can use only the key ARN or the alias ARN.
-- If you don't specify this value, then Secrets Manager defaults to using the AWS account's default CMK (the one named @aws/secretsmanager@ ). If a AWS KMS CMK with that name doesn't yet exist, then Secrets Manager creates it for you automatically the first time it needs to encrypt a version's @SecretString@ or @SecretBinary@ fields.
-- /Important:/ You can use the account default CMK to encrypt and decrypt only if you call this operation using credentials from the same account that owns the secret. If the secret resides in a different account, then you must create a custom CMK and specify the ARN in this field.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKmsKeyId :: Lens.Lens' CreateSecret (Core.Maybe Types.KmsKeyIdType)
csKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED csKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | (Optional) Specifies binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty.
-- This parameter is not available using the Secrets Manager console. It can be accessed only by using the AWS CLI or one of the AWS SDKs.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'secretBinary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSecretBinary :: Lens.Lens' CreateSecret (Core.Maybe (Core.Sensitive Core.Base64))
csSecretBinary = Lens.field @"secretBinary"
{-# DEPRECATED csSecretBinary "Use generic-lens or generic-optics with 'secretBinary' instead." #-}

-- | (Optional) Specifies text data that you want to encrypt and store in this new version of the secret.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both. They cannot both be empty.
-- If you create a secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse.
-- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
-- @{"username":"bob","password":"abc123xyz456"}@
-- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
--
-- /Note:/ Consider using 'secretString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSecretString :: Lens.Lens' CreateSecret (Core.Maybe Types.SecretStringType)
csSecretString = Lens.field @"secretString"
{-# DEPRECATED csSecretString "Use generic-lens or generic-optics with 'secretString' instead." #-}

-- | (Optional) Specifies a list of user-defined tags that are attached to the secret. Each tag is a "Key" and "Value" pair of strings. This operation only appends tags to the existing list of tags. To remove tags, you must use 'UntagResource' .
--
-- /Important:/
--     * Secrets Manager tag key names are case sensitive. A tag with the key "ABC" is a different tag from one with key "abc".
--
--
--     * If you check tags in IAM policy @Condition@ elements as part of your security strategy, then adding or removing a tag can change permissions. If the successful completion of this operation would result in you losing your permissions for this secret, then this operation is blocked and returns an @Access Denied@ error.
--
--
-- This parameter requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
-- @[{"Key":"CostCenter","Value":"12345"},{"Key":"environment","Value":"production"}]@
-- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text.
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per secret—50
--
--
--     * Maximum key length—127 Unicode characters in UTF-8
--
--
--     * Maximum value length—255 Unicode characters in UTF-8
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Do not use the @aws:@ prefix in your tag names or values because AWS reserves it for AWS use. You can't edit or delete tag names or values with this prefix. Tags with this prefix do not count against your tags per secret limit.
--
--
--     * If you use your tagging schema across multiple services and resources, remember other services might have restrictions on allowed characters. Generally allowed characters: letters, spaces, and numbers representable in UTF-8, plus the following special characters: + - = . _ : / @.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateSecret (Core.Maybe [Types.Tag])
csTags = Lens.field @"tags"
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateSecret where
  toJSON CreateSecret {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("Description" Core..=) Core.<$> description,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("SecretBinary" Core..=) Core.<$> secretBinary,
            ("SecretString" Core..=) Core.<$> secretString,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateSecret where
  type Rs CreateSecret = CreateSecretResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.CreateSecret")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecretResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "VersionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSecretResponse' smart constructor.
data CreateSecretResponse = CreateSecretResponse'
  { -- | The Amazon Resource Name (ARN) of the secret that you just created.
    arn :: Core.Maybe Types.SecretARNType,
    -- | The friendly name of the secret that you just created.
    name :: Core.Maybe Types.Name,
    -- | The unique identifier associated with the version of the secret you just created.
    versionId :: Core.Maybe Types.SecretVersionIdType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecretResponse' value with any optional fields omitted.
mkCreateSecretResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSecretResponse
mkCreateSecretResponse responseStatus =
  CreateSecretResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      versionId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the secret that you just created.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsARN :: Lens.Lens' CreateSecretResponse (Core.Maybe Types.SecretARNType)
csrrsARN = Lens.field @"arn"
{-# DEPRECATED csrrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that you just created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsName :: Lens.Lens' CreateSecretResponse (Core.Maybe Types.Name)
csrrsName = Lens.field @"name"
{-# DEPRECATED csrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier associated with the version of the secret you just created.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsVersionId :: Lens.Lens' CreateSecretResponse (Core.Maybe Types.SecretVersionIdType)
csrrsVersionId = Lens.field @"versionId"
{-# DEPRECATED csrrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSecretResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
