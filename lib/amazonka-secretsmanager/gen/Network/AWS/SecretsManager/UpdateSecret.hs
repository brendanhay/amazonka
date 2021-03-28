{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.SecretsManager.UpdateSecret
    (
    -- * Creating a request
      UpdateSecret (..)
    , mkUpdateSecret
    -- ** Request lenses
    , usSecretId
    , usClientRequestToken
    , usDescription
    , usKmsKeyId
    , usSecretBinary
    , usSecretString

    -- * Destructuring the response
    , UpdateSecretResponse (..)
    , mkUpdateSecretResponse
    -- ** Response lenses
    , usrrsARN
    , usrrsName
    , usrrsVersionId
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkUpdateSecret' smart constructor.
data UpdateSecret = UpdateSecret'
  { secretId :: Types.SecretIdType
    -- ^ Specifies the secret that you want to modify or to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
  , clientRequestToken :: Core.Maybe Types.ClientRequestTokenType
    -- ^ (Optional) If you want to add a new version to the secret, this parameter specifies a unique identifier for the new version that helps ensure idempotency. 
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
  , description :: Core.Maybe Types.DescriptionType
    -- ^ (Optional) Specifies an updated user-provided description of the secret.
  , kmsKeyId :: Core.Maybe Types.KmsKeyIdType
    -- ^ (Optional) Specifies an updated ARN or alias of the AWS KMS customer master key (CMK) to be used to encrypt the protected text in new versions of this secret.
--
-- /Important:/ You can only use the account's default CMK to encrypt and decrypt if you call this operation using credentials from the same account that owns the secret. If the secret is in a different account, then you must create a custom CMK and provide the ARN of that CMK in this field. The user making the call must have permissions to both the secret and the CMK in their respective accounts.
  , secretBinary :: Core.Maybe (Core.Sensitive Core.Base64)
    -- ^ (Optional) Specifies updated binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- This parameter is not accessible using the Secrets Manager console.
  , secretString :: Core.Maybe Types.SecretStringType
    -- ^ (Optional) Specifies updated text data that you want to encrypt and store in this new version of the secret. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
-- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
-- @[{"username":"bob"},{"password":"abc123xyz456"}]@ 
-- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text. You can also 'escape' the double quote character in the embedded JSON text by prefacing each with a backslash. For example, the following string is surrounded by double-quotes. All of the embedded double quotes are escaped:
-- @"[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]"@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecret' value with any optional fields omitted.
mkUpdateSecret
    :: Types.SecretIdType -- ^ 'secretId'
    -> UpdateSecret
mkUpdateSecret secretId
  = UpdateSecret'{secretId, clientRequestToken = Core.Nothing,
                  description = Core.Nothing, kmsKeyId = Core.Nothing,
                  secretBinary = Core.Nothing, secretString = Core.Nothing}

-- | Specifies the secret that you want to modify or to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSecretId :: Lens.Lens' UpdateSecret Types.SecretIdType
usSecretId = Lens.field @"secretId"
{-# INLINEABLE usSecretId #-}
{-# DEPRECATED secretId "Use generic-lens or generic-optics with 'secretId' instead"  #-}

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
usClientRequestToken :: Lens.Lens' UpdateSecret (Core.Maybe Types.ClientRequestTokenType)
usClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE usClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | (Optional) Specifies an updated user-provided description of the secret.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDescription :: Lens.Lens' UpdateSecret (Core.Maybe Types.DescriptionType)
usDescription = Lens.field @"description"
{-# INLINEABLE usDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | (Optional) Specifies an updated ARN or alias of the AWS KMS customer master key (CMK) to be used to encrypt the protected text in new versions of this secret.
--
-- /Important:/ You can only use the account's default CMK to encrypt and decrypt if you call this operation using credentials from the same account that owns the secret. If the secret is in a different account, then you must create a custom CMK and provide the ARN of that CMK in this field. The user making the call must have permissions to both the secret and the CMK in their respective accounts.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usKmsKeyId :: Lens.Lens' UpdateSecret (Core.Maybe Types.KmsKeyIdType)
usKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE usKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | (Optional) Specifies updated binary data that you want to encrypt and store in the new version of the secret. To use this parameter in the command-line tools, we recommend that you store your binary data in a file and then use the appropriate technique for your tool to pass the contents of the file as a parameter. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- This parameter is not accessible using the Secrets Manager console.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'secretBinary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSecretBinary :: Lens.Lens' UpdateSecret (Core.Maybe (Core.Sensitive Core.Base64))
usSecretBinary = Lens.field @"secretBinary"
{-# INLINEABLE usSecretBinary #-}
{-# DEPRECATED secretBinary "Use generic-lens or generic-optics with 'secretBinary' instead"  #-}

-- | (Optional) Specifies updated text data that you want to encrypt and store in this new version of the secret. Either @SecretBinary@ or @SecretString@ must have a value, but not both. They cannot both be empty.
--
-- If you create this secret by using the Secrets Manager console then Secrets Manager puts the protected secret text in only the @SecretString@ parameter. The Secrets Manager console stores the information as a JSON structure of key/value pairs that the default Lambda rotation function knows how to parse.
-- For storing multiple values, we recommend that you use a JSON text string argument and specify key/value pairs. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For example:
-- @[{"username":"bob"},{"password":"abc123xyz456"}]@ 
-- If your command-line tool or SDK requires quotation marks around the parameter, you should use single quotes to avoid confusion with the double quotes required in the JSON text. You can also 'escape' the double quote character in the embedded JSON text by prefacing each with a backslash. For example, the following string is surrounded by double-quotes. All of the embedded double quotes are escaped:
-- @"[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]"@ 
--
-- /Note:/ Consider using 'secretString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSecretString :: Lens.Lens' UpdateSecret (Core.Maybe Types.SecretStringType)
usSecretString = Lens.field @"secretString"
{-# INLINEABLE usSecretString #-}
{-# DEPRECATED secretString "Use generic-lens or generic-optics with 'secretString' instead"  #-}

instance Core.ToQuery UpdateSecret where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSecret where
        toHeaders UpdateSecret{..}
          = Core.pure ("X-Amz-Target", "secretsmanager.UpdateSecret") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSecret where
        toJSON UpdateSecret{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SecretId" Core..= secretId),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Description" Core..=) Core.<$> description,
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
                  ("SecretBinary" Core..=) Core.<$> secretBinary,
                  ("SecretString" Core..=) Core.<$> secretString])

instance Core.AWSRequest UpdateSecret where
        type Rs UpdateSecret = UpdateSecretResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateSecretResponse' Core.<$>
                   (x Core..:? "ARN") Core.<*> x Core..:? "Name" Core.<*>
                     x Core..:? "VersionId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSecretResponse' smart constructor.
data UpdateSecretResponse = UpdateSecretResponse'
  { arn :: Core.Maybe Types.ARN
    -- ^ The ARN of the secret that was updated.
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name of the secret that was updated.
  , versionId :: Core.Maybe Types.VersionId
    -- ^ If a new version of the secret was created by this operation, then @VersionId@ contains the unique identifier of the new version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecretResponse' value with any optional fields omitted.
mkUpdateSecretResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSecretResponse
mkUpdateSecretResponse responseStatus
  = UpdateSecretResponse'{arn = Core.Nothing, name = Core.Nothing,
                          versionId = Core.Nothing, responseStatus}

-- | The ARN of the secret that was updated.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsARN :: Lens.Lens' UpdateSecretResponse (Core.Maybe Types.ARN)
usrrsARN = Lens.field @"arn"
{-# INLINEABLE usrrsARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The friendly name of the secret that was updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsName :: Lens.Lens' UpdateSecretResponse (Core.Maybe Types.Name)
usrrsName = Lens.field @"name"
{-# INLINEABLE usrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | If a new version of the secret was created by this operation, then @VersionId@ contains the unique identifier of the new version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsVersionId :: Lens.Lens' UpdateSecretResponse (Core.Maybe Types.VersionId)
usrrsVersionId = Lens.field @"versionId"
{-# INLINEABLE usrrsVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSecretResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
