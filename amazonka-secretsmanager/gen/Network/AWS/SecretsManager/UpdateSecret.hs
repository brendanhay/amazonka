{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.UpdateSecret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies many of the details of the specified secret. If you include a
-- @ClientRequestToken@ and /either/ @SecretString@ or @SecretBinary@ then
-- it also creates a new version attached to the secret.
--
-- To modify the rotation configuration of a secret, use RotateSecret
-- instead.
--
-- The Secrets Manager console uses only the @SecretString@ parameter and
-- therefore limits you to encrypting and storing only a text string. To
-- encrypt and store binary data as part of the version of a secret, you
-- must use either the AWS CLI or one of the AWS SDKs.
--
-- -   If a version with a @VersionId@ with the same value as the
--     @ClientRequestToken@ parameter already exists, the operation results
--     in an error. You cannot modify an existing version, you can only
--     create a new version.
--
-- -   If you include @SecretString@ or @SecretBinary@ to create a new
--     secret version, Secrets Manager automatically attaches the staging
--     label @AWSCURRENT@ to the new version.
--
-- -   If you call an operation to encrypt or decrypt the @SecretString@ or
--     @SecretBinary@ for a secret in the same account as the calling user
--     and that secret doesn\'t specify a AWS KMS encryption key, Secrets
--     Manager uses the account\'s default AWS managed customer master key
--     (CMK) with the alias @aws\/secretsmanager@. If this key doesn\'t
--     already exist in your account then Secrets Manager creates it for
--     you automatically. All users and roles in the same AWS account
--     automatically have access to use the default CMK. Note that if an
--     Secrets Manager API call results in AWS creating the account\'s
--     AWS-managed CMK, it can result in a one-time significant delay in
--     returning the result.
--
-- -   If the secret resides in a different AWS account from the
--     credentials calling an API that requires encryption or decryption of
--     the secret value then you must create and use a custom AWS KMS CMK
--     because you can\'t access the default CMK for the account using
--     credentials from a different AWS account. Store the ARN of the CMK
--     in the secret when you create the secret or when you update it by
--     including it in the @KMSKeyId@. If you call an API that must encrypt
--     or decrypt @SecretString@ or @SecretBinary@ using credentials from a
--     different account then the AWS KMS key policy must grant
--     cross-account access to that other account\'s user or role for both
--     the kms:GenerateDataKey and kms:Decrypt operations.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:UpdateSecret
--
-- -   kms:GenerateDataKey - needed only if you use a custom AWS KMS key to
--     encrypt the secret. You do not need this permission to use the
--     account\'s AWS managed CMK for Secrets Manager.
--
-- -   kms:Decrypt - needed only if you use a custom AWS KMS key to encrypt
--     the secret. You do not need this permission to use the account\'s
--     AWS managed CMK for Secrets Manager.
--
-- __Related operations__
--
-- -   To create a new secret, use CreateSecret.
--
-- -   To add only a new version to an existing secret, use PutSecretValue.
--
-- -   To get the details for a secret, use DescribeSecret.
--
-- -   To list the versions contained in a secret, use
--     ListSecretVersionIds.
module Network.AWS.SecretsManager.UpdateSecret
  ( -- * Creating a Request
    UpdateSecret (..),
    newUpdateSecret,

    -- * Request Lenses
    updateSecret_secretBinary,
    updateSecret_kmsKeyId,
    updateSecret_description,
    updateSecret_secretString,
    updateSecret_clientRequestToken,
    updateSecret_secretId,

    -- * Destructuring the Response
    UpdateSecretResponse (..),
    newUpdateSecretResponse,

    -- * Response Lenses
    updateSecretResponse_arn,
    updateSecretResponse_versionId,
    updateSecretResponse_name,
    updateSecretResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newUpdateSecret' smart constructor.
data UpdateSecret = UpdateSecret'
  { -- | (Optional) Specifies updated binary data that you want to encrypt and
    -- store in the new version of the secret. To use this parameter in the
    -- command-line tools, we recommend that you store your binary data in a
    -- file and then use the appropriate technique for your tool to pass the
    -- contents of the file as a parameter. Either @SecretBinary@ or
    -- @SecretString@ must have a value, but not both. They cannot both be
    -- empty.
    --
    -- This parameter is not accessible using the Secrets Manager console.
    secretBinary :: Prelude.Maybe (Core.Sensitive Core.Base64),
    -- | (Optional) Specifies an updated ARN or alias of the AWS KMS customer
    -- master key (CMK) to be used to encrypt the protected text in new
    -- versions of this secret.
    --
    -- You can only use the account\'s default CMK to encrypt and decrypt if
    -- you call this operation using credentials from the same account that
    -- owns the secret. If the secret is in a different account, then you must
    -- create a custom CMK and provide the ARN of that CMK in this field. The
    -- user making the call must have permissions to both the secret and the
    -- CMK in their respective accounts.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Specifies an updated user-provided description of the secret.
    description :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Specifies updated text data that you want to encrypt and
    -- store in this new version of the secret. Either @SecretBinary@ or
    -- @SecretString@ must have a value, but not both. They cannot both be
    -- empty.
    --
    -- If you create this secret by using the Secrets Manager console then
    -- Secrets Manager puts the protected secret text in only the
    -- @SecretString@ parameter. The Secrets Manager console stores the
    -- information as a JSON structure of key\/value pairs that the default
    -- Lambda rotation function knows how to parse.
    --
    -- For storing multiple values, we recommend that you use a JSON text
    -- string argument and specify key\/value pairs. For information on how to
    -- format a JSON parameter for the various command line tool environments,
    -- see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
    -- in the /AWS CLI User Guide/. For example:
    --
    -- @[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]@
    --
    -- If your command-line tool or SDK requires quotation marks around the
    -- parameter, you should use single quotes to avoid confusion with the
    -- double quotes required in the JSON text. You can also \'escape\' the
    -- double quote character in the embedded JSON text by prefacing each with
    -- a backslash. For example, the following string is surrounded by
    -- double-quotes. All of the embedded double quotes are escaped:
    --
    -- @\"[{\\\"username\\\":\\\"bob\\\"},{\\\"password\\\":\\\"abc123xyz456\\\"}]\"@
    secretString :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | (Optional) If you want to add a new version to the secret, this
    -- parameter specifies a unique identifier for the new version that helps
    -- ensure idempotency.
    --
    -- If you use the AWS CLI or one of the AWS SDK to call this operation,
    -- then you can leave this parameter empty. The CLI or SDK generates a
    -- random UUID for you and includes that in the request. If you don\'t use
    -- the SDK and instead generate a raw HTTP request to the Secrets Manager
    -- service endpoint, then you must generate a @ClientRequestToken@ yourself
    -- for new versions and include that value in the request.
    --
    -- You typically only need to interact with this value if you implement
    -- your own retry logic and want to ensure that a given secret is not
    -- created twice. We recommend that you generate a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
    -- value to ensure uniqueness within the specified secret.
    --
    -- Secrets Manager uses this value to prevent the accidental creation of
    -- duplicate versions if there are failures and retries during the Lambda
    -- rotation function\'s processing.
    --
    -- -   If the @ClientRequestToken@ value isn\'t already associated with a
    --     version of the secret then a new version of the secret is created.
    --
    -- -   If a version with this value already exists and that version\'s
    --     @SecretString@ and @SecretBinary@ values are the same as those in
    --     the request then the request is ignored (the operation is
    --     idempotent).
    --
    -- -   If a version with this value already exists and that version\'s
    --     @SecretString@ and @SecretBinary@ values are different from the
    --     request then an error occurs because you cannot modify an existing
    --     secret value.
    --
    -- This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the secret that you want to modify or to which you want to add
    -- a new version. You can specify either the Amazon Resource Name (ARN) or
    -- the friendly name of the secret.
    --
    -- If you specify an ARN, we generally recommend that you specify a
    -- complete ARN. You can specify a partial ARN too—for example, if you
    -- don’t include the final hyphen and six random characters that Secrets
    -- Manager adds at the end of the ARN when you created the secret. A
    -- partial ARN match can work as long as it uniquely matches only one
    -- secret. However, if your secret has a name that ends in a hyphen
    -- followed by six characters (before Secrets Manager adds the hyphen and
    -- six characters to the ARN) and you try to use that as a partial ARN,
    -- then those characters cause Secrets Manager to assume that you’re
    -- specifying a complete ARN. This confusion can cause unexpected results.
    -- To avoid this situation, we recommend that you don’t create secret names
    -- ending with a hyphen followed by six characters.
    --
    -- If you specify an incomplete ARN without the random suffix, and instead
    -- provide the \'friendly name\', you /must/ not include the random suffix.
    -- If you do include the random suffix added by Secrets Manager, you
    -- receive either a /ResourceNotFoundException/ or an
    -- /AccessDeniedException/ error, depending on your permissions.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretBinary', 'updateSecret_secretBinary' - (Optional) Specifies updated binary data that you want to encrypt and
-- store in the new version of the secret. To use this parameter in the
-- command-line tools, we recommend that you store your binary data in a
-- file and then use the appropriate technique for your tool to pass the
-- contents of the file as a parameter. Either @SecretBinary@ or
-- @SecretString@ must have a value, but not both. They cannot both be
-- empty.
--
-- This parameter is not accessible using the Secrets Manager console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'kmsKeyId', 'updateSecret_kmsKeyId' - (Optional) Specifies an updated ARN or alias of the AWS KMS customer
-- master key (CMK) to be used to encrypt the protected text in new
-- versions of this secret.
--
-- You can only use the account\'s default CMK to encrypt and decrypt if
-- you call this operation using credentials from the same account that
-- owns the secret. If the secret is in a different account, then you must
-- create a custom CMK and provide the ARN of that CMK in this field. The
-- user making the call must have permissions to both the secret and the
-- CMK in their respective accounts.
--
-- 'description', 'updateSecret_description' - (Optional) Specifies an updated user-provided description of the secret.
--
-- 'secretString', 'updateSecret_secretString' - (Optional) Specifies updated text data that you want to encrypt and
-- store in this new version of the secret. Either @SecretBinary@ or
-- @SecretString@ must have a value, but not both. They cannot both be
-- empty.
--
-- If you create this secret by using the Secrets Manager console then
-- Secrets Manager puts the protected secret text in only the
-- @SecretString@ parameter. The Secrets Manager console stores the
-- information as a JSON structure of key\/value pairs that the default
-- Lambda rotation function knows how to parse.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For information on how to
-- format a JSON parameter for the various command line tool environments,
-- see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /AWS CLI User Guide/. For example:
--
-- @[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]@
--
-- If your command-line tool or SDK requires quotation marks around the
-- parameter, you should use single quotes to avoid confusion with the
-- double quotes required in the JSON text. You can also \'escape\' the
-- double quote character in the embedded JSON text by prefacing each with
-- a backslash. For example, the following string is surrounded by
-- double-quotes. All of the embedded double quotes are escaped:
--
-- @\"[{\\\"username\\\":\\\"bob\\\"},{\\\"password\\\":\\\"abc123xyz456\\\"}]\"@
--
-- 'clientRequestToken', 'updateSecret_clientRequestToken' - (Optional) If you want to add a new version to the secret, this
-- parameter specifies a unique identifier for the new version that helps
-- ensure idempotency.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation,
-- then you can leave this parameter empty. The CLI or SDK generates a
-- random UUID for you and includes that in the request. If you don\'t use
-- the SDK and instead generate a raw HTTP request to the Secrets Manager
-- service endpoint, then you must generate a @ClientRequestToken@ yourself
-- for new versions and include that value in the request.
--
-- You typically only need to interact with this value if you implement
-- your own retry logic and want to ensure that a given secret is not
-- created twice. We recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- Secrets Manager uses this value to prevent the accidental creation of
-- duplicate versions if there are failures and retries during the Lambda
-- rotation function\'s processing.
--
-- -   If the @ClientRequestToken@ value isn\'t already associated with a
--     version of the secret then a new version of the secret is created.
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ and @SecretBinary@ values are the same as those in
--     the request then the request is ignored (the operation is
--     idempotent).
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ and @SecretBinary@ values are different from the
--     request then an error occurs because you cannot modify an existing
--     secret value.
--
-- This value becomes the @VersionId@ of the new version.
--
-- 'secretId', 'updateSecret_secretId' - Specifies the secret that you want to modify or to which you want to add
-- a new version. You can specify either the Amazon Resource Name (ARN) or
-- the friendly name of the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
newUpdateSecret ::
  -- | 'secretId'
  Prelude.Text ->
  UpdateSecret
newUpdateSecret pSecretId_ =
  UpdateSecret'
    { secretBinary = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      description = Prelude.Nothing,
      secretString = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | (Optional) Specifies updated binary data that you want to encrypt and
-- store in the new version of the secret. To use this parameter in the
-- command-line tools, we recommend that you store your binary data in a
-- file and then use the appropriate technique for your tool to pass the
-- contents of the file as a parameter. Either @SecretBinary@ or
-- @SecretString@ must have a value, but not both. They cannot both be
-- empty.
--
-- This parameter is not accessible using the Secrets Manager console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateSecret_secretBinary :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.ByteString)
updateSecret_secretBinary = Lens.lens (\UpdateSecret' {secretBinary} -> secretBinary) (\s@UpdateSecret' {} a -> s {secretBinary = a} :: UpdateSecret) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

-- | (Optional) Specifies an updated ARN or alias of the AWS KMS customer
-- master key (CMK) to be used to encrypt the protected text in new
-- versions of this secret.
--
-- You can only use the account\'s default CMK to encrypt and decrypt if
-- you call this operation using credentials from the same account that
-- owns the secret. If the secret is in a different account, then you must
-- create a custom CMK and provide the ARN of that CMK in this field. The
-- user making the call must have permissions to both the secret and the
-- CMK in their respective accounts.
updateSecret_kmsKeyId :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.Text)
updateSecret_kmsKeyId = Lens.lens (\UpdateSecret' {kmsKeyId} -> kmsKeyId) (\s@UpdateSecret' {} a -> s {kmsKeyId = a} :: UpdateSecret)

-- | (Optional) Specifies an updated user-provided description of the secret.
updateSecret_description :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.Text)
updateSecret_description = Lens.lens (\UpdateSecret' {description} -> description) (\s@UpdateSecret' {} a -> s {description = a} :: UpdateSecret)

-- | (Optional) Specifies updated text data that you want to encrypt and
-- store in this new version of the secret. Either @SecretBinary@ or
-- @SecretString@ must have a value, but not both. They cannot both be
-- empty.
--
-- If you create this secret by using the Secrets Manager console then
-- Secrets Manager puts the protected secret text in only the
-- @SecretString@ parameter. The Secrets Manager console stores the
-- information as a JSON structure of key\/value pairs that the default
-- Lambda rotation function knows how to parse.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For information on how to
-- format a JSON parameter for the various command line tool environments,
-- see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /AWS CLI User Guide/. For example:
--
-- @[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]@
--
-- If your command-line tool or SDK requires quotation marks around the
-- parameter, you should use single quotes to avoid confusion with the
-- double quotes required in the JSON text. You can also \'escape\' the
-- double quote character in the embedded JSON text by prefacing each with
-- a backslash. For example, the following string is surrounded by
-- double-quotes. All of the embedded double quotes are escaped:
--
-- @\"[{\\\"username\\\":\\\"bob\\\"},{\\\"password\\\":\\\"abc123xyz456\\\"}]\"@
updateSecret_secretString :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.Text)
updateSecret_secretString = Lens.lens (\UpdateSecret' {secretString} -> secretString) (\s@UpdateSecret' {} a -> s {secretString = a} :: UpdateSecret) Prelude.. Lens.mapping Core._Sensitive

-- | (Optional) If you want to add a new version to the secret, this
-- parameter specifies a unique identifier for the new version that helps
-- ensure idempotency.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation,
-- then you can leave this parameter empty. The CLI or SDK generates a
-- random UUID for you and includes that in the request. If you don\'t use
-- the SDK and instead generate a raw HTTP request to the Secrets Manager
-- service endpoint, then you must generate a @ClientRequestToken@ yourself
-- for new versions and include that value in the request.
--
-- You typically only need to interact with this value if you implement
-- your own retry logic and want to ensure that a given secret is not
-- created twice. We recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- Secrets Manager uses this value to prevent the accidental creation of
-- duplicate versions if there are failures and retries during the Lambda
-- rotation function\'s processing.
--
-- -   If the @ClientRequestToken@ value isn\'t already associated with a
--     version of the secret then a new version of the secret is created.
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ and @SecretBinary@ values are the same as those in
--     the request then the request is ignored (the operation is
--     idempotent).
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ and @SecretBinary@ values are different from the
--     request then an error occurs because you cannot modify an existing
--     secret value.
--
-- This value becomes the @VersionId@ of the new version.
updateSecret_clientRequestToken :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.Text)
updateSecret_clientRequestToken = Lens.lens (\UpdateSecret' {clientRequestToken} -> clientRequestToken) (\s@UpdateSecret' {} a -> s {clientRequestToken = a} :: UpdateSecret)

-- | Specifies the secret that you want to modify or to which you want to add
-- a new version. You can specify either the Amazon Resource Name (ARN) or
-- the friendly name of the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
updateSecret_secretId :: Lens.Lens' UpdateSecret Prelude.Text
updateSecret_secretId = Lens.lens (\UpdateSecret' {secretId} -> secretId) (\s@UpdateSecret' {} a -> s {secretId = a} :: UpdateSecret)

instance Core.AWSRequest UpdateSecret where
  type AWSResponse UpdateSecret = UpdateSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecretResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "VersionId")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecret

instance Prelude.NFData UpdateSecret

instance Core.ToHeaders UpdateSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.UpdateSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSecret where
  toJSON UpdateSecret' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecretBinary" Core..=) Prelude.<$> secretBinary,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Description" Core..=) Prelude.<$> description,
            ("SecretString" Core..=) Prelude.<$> secretString,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath UpdateSecret where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecretResponse' smart constructor.
data UpdateSecretResponse = UpdateSecretResponse'
  { -- | The ARN of the secret that was updated.
    --
    -- Secrets Manager automatically adds several random characters to the name
    -- at the end of the ARN when you initially create a secret. This affects
    -- only the ARN and not the actual friendly name. This ensures that if you
    -- create a new secret with the same name as an old secret that you
    -- previously deleted, then users with access to the old secret /don\'t/
    -- automatically get access to the new secret because the ARNs are
    -- different.
    arn :: Prelude.Maybe Prelude.Text,
    -- | If a new version of the secret was created by this operation, then
    -- @VersionId@ contains the unique identifier of the new version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret that was updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateSecretResponse_arn' - The ARN of the secret that was updated.
--
-- Secrets Manager automatically adds several random characters to the name
-- at the end of the ARN when you initially create a secret. This affects
-- only the ARN and not the actual friendly name. This ensures that if you
-- create a new secret with the same name as an old secret that you
-- previously deleted, then users with access to the old secret /don\'t/
-- automatically get access to the new secret because the ARNs are
-- different.
--
-- 'versionId', 'updateSecretResponse_versionId' - If a new version of the secret was created by this operation, then
-- @VersionId@ contains the unique identifier of the new version.
--
-- 'name', 'updateSecretResponse_name' - The friendly name of the secret that was updated.
--
-- 'httpStatus', 'updateSecretResponse_httpStatus' - The response's http status code.
newUpdateSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecretResponse
newUpdateSecretResponse pHttpStatus_ =
  UpdateSecretResponse'
    { arn = Prelude.Nothing,
      versionId = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret that was updated.
--
-- Secrets Manager automatically adds several random characters to the name
-- at the end of the ARN when you initially create a secret. This affects
-- only the ARN and not the actual friendly name. This ensures that if you
-- create a new secret with the same name as an old secret that you
-- previously deleted, then users with access to the old secret /don\'t/
-- automatically get access to the new secret because the ARNs are
-- different.
updateSecretResponse_arn :: Lens.Lens' UpdateSecretResponse (Prelude.Maybe Prelude.Text)
updateSecretResponse_arn = Lens.lens (\UpdateSecretResponse' {arn} -> arn) (\s@UpdateSecretResponse' {} a -> s {arn = a} :: UpdateSecretResponse)

-- | If a new version of the secret was created by this operation, then
-- @VersionId@ contains the unique identifier of the new version.
updateSecretResponse_versionId :: Lens.Lens' UpdateSecretResponse (Prelude.Maybe Prelude.Text)
updateSecretResponse_versionId = Lens.lens (\UpdateSecretResponse' {versionId} -> versionId) (\s@UpdateSecretResponse' {} a -> s {versionId = a} :: UpdateSecretResponse)

-- | The friendly name of the secret that was updated.
updateSecretResponse_name :: Lens.Lens' UpdateSecretResponse (Prelude.Maybe Prelude.Text)
updateSecretResponse_name = Lens.lens (\UpdateSecretResponse' {name} -> name) (\s@UpdateSecretResponse' {} a -> s {name = a} :: UpdateSecretResponse)

-- | The response's http status code.
updateSecretResponse_httpStatus :: Lens.Lens' UpdateSecretResponse Prelude.Int
updateSecretResponse_httpStatus = Lens.lens (\UpdateSecretResponse' {httpStatus} -> httpStatus) (\s@UpdateSecretResponse' {} a -> s {httpStatus = a} :: UpdateSecretResponse)

instance Prelude.NFData UpdateSecretResponse
