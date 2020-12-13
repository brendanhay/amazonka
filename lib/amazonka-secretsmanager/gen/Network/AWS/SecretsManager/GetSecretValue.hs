{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.GetSecretValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of the encrypted fields @SecretString@ or @SecretBinary@ from the specified version of a secret, whichever contains content.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:GetSecretValue
--
--
--     * kms:Decrypt - required only if you use a customer-managed AWS KMS key to encrypt the secret. You do not need this permission to use the account's default AWS managed CMK for Secrets Manager.
--
--
-- __Related operations__
--
--     * To create a new version of the secret with different encrypted information, use 'PutSecretValue' .
--
--
--     * To retrieve the non-encrypted details for the secret, use 'DescribeSecret' .
module Network.AWS.SecretsManager.GetSecretValue
  ( -- * Creating a request
    GetSecretValue (..),
    mkGetSecretValue,

    -- ** Request lenses
    gsvVersionId,
    gsvSecretId,
    gsvVersionStage,

    -- * Destructuring the response
    GetSecretValueResponse (..),
    mkGetSecretValueResponse,

    -- ** Response lenses
    gsvrsVersionId,
    gsvrsARN,
    gsvrsVersionStages,
    gsvrsSecretBinary,
    gsvrsCreatedDate,
    gsvrsName,
    gsvrsSecretString,
    gsvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkGetSecretValue' smart constructor.
data GetSecretValue = GetSecretValue'
  { -- | Specifies the unique identifier of the version of the secret that you want to retrieve. If you specify this parameter then don't specify @VersionStage@ . If you don't specify either a @VersionStage@ or @VersionId@ then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
    --
    -- This value is typically a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value with 32 hexadecimal digits.
    versionId :: Lude.Maybe Lude.Text,
    -- | Specifies the secret containing the version that you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Lude.Text,
    -- | Specifies the secret version that you want to retrieve by the staging label attached to the version.
    --
    -- Staging labels are used to keep track of different versions during the rotation process. If you use this parameter then don't specify @VersionId@ . If you don't specify either a @VersionStage@ or @VersionId@ , then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
    versionStage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSecretValue' with the minimum fields required to make a request.
--
-- * 'versionId' - Specifies the unique identifier of the version of the secret that you want to retrieve. If you specify this parameter then don't specify @VersionStage@ . If you don't specify either a @VersionStage@ or @VersionId@ then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
--
-- This value is typically a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value with 32 hexadecimal digits.
-- * 'secretId' - Specifies the secret containing the version that you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
-- * 'versionStage' - Specifies the secret version that you want to retrieve by the staging label attached to the version.
--
-- Staging labels are used to keep track of different versions during the rotation process. If you use this parameter then don't specify @VersionId@ . If you don't specify either a @VersionStage@ or @VersionId@ , then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
mkGetSecretValue ::
  -- | 'secretId'
  Lude.Text ->
  GetSecretValue
mkGetSecretValue pSecretId_ =
  GetSecretValue'
    { versionId = Lude.Nothing,
      secretId = pSecretId_,
      versionStage = Lude.Nothing
    }

-- | Specifies the unique identifier of the version of the secret that you want to retrieve. If you specify this parameter then don't specify @VersionStage@ . If you don't specify either a @VersionStage@ or @VersionId@ then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
--
-- This value is typically a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value with 32 hexadecimal digits.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvVersionId :: Lens.Lens' GetSecretValue (Lude.Maybe Lude.Text)
gsvVersionId = Lens.lens (versionId :: GetSecretValue -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: GetSecretValue)
{-# DEPRECATED gsvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Specifies the secret containing the version that you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSecretId :: Lens.Lens' GetSecretValue Lude.Text
gsvSecretId = Lens.lens (secretId :: GetSecretValue -> Lude.Text) (\s a -> s {secretId = a} :: GetSecretValue)
{-# DEPRECATED gsvSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | Specifies the secret version that you want to retrieve by the staging label attached to the version.
--
-- Staging labels are used to keep track of different versions during the rotation process. If you use this parameter then don't specify @VersionId@ . If you don't specify either a @VersionStage@ or @VersionId@ , then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
--
-- /Note:/ Consider using 'versionStage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvVersionStage :: Lens.Lens' GetSecretValue (Lude.Maybe Lude.Text)
gsvVersionStage = Lens.lens (versionStage :: GetSecretValue -> Lude.Maybe Lude.Text) (\s a -> s {versionStage = a} :: GetSecretValue)
{-# DEPRECATED gsvVersionStage "Use generic-lens or generic-optics with 'versionStage' instead." #-}

instance Lude.AWSRequest GetSecretValue where
  type Rs GetSecretValue = GetSecretValueResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSecretValueResponse'
            Lude.<$> (x Lude..?> "VersionId")
            Lude.<*> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "VersionStages")
            Lude.<*> (x Lude..?> "SecretBinary")
            Lude.<*> (x Lude..?> "CreatedDate")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "SecretString")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSecretValue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.GetSecretValue" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSecretValue where
  toJSON GetSecretValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VersionId" Lude..=) Lude.<$> versionId,
            Lude.Just ("SecretId" Lude..= secretId),
            ("VersionStage" Lude..=) Lude.<$> versionStage
          ]
      )

instance Lude.ToPath GetSecretValue where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSecretValue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSecretValueResponse' smart constructor.
data GetSecretValueResponse = GetSecretValueResponse'
  { -- | The unique identifier of this version of the secret.
    versionId :: Lude.Maybe Lude.Text,
    -- | The ARN of the secret.
    arn :: Lude.Maybe Lude.Text,
    -- | A list of all of the staging labels currently attached to this version of the secret.
    versionStages :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The decrypted part of the protected secret information that was originally provided as binary data in the form of a byte array. The response parameter represents the binary data as a <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
    --
    -- This parameter is not used if the secret is created by the Secrets Manager console.
    -- If you store custom information in this field of the secret, then you must code your Lambda rotation function to parse and interpret whatever you store in the @SecretString@ or @SecretBinary@ fields.
    secretBinary :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    -- | The date and time that this version of the secret was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The friendly name of the secret.
    name :: Lude.Maybe Lude.Text,
    -- | The decrypted part of the protected secret information that was originally provided as a string.
    --
    -- If you create this secret by using the Secrets Manager console then only the @SecretString@ parameter contains data. Secrets Manager stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse.
    -- If you store custom information in the secret by using the 'CreateSecret' , 'UpdateSecret' , or 'PutSecretValue' API operations instead of the Secrets Manager console, or by using the __Other secret type__ in the console, then you must code your Lambda rotation function to parse and interpret those values.
    secretString :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSecretValueResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The unique identifier of this version of the secret.
-- * 'arn' - The ARN of the secret.
-- * 'versionStages' - A list of all of the staging labels currently attached to this version of the secret.
-- * 'secretBinary' - The decrypted part of the protected secret information that was originally provided as binary data in the form of a byte array. The response parameter represents the binary data as a <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
--
-- This parameter is not used if the secret is created by the Secrets Manager console.
-- If you store custom information in this field of the secret, then you must code your Lambda rotation function to parse and interpret whatever you store in the @SecretString@ or @SecretBinary@ fields.
-- * 'createdDate' - The date and time that this version of the secret was created.
-- * 'name' - The friendly name of the secret.
-- * 'secretString' - The decrypted part of the protected secret information that was originally provided as a string.
--
-- If you create this secret by using the Secrets Manager console then only the @SecretString@ parameter contains data. Secrets Manager stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse.
-- If you store custom information in the secret by using the 'CreateSecret' , 'UpdateSecret' , or 'PutSecretValue' API operations instead of the Secrets Manager console, or by using the __Other secret type__ in the console, then you must code your Lambda rotation function to parse and interpret those values.
-- * 'responseStatus' - The response status code.
mkGetSecretValueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSecretValueResponse
mkGetSecretValueResponse pResponseStatus_ =
  GetSecretValueResponse'
    { versionId = Lude.Nothing,
      arn = Lude.Nothing,
      versionStages = Lude.Nothing,
      secretBinary = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      secretString = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier of this version of the secret.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsVersionId :: Lens.Lens' GetSecretValueResponse (Lude.Maybe Lude.Text)
gsvrsVersionId = Lens.lens (versionId :: GetSecretValueResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: GetSecretValueResponse)
{-# DEPRECATED gsvrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ARN of the secret.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsARN :: Lens.Lens' GetSecretValueResponse (Lude.Maybe Lude.Text)
gsvrsARN = Lens.lens (arn :: GetSecretValueResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetSecretValueResponse)
{-# DEPRECATED gsvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of all of the staging labels currently attached to this version of the secret.
--
-- /Note:/ Consider using 'versionStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsVersionStages :: Lens.Lens' GetSecretValueResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
gsvrsVersionStages = Lens.lens (versionStages :: GetSecretValueResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {versionStages = a} :: GetSecretValueResponse)
{-# DEPRECATED gsvrsVersionStages "Use generic-lens or generic-optics with 'versionStages' instead." #-}

-- | The decrypted part of the protected secret information that was originally provided as binary data in the form of a byte array. The response parameter represents the binary data as a <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
--
-- This parameter is not used if the secret is created by the Secrets Manager console.
-- If you store custom information in this field of the secret, then you must code your Lambda rotation function to parse and interpret whatever you store in the @SecretString@ or @SecretBinary@ fields.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'secretBinary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsSecretBinary :: Lens.Lens' GetSecretValueResponse (Lude.Maybe (Lude.Sensitive Lude.Base64))
gsvrsSecretBinary = Lens.lens (secretBinary :: GetSecretValueResponse -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {secretBinary = a} :: GetSecretValueResponse)
{-# DEPRECATED gsvrsSecretBinary "Use generic-lens or generic-optics with 'secretBinary' instead." #-}

-- | The date and time that this version of the secret was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsCreatedDate :: Lens.Lens' GetSecretValueResponse (Lude.Maybe Lude.Timestamp)
gsvrsCreatedDate = Lens.lens (createdDate :: GetSecretValueResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetSecretValueResponse)
{-# DEPRECATED gsvrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The friendly name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsName :: Lens.Lens' GetSecretValueResponse (Lude.Maybe Lude.Text)
gsvrsName = Lens.lens (name :: GetSecretValueResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetSecretValueResponse)
{-# DEPRECATED gsvrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The decrypted part of the protected secret information that was originally provided as a string.
--
-- If you create this secret by using the Secrets Manager console then only the @SecretString@ parameter contains data. Secrets Manager stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse.
-- If you store custom information in the secret by using the 'CreateSecret' , 'UpdateSecret' , or 'PutSecretValue' API operations instead of the Secrets Manager console, or by using the __Other secret type__ in the console, then you must code your Lambda rotation function to parse and interpret those values.
--
-- /Note:/ Consider using 'secretString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsSecretString :: Lens.Lens' GetSecretValueResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
gsvrsSecretString = Lens.lens (secretString :: GetSecretValueResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretString = a} :: GetSecretValueResponse)
{-# DEPRECATED gsvrsSecretString "Use generic-lens or generic-optics with 'secretString' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsResponseStatus :: Lens.Lens' GetSecretValueResponse Lude.Int
gsvrsResponseStatus = Lens.lens (responseStatus :: GetSecretValueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSecretValueResponse)
{-# DEPRECATED gsvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
