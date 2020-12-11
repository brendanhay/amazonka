{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateFunctionCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function's code. If code signing is enabled for the function, the code package must be signed by a trusted publisher. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-trustedcode.html Configuring code signing> .
--
-- The function's code is locked when you publish a version. You can't modify the code of a published version, only the unpublished version.
module Network.AWS.Lambda.UpdateFunctionCode
  ( -- * Creating a request
    UpdateFunctionCode (..),
    mkUpdateFunctionCode,

    -- ** Request lenses
    uS3ObjectVersion,
    uS3Key,
    uZipFile,
    uS3Bucket,
    uDryRun,
    uRevisionId,
    uPublish,
    uFunctionName,

    -- * Destructuring the response
    FunctionConfiguration (..),
    mkFunctionConfiguration,

    -- ** Response lenses
    fcMemorySize,
    fcRuntime,
    fcState,
    fcSigningProfileVersionARN,
    fcLastUpdateStatus,
    fcFunctionARN,
    fcKMSKeyARN,
    fcFileSystemConfigs,
    fcEnvironment,
    fcDeadLetterConfig,
    fcSigningJobARN,
    fcRole,
    fcVPCConfig,
    fcVersion,
    fcFunctionName,
    fcLayers,
    fcCodeSize,
    fcHandler,
    fcTimeout,
    fcLastUpdateStatusReason,
    fcStateReason,
    fcLastModified,
    fcCodeSha256,
    fcTracingConfig,
    fcStateReasonCode,
    fcDescription,
    fcLastUpdateStatusReasonCode,
    fcRevisionId,
    fcMasterARN,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFunctionCode' smart constructor.
data UpdateFunctionCode = UpdateFunctionCode'
  { s3ObjectVersion ::
      Lude.Maybe Lude.Text,
    s3Key :: Lude.Maybe Lude.Text,
    zipFile :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    s3Bucket :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    revisionId :: Lude.Maybe Lude.Text,
    publish :: Lude.Maybe Lude.Bool,
    functionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFunctionCode' with the minimum fields required to make a request.
--
-- * 'dryRun' - Set to true to validate the request parameters and access permissions without modifying the function code.
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'publish' - Set to true to publish a new version of the function after updating the code. This has the same effect as calling 'PublishVersion' separately.
-- * 'revisionId' - Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
-- * 's3Bucket' - An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
-- * 's3Key' - The Amazon S3 key of the deployment package.
-- * 's3ObjectVersion' - For versioned objects, the version of the deployment package object to use.
-- * 'zipFile' - The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
mkUpdateFunctionCode ::
  -- | 'functionName'
  Lude.Text ->
  UpdateFunctionCode
mkUpdateFunctionCode pFunctionName_ =
  UpdateFunctionCode'
    { s3ObjectVersion = Lude.Nothing,
      s3Key = Lude.Nothing,
      zipFile = Lude.Nothing,
      s3Bucket = Lude.Nothing,
      dryRun = Lude.Nothing,
      revisionId = Lude.Nothing,
      publish = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | For versioned objects, the version of the deployment package object to use.
--
-- /Note:/ Consider using 's3ObjectVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uS3ObjectVersion :: Lens.Lens' UpdateFunctionCode (Lude.Maybe Lude.Text)
uS3ObjectVersion = Lens.lens (s3ObjectVersion :: UpdateFunctionCode -> Lude.Maybe Lude.Text) (\s a -> s {s3ObjectVersion = a} :: UpdateFunctionCode)
{-# DEPRECATED uS3ObjectVersion "Use generic-lens or generic-optics with 's3ObjectVersion' instead." #-}

-- | The Amazon S3 key of the deployment package.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uS3Key :: Lens.Lens' UpdateFunctionCode (Lude.Maybe Lude.Text)
uS3Key = Lens.lens (s3Key :: UpdateFunctionCode -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: UpdateFunctionCode)
{-# DEPRECATED uS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uZipFile :: Lens.Lens' UpdateFunctionCode (Lude.Maybe (Lude.Sensitive Lude.Base64))
uZipFile = Lens.lens (zipFile :: UpdateFunctionCode -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {zipFile = a} :: UpdateFunctionCode)
{-# DEPRECATED uZipFile "Use generic-lens or generic-optics with 'zipFile' instead." #-}

-- | An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uS3Bucket :: Lens.Lens' UpdateFunctionCode (Lude.Maybe Lude.Text)
uS3Bucket = Lens.lens (s3Bucket :: UpdateFunctionCode -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: UpdateFunctionCode)
{-# DEPRECATED uS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | Set to true to validate the request parameters and access permissions without modifying the function code.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDryRun :: Lens.Lens' UpdateFunctionCode (Lude.Maybe Lude.Bool)
uDryRun = Lens.lens (dryRun :: UpdateFunctionCode -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: UpdateFunctionCode)
{-# DEPRECATED uDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRevisionId :: Lens.Lens' UpdateFunctionCode (Lude.Maybe Lude.Text)
uRevisionId = Lens.lens (revisionId :: UpdateFunctionCode -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: UpdateFunctionCode)
{-# DEPRECATED uRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | Set to true to publish a new version of the function after updating the code. This has the same effect as calling 'PublishVersion' separately.
--
-- /Note:/ Consider using 'publish' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPublish :: Lens.Lens' UpdateFunctionCode (Lude.Maybe Lude.Bool)
uPublish = Lens.lens (publish :: UpdateFunctionCode -> Lude.Maybe Lude.Bool) (\s a -> s {publish = a} :: UpdateFunctionCode)
{-# DEPRECATED uPublish "Use generic-lens or generic-optics with 'publish' instead." #-}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uFunctionName :: Lens.Lens' UpdateFunctionCode Lude.Text
uFunctionName = Lens.lens (functionName :: UpdateFunctionCode -> Lude.Text) (\s a -> s {functionName = a} :: UpdateFunctionCode)
{-# DEPRECATED uFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest UpdateFunctionCode where
  type Rs UpdateFunctionCode = FunctionConfiguration
  request = Req.putJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateFunctionCode where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateFunctionCode where
  toJSON UpdateFunctionCode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3ObjectVersion" Lude..=) Lude.<$> s3ObjectVersion,
            ("S3Key" Lude..=) Lude.<$> s3Key,
            ("ZipFile" Lude..=) Lude.<$> zipFile,
            ("S3Bucket" Lude..=) Lude.<$> s3Bucket,
            ("DryRun" Lude..=) Lude.<$> dryRun,
            ("RevisionId" Lude..=) Lude.<$> revisionId,
            ("Publish" Lude..=) Lude.<$> publish
          ]
      )

instance Lude.ToPath UpdateFunctionCode where
  toPath UpdateFunctionCode' {..} =
    Lude.mconcat
      ["/2015-03-31/functions/", Lude.toBS functionName, "/code"]

instance Lude.ToQuery UpdateFunctionCode where
  toQuery = Lude.const Lude.mempty
