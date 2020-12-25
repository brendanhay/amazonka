{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    uFunctionName,
    uDryRun,
    uPublish,
    uRevisionId,
    uS3Bucket,
    uS3Key,
    uS3ObjectVersion,
    uZipFile,

    -- * Destructuring the response
    Types.FunctionConfiguration (..),
    Types.mkFunctionConfiguration,

    -- ** Response lenses
    Types.fcCodeSha256,
    Types.fcCodeSize,
    Types.fcDeadLetterConfig,
    Types.fcDescription,
    Types.fcEnvironment,
    Types.fcFileSystemConfigs,
    Types.fcFunctionArn,
    Types.fcFunctionName,
    Types.fcHandler,
    Types.fcKMSKeyArn,
    Types.fcLastModified,
    Types.fcLastUpdateStatus,
    Types.fcLastUpdateStatusReason,
    Types.fcLastUpdateStatusReasonCode,
    Types.fcLayers,
    Types.fcMasterArn,
    Types.fcMemorySize,
    Types.fcRevisionId,
    Types.fcRole,
    Types.fcRuntime,
    Types.fcSigningJobArn,
    Types.fcSigningProfileVersionArn,
    Types.fcState,
    Types.fcStateReason,
    Types.fcStateReasonCode,
    Types.fcTimeout,
    Types.fcTracingConfig,
    Types.fcVersion,
    Types.fcVpcConfig,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFunctionCode' smart constructor.
data UpdateFunctionCode = UpdateFunctionCode'
  { -- | The name of the Lambda function.
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
    functionName :: Types.FunctionName,
    -- | Set to true to validate the request parameters and access permissions without modifying the function code.
    dryRun :: Core.Maybe Core.Bool,
    -- | Set to true to publish a new version of the function after updating the code. This has the same effect as calling 'PublishVersion' separately.
    publish :: Core.Maybe Core.Bool,
    -- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
    revisionId :: Core.Maybe Types.String,
    -- | An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
    s3Bucket :: Core.Maybe Types.S3Bucket,
    -- | The Amazon S3 key of the deployment package.
    s3Key :: Core.Maybe Types.S3Key,
    -- | For versioned objects, the version of the deployment package object to use.
    s3ObjectVersion :: Core.Maybe Types.S3ObjectVersion,
    -- | The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.
    zipFile :: Core.Maybe (Core.Sensitive Core.Base64)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunctionCode' value with any optional fields omitted.
mkUpdateFunctionCode ::
  -- | 'functionName'
  Types.FunctionName ->
  UpdateFunctionCode
mkUpdateFunctionCode functionName =
  UpdateFunctionCode'
    { functionName,
      dryRun = Core.Nothing,
      publish = Core.Nothing,
      revisionId = Core.Nothing,
      s3Bucket = Core.Nothing,
      s3Key = Core.Nothing,
      s3ObjectVersion = Core.Nothing,
      zipFile = Core.Nothing
    }

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
uFunctionName :: Lens.Lens' UpdateFunctionCode Types.FunctionName
uFunctionName = Lens.field @"functionName"
{-# DEPRECATED uFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Set to true to validate the request parameters and access permissions without modifying the function code.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDryRun :: Lens.Lens' UpdateFunctionCode (Core.Maybe Core.Bool)
uDryRun = Lens.field @"dryRun"
{-# DEPRECATED uDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Set to true to publish a new version of the function after updating the code. This has the same effect as calling 'PublishVersion' separately.
--
-- /Note:/ Consider using 'publish' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPublish :: Lens.Lens' UpdateFunctionCode (Core.Maybe Core.Bool)
uPublish = Lens.field @"publish"
{-# DEPRECATED uPublish "Use generic-lens or generic-optics with 'publish' instead." #-}

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRevisionId :: Lens.Lens' UpdateFunctionCode (Core.Maybe Types.String)
uRevisionId = Lens.field @"revisionId"
{-# DEPRECATED uRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uS3Bucket :: Lens.Lens' UpdateFunctionCode (Core.Maybe Types.S3Bucket)
uS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED uS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The Amazon S3 key of the deployment package.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uS3Key :: Lens.Lens' UpdateFunctionCode (Core.Maybe Types.S3Key)
uS3Key = Lens.field @"s3Key"
{-# DEPRECATED uS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | For versioned objects, the version of the deployment package object to use.
--
-- /Note:/ Consider using 's3ObjectVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uS3ObjectVersion :: Lens.Lens' UpdateFunctionCode (Core.Maybe Types.S3ObjectVersion)
uS3ObjectVersion = Lens.field @"s3ObjectVersion"
{-# DEPRECATED uS3ObjectVersion "Use generic-lens or generic-optics with 's3ObjectVersion' instead." #-}

-- | The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uZipFile :: Lens.Lens' UpdateFunctionCode (Core.Maybe (Core.Sensitive Core.Base64))
uZipFile = Lens.field @"zipFile"
{-# DEPRECATED uZipFile "Use generic-lens or generic-optics with 'zipFile' instead." #-}

instance Core.FromJSON UpdateFunctionCode where
  toJSON UpdateFunctionCode {..} =
    Core.object
      ( Core.catMaybes
          [ ("DryRun" Core..=) Core.<$> dryRun,
            ("Publish" Core..=) Core.<$> publish,
            ("RevisionId" Core..=) Core.<$> revisionId,
            ("S3Bucket" Core..=) Core.<$> s3Bucket,
            ("S3Key" Core..=) Core.<$> s3Key,
            ("S3ObjectVersion" Core..=) Core.<$> s3ObjectVersion,
            ("ZipFile" Core..=) Core.<$> zipFile
          ]
      )

instance Core.AWSRequest UpdateFunctionCode where
  type Rs UpdateFunctionCode = Types.FunctionConfiguration
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2015-03-31/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/code")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
