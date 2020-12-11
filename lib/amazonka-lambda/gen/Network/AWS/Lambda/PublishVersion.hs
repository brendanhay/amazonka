{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PublishVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html version> from the current code and configuration of a function. Use versions to create a snapshot of your function code and configuration that doesn't change.
--
-- AWS Lambda doesn't publish a version if the function's configuration and code haven't changed since the last version. Use 'UpdateFunctionCode' or 'UpdateFunctionConfiguration' to update the function before publishing a version.
-- Clients can invoke versions directly or with an alias. To create an alias, use 'CreateAlias' .
module Network.AWS.Lambda.PublishVersion
  ( -- * Creating a request
    PublishVersion (..),
    mkPublishVersion,

    -- ** Request lenses
    pvCodeSha256,
    pvDescription,
    pvRevisionId,
    pvFunctionName,

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

-- | /See:/ 'mkPublishVersion' smart constructor.
data PublishVersion = PublishVersion'
  { codeSha256 ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    revisionId :: Lude.Maybe Lude.Text,
    functionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublishVersion' with the minimum fields required to make a request.
--
-- * 'codeSha256' - Only publish a version if the hash value matches the value that's specified. Use this option to avoid publishing a version if the function code has changed since you last updated it. You can get the hash for the version that you uploaded from the output of 'UpdateFunctionCode' .
-- * 'description' - A description for the version to override the description in the function configuration.
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'revisionId' - Only update the function if the revision ID matches the ID that's specified. Use this option to avoid publishing a version if the function configuration has changed since you last updated it.
mkPublishVersion ::
  -- | 'functionName'
  Lude.Text ->
  PublishVersion
mkPublishVersion pFunctionName_ =
  PublishVersion'
    { codeSha256 = Lude.Nothing,
      description = Lude.Nothing,
      revisionId = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | Only publish a version if the hash value matches the value that's specified. Use this option to avoid publishing a version if the function code has changed since you last updated it. You can get the hash for the version that you uploaded from the output of 'UpdateFunctionCode' .
--
-- /Note:/ Consider using 'codeSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvCodeSha256 :: Lens.Lens' PublishVersion (Lude.Maybe Lude.Text)
pvCodeSha256 = Lens.lens (codeSha256 :: PublishVersion -> Lude.Maybe Lude.Text) (\s a -> s {codeSha256 = a} :: PublishVersion)
{-# DEPRECATED pvCodeSha256 "Use generic-lens or generic-optics with 'codeSha256' instead." #-}

-- | A description for the version to override the description in the function configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvDescription :: Lens.Lens' PublishVersion (Lude.Maybe Lude.Text)
pvDescription = Lens.lens (description :: PublishVersion -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PublishVersion)
{-# DEPRECATED pvDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid publishing a version if the function configuration has changed since you last updated it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvRevisionId :: Lens.Lens' PublishVersion (Lude.Maybe Lude.Text)
pvRevisionId = Lens.lens (revisionId :: PublishVersion -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: PublishVersion)
{-# DEPRECATED pvRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvFunctionName :: Lens.Lens' PublishVersion Lude.Text
pvFunctionName = Lens.lens (functionName :: PublishVersion -> Lude.Text) (\s a -> s {functionName = a} :: PublishVersion)
{-# DEPRECATED pvFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest PublishVersion where
  type Rs PublishVersion = FunctionConfiguration
  request = Req.postJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PublishVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PublishVersion where
  toJSON PublishVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CodeSha256" Lude..=) Lude.<$> codeSha256,
            ("Description" Lude..=) Lude.<$> description,
            ("RevisionId" Lude..=) Lude.<$> revisionId
          ]
      )

instance Lude.ToPath PublishVersion where
  toPath PublishVersion' {..} =
    Lude.mconcat
      ["/2015-03-31/functions/", Lude.toBS functionName, "/versions"]

instance Lude.ToQuery PublishVersion where
  toQuery = Lude.const Lude.mempty
