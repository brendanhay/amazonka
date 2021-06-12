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
-- Module      : Network.AWS.Lambda.PublishVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a
-- <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html version>
-- from the current code and configuration of a function. Use versions to
-- create a snapshot of your function code and configuration that doesn\'t
-- change.
--
-- AWS Lambda doesn\'t publish a version if the function\'s configuration
-- and code haven\'t changed since the last version. Use UpdateFunctionCode
-- or UpdateFunctionConfiguration to update the function before publishing
-- a version.
--
-- Clients can invoke versions directly or with an alias. To create an
-- alias, use CreateAlias.
module Network.AWS.Lambda.PublishVersion
  ( -- * Creating a Request
    PublishVersion (..),
    newPublishVersion,

    -- * Request Lenses
    publishVersion_revisionId,
    publishVersion_codeSha256,
    publishVersion_description,
    publishVersion_functionName,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_vpcConfig,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_functionName,
    functionConfiguration_environment,
    functionConfiguration_version,
    functionConfiguration_functionArn,
    functionConfiguration_state,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_description,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPublishVersion' smart constructor.
data PublishVersion = PublishVersion'
  { -- | Only update the function if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid publishing a version if the function
    -- configuration has changed since you last updated it.
    revisionId :: Core.Maybe Core.Text,
    -- | Only publish a version if the hash value matches the value that\'s
    -- specified. Use this option to avoid publishing a version if the function
    -- code has changed since you last updated it. You can get the hash for the
    -- version that you uploaded from the output of UpdateFunctionCode.
    codeSha256 :: Core.Maybe Core.Text,
    -- | A description for the version to override the description in the
    -- function configuration.
    description :: Core.Maybe Core.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PublishVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'publishVersion_revisionId' - Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid publishing a version if the function
-- configuration has changed since you last updated it.
--
-- 'codeSha256', 'publishVersion_codeSha256' - Only publish a version if the hash value matches the value that\'s
-- specified. Use this option to avoid publishing a version if the function
-- code has changed since you last updated it. You can get the hash for the
-- version that you uploaded from the output of UpdateFunctionCode.
--
-- 'description', 'publishVersion_description' - A description for the version to override the description in the
-- function configuration.
--
-- 'functionName', 'publishVersion_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newPublishVersion ::
  -- | 'functionName'
  Core.Text ->
  PublishVersion
newPublishVersion pFunctionName_ =
  PublishVersion'
    { revisionId = Core.Nothing,
      codeSha256 = Core.Nothing,
      description = Core.Nothing,
      functionName = pFunctionName_
    }

-- | Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid publishing a version if the function
-- configuration has changed since you last updated it.
publishVersion_revisionId :: Lens.Lens' PublishVersion (Core.Maybe Core.Text)
publishVersion_revisionId = Lens.lens (\PublishVersion' {revisionId} -> revisionId) (\s@PublishVersion' {} a -> s {revisionId = a} :: PublishVersion)

-- | Only publish a version if the hash value matches the value that\'s
-- specified. Use this option to avoid publishing a version if the function
-- code has changed since you last updated it. You can get the hash for the
-- version that you uploaded from the output of UpdateFunctionCode.
publishVersion_codeSha256 :: Lens.Lens' PublishVersion (Core.Maybe Core.Text)
publishVersion_codeSha256 = Lens.lens (\PublishVersion' {codeSha256} -> codeSha256) (\s@PublishVersion' {} a -> s {codeSha256 = a} :: PublishVersion)

-- | A description for the version to override the description in the
-- function configuration.
publishVersion_description :: Lens.Lens' PublishVersion (Core.Maybe Core.Text)
publishVersion_description = Lens.lens (\PublishVersion' {description} -> description) (\s@PublishVersion' {} a -> s {description = a} :: PublishVersion)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
publishVersion_functionName :: Lens.Lens' PublishVersion Core.Text
publishVersion_functionName = Lens.lens (\PublishVersion' {functionName} -> functionName) (\s@PublishVersion' {} a -> s {functionName = a} :: PublishVersion)

instance Core.AWSRequest PublishVersion where
  type
    AWSResponse PublishVersion =
      FunctionConfiguration
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable PublishVersion

instance Core.NFData PublishVersion

instance Core.ToHeaders PublishVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON PublishVersion where
  toJSON PublishVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RevisionId" Core..=) Core.<$> revisionId,
            ("CodeSha256" Core..=) Core.<$> codeSha256,
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath PublishVersion where
  toPath PublishVersion' {..} =
    Core.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/versions"
      ]

instance Core.ToQuery PublishVersion where
  toQuery = Core.const Core.mempty
