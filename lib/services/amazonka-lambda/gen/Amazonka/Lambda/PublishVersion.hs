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
-- Module      : Amazonka.Lambda.PublishVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- Lambda doesn\'t publish a version if the function\'s configuration and
-- code haven\'t changed since the last version. Use UpdateFunctionCode or
-- UpdateFunctionConfiguration to update the function before publishing a
-- version.
--
-- Clients can invoke versions directly or with an alias. To create an
-- alias, use CreateAlias.
module Amazonka.Lambda.PublishVersion
  ( -- * Creating a Request
    PublishVersion (..),
    newPublishVersion,

    -- * Request Lenses
    publishVersion_codeSha256,
    publishVersion_description,
    publishVersion_revisionId,
    publishVersion_functionName,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPublishVersion' smart constructor.
data PublishVersion = PublishVersion'
  { -- | Only publish a version if the hash value matches the value that\'s
    -- specified. Use this option to avoid publishing a version if the function
    -- code has changed since you last updated it. You can get the hash for the
    -- version that you uploaded from the output of UpdateFunctionCode.
    codeSha256 :: Prelude.Maybe Prelude.Text,
    -- | A description for the version to override the description in the
    -- function configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | Only update the function if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid publishing a version if the function
    -- configuration has changed since you last updated it.
    revisionId :: Prelude.Maybe Prelude.Text,
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
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSha256', 'publishVersion_codeSha256' - Only publish a version if the hash value matches the value that\'s
-- specified. Use this option to avoid publishing a version if the function
-- code has changed since you last updated it. You can get the hash for the
-- version that you uploaded from the output of UpdateFunctionCode.
--
-- 'description', 'publishVersion_description' - A description for the version to override the description in the
-- function configuration.
--
-- 'revisionId', 'publishVersion_revisionId' - Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid publishing a version if the function
-- configuration has changed since you last updated it.
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
  Prelude.Text ->
  PublishVersion
newPublishVersion pFunctionName_ =
  PublishVersion'
    { codeSha256 = Prelude.Nothing,
      description = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Only publish a version if the hash value matches the value that\'s
-- specified. Use this option to avoid publishing a version if the function
-- code has changed since you last updated it. You can get the hash for the
-- version that you uploaded from the output of UpdateFunctionCode.
publishVersion_codeSha256 :: Lens.Lens' PublishVersion (Prelude.Maybe Prelude.Text)
publishVersion_codeSha256 = Lens.lens (\PublishVersion' {codeSha256} -> codeSha256) (\s@PublishVersion' {} a -> s {codeSha256 = a} :: PublishVersion)

-- | A description for the version to override the description in the
-- function configuration.
publishVersion_description :: Lens.Lens' PublishVersion (Prelude.Maybe Prelude.Text)
publishVersion_description = Lens.lens (\PublishVersion' {description} -> description) (\s@PublishVersion' {} a -> s {description = a} :: PublishVersion)

-- | Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid publishing a version if the function
-- configuration has changed since you last updated it.
publishVersion_revisionId :: Lens.Lens' PublishVersion (Prelude.Maybe Prelude.Text)
publishVersion_revisionId = Lens.lens (\PublishVersion' {revisionId} -> revisionId) (\s@PublishVersion' {} a -> s {revisionId = a} :: PublishVersion)

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
publishVersion_functionName :: Lens.Lens' PublishVersion Prelude.Text
publishVersion_functionName = Lens.lens (\PublishVersion' {functionName} -> functionName) (\s@PublishVersion' {} a -> s {functionName = a} :: PublishVersion)

instance Core.AWSRequest PublishVersion where
  type
    AWSResponse PublishVersion =
      FunctionConfiguration
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PublishVersion where
  hashWithSalt _salt PublishVersion' {..} =
    _salt
      `Prelude.hashWithSalt` codeSha256
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData PublishVersion where
  rnf PublishVersion' {..} =
    Prelude.rnf codeSha256
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders PublishVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PublishVersion where
  toJSON PublishVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CodeSha256" Data..=) Prelude.<$> codeSha256,
            ("Description" Data..=) Prelude.<$> description,
            ("RevisionId" Data..=) Prelude.<$> revisionId
          ]
      )

instance Data.ToPath PublishVersion where
  toPath PublishVersion' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/versions"
      ]

instance Data.ToQuery PublishVersion where
  toQuery = Prelude.const Prelude.mempty
