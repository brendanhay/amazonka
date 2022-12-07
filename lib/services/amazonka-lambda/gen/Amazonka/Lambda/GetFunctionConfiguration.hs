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
-- Module      : Amazonka.Lambda.GetFunctionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the version-specific settings of a Lambda function or version.
-- The output includes only options that can vary between versions of a
-- function. To modify these settings, use UpdateFunctionConfiguration.
--
-- To get all of a function\'s details, including function-level settings,
-- use GetFunction.
module Amazonka.Lambda.GetFunctionConfiguration
  ( -- * Creating a Request
    GetFunctionConfiguration (..),
    newGetFunctionConfiguration,

    -- * Request Lenses
    getFunctionConfiguration_qualifier,
    getFunctionConfiguration_functionName,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFunctionConfiguration' smart constructor.
data GetFunctionConfiguration = GetFunctionConfiguration'
  { -- | Specify a version or alias to get details about a published version of
    -- the function.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'getFunctionConfiguration_qualifier' - Specify a version or alias to get details about a published version of
-- the function.
--
-- 'functionName', 'getFunctionConfiguration_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
newGetFunctionConfiguration ::
  -- | 'functionName'
  Prelude.Text ->
  GetFunctionConfiguration
newGetFunctionConfiguration pFunctionName_ =
  GetFunctionConfiguration'
    { qualifier =
        Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version or alias to get details about a published version of
-- the function.
getFunctionConfiguration_qualifier :: Lens.Lens' GetFunctionConfiguration (Prelude.Maybe Prelude.Text)
getFunctionConfiguration_qualifier = Lens.lens (\GetFunctionConfiguration' {qualifier} -> qualifier) (\s@GetFunctionConfiguration' {} a -> s {qualifier = a} :: GetFunctionConfiguration)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
getFunctionConfiguration_functionName :: Lens.Lens' GetFunctionConfiguration Prelude.Text
getFunctionConfiguration_functionName = Lens.lens (\GetFunctionConfiguration' {functionName} -> functionName) (\s@GetFunctionConfiguration' {} a -> s {functionName = a} :: GetFunctionConfiguration)

instance Core.AWSRequest GetFunctionConfiguration where
  type
    AWSResponse GetFunctionConfiguration =
      FunctionConfiguration
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetFunctionConfiguration where
  hashWithSalt _salt GetFunctionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData GetFunctionConfiguration where
  rnf GetFunctionConfiguration' {..} =
    Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders GetFunctionConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetFunctionConfiguration where
  toPath GetFunctionConfiguration' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/configuration"
      ]

instance Data.ToQuery GetFunctionConfiguration where
  toQuery GetFunctionConfiguration' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]
